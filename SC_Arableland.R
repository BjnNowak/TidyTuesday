library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(ggforce)
library(glue)
library(cartogram)
library(sf)
library(units)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 32, 
  height = 16.6, 
  units = "cm", 
  dpi = 300 
)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load data
###########
# Data about agricultural surfaces (from FAOStats)
sau <- read_delim('Data/nb_farm/data/sau_world.csv',delim=',')%>% # Surface in 1,000 ha
  filter(Item%in%c("Agricultural land","Arable land","Land under perm. meadows and pastures"))%>%
  mutate(Item=case_when(
    Item=="Agricultural land"~"Total",
    Item=="Arable land"~"Arable",
    Item=="Land under perm. meadows and pastures"~"Grass"
  ))%>%
  filter(Year==2020)%>%
  select(M49="Area Code (M49)",Area,Year,Item,Value)%>%
  pivot_wider(names_from=Item,values_from = Value)%>%
  mutate(diff=Arable-Grass)

# Matrix to add iso code for country names to FAOStats data
mat<-read_delim('Data/Trade/mat_change.csv',delim=";")%>%
  dplyr::rename(
    # New name = Old name
    global_code = 'Global Code',
    global_name = 'Global Name',
    region_code = 'Region Code',
    region_name = 'Region Name',
    subregion_code = 'Sub-region Code',
    subregion_name = 'Sub-region Name',
    inter_region_code = 'Intermediate Region Code',
    inter_region_name = 'Intermediate Region Name',
    area = "Country or Area",
    M49_code = "M49 Code",
    iso_alpha2_code = "ISO-alpha2 Code",
    iso_alpha3_code = "ISO-alpha3 Code"
  )%>%
  mutate(M49 = M49_code)

# World map (data: Natural Earth)
world_ne <- sf::read_sf("Data/Erosion/data/world_map/ne_110m_admin_0_countries_lakes.shp")


# Merge map and data
####################
sau$M49<-as.numeric(sau$M49)
sau$M49[sau$Area=="China"]<-156

clean_sau<-sau%>%
  filter(Area!="China")%>%
  left_join(mat)

# Changing Namibia iso code (to avoid confusion with NA)
clean_sau$iso_alpha2_code[clean_sau$Area=="Namibia"]<-"NAM"
world_ne$ISO_A2_EH[world_ne$ADMIN=="Namibia"]<-"NAM"

# Change projection to Robinson
world_map <- sf::st_transform(world_ne, crs="ESRI:54030")

# Merge data to map
map_ne <- world_map%>%
  left_join(clean_sau,by=c("ISO_A2_EH"="iso_alpha2_code"))%>%
  select(Area,Total,Arable,Grass,geometry)%>%
  drop_na()

# Make cartograms
#################
# Making Dorling cartogram based on total cropland
dorl<-cartogram_dorling(map_ne, weight="Total", k = 5, m_weight = 1, itermax = 1000)

# Compute area and radius for each circus
d2<-dorl%>%
  mutate(
    ar=st_area(dorl),
    rad=sqrt(ar/pi)
  )

# Extract centroids for each circle
centr <- dorl%>%
  st_centroid()%>%
  st_coordinates()

# Merge area and centroids for total agri land
# and compute radius for crops or grass
d3 <- tibble(d2,X=centr[,1],Y=centr[,2])%>%
  mutate(rad=as.numeric(rad))%>%
  mutate(
    ratio_ara = Arable/Total,
    ratio_grass = Grass/Total
  )%>%
  mutate(Type=case_when(
    ratio_ara>ratio_grass~"Crop",
    TRUE~"Grass"
  ))%>%
  mutate(
    rad_crop=sqrt(rad*rad*ratio_ara),
    rad_grass=sqrt(rad*rad*ratio_grass)
  )



# Post on StackOverflow to define function to draw (half) circle:
# https://stackoverflow.com/questions/28185743/draw-a-half-circle-with-ggplot2

circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2)
{
  tt <- seq(start*pi, end*pi, length.out=npoints)
  data.frame(x = center[1] + diameter / 2 * cos(tt), 
             y = center[2] + diameter / 2 * sin(tt))
}

# Apply function to all countries for arable land and grassland
dord<-d3%>%
  arrange(Area)

t1 <- tibble(
  Area = rep(dord$Area[1],100),
  X = rep(dord$X[1],100),
  Y = rep(dord$Y[1],100)
)

# Draw for crops
t2 <- circleFun(
  c(dord$X[1],dord$Y[1]),dord$rad_crop[1]*2, start=1.5, end=2.5
)
# Draw for grass
t2b <- circleFun(
  c(dord$X[1],dord$Y[1]),dord$rad_grass[1]*2, start=0.5, end=1.5
)

tCrops<-bind_cols(t1,t2)
tGrass<-bind_cols(t1,t2b)

for (i in 2:dim(dord)[1]){
  t1 <- tibble(
    Area = rep(dord$Area[i],100),
    X = rep(dord$X[i],100),
    Y = rep(dord$Y[i],100)
  )
  
  t2 <- circleFun(
    c(dord$X[i],dord$Y[i]),dord$rad_crop[i]*2, start=1.5, end=2.5
  )
  t2b<- circleFun(
    c(dord$X[i],dord$Y[i]),dord$rad_grass[i]*2, start=0.5, end=1.5
  )
  
  temp<-bind_cols(t1,t2)
  tempb<-bind_cols(t1,t2b)
  
  tCrops<-tCrops%>%
    bind_rows(temp)
  
  tGrass<-tGrass%>%
    bind_rows(tempb)
}

# Make plot
###########

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 40, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Create a graticule
grat_short <- sf::st_graticule(lat = c(-89.9, seq(-90, 60, 20), 89.9))


col_world <- "#073B4C"
col_arable <- "#6C809A"
col_borders <- "#CFBCDF"
col_back <- "#1D201F"

 

ggplot()+
  geom_sf(
    world_map,mapping=aes(geometry=geometry),
    linewidth=0.05,fill="#9CB4BF",color="dimgrey"
  )+
  geom_sf(
    grat_short,
    mapping=aes(geometry=geometry),
    alpha=0.05,
    color=col_borders)+
  geom_circle(
    data = d3,
    aes(x0 = X, y0 = Y, r = rad),
    fill="dimgrey",alpha=0.75,
    color="white",
    linewidth=0.05
  )+
  geom_polygon(
    tCrops,
    mapping=aes(x,y,group=Area),
    fill="#f2e901",color=NA
  )+ 
  geom_polygon(
    tGrass,
    mapping=aes(x,y,group=Area),
    fill="#51c26f",color=NA
  )+
  theme_void()+
  theme(plot.background = element_rect(fill=col_back,color=NA))

