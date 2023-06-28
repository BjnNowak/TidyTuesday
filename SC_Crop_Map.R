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

# Load data
data_crop <- readr::read_csv('https://raw.githubusercontent.com/BjnNowak/CropMap/main/Data/all_crops_area.csv')

# Get first crop per country
clean_crop <- data_crop%>%
  filter(Year==2018)%>%
  filter(Element=='Area harvested')%>%
  filter(Unit=='ha')%>%
  group_by(Area)%>%
  slice_max(Value)%>%
  select(Area,Item,Value)%>%
  mutate(Crop=case_when(
    Item %in% c('Maize','Soybeans','Wheat','Millet','Barley','Rice, paddy')~Item,
    TRUE~'Other'
  ))
  
sau<-sau%>%
  left_join(clean_crop)


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
  select(Area,Arable,Crop,Value,geometry)%>%
  drop_na()


# Make cartograms
#################
# Making Dorling cartogram based on total cropland
dorl<-cartogram_dorling(map_ne, weight="Arable", k = 5, m_weight = 1, itermax = 1000)

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
    ratio_crop = Value/(Arable*1000)
  )%>%
  mutate(
    ratio_crop = case_when(
      Value/(Arable*1000)>1~1,
      TRUE~ratio_crop
    )
  )%>%
  mutate(
    rad_crop=sqrt(rad*rad*ratio_crop)
  )

# Set color palette
pal <- c(
  'Maize' = '#4CB944',
  'Wheat' = '#FF7D00',
  #'Millet'= '#FE9D3F',
  'Millet'= '#D6E681',
  
  'Barley' = '#F6C6B7' ,
  
  'Soybeans' = '#40CEFE',
  'Rice, paddy' = '#FE4091',
  'Other' = 'grey90'
  #'Other' = '#F6C6B7'
  #'#F9DAD0'
)

col_back <- "#1D201F"

col_back <- "#080F0F"
col_borders <- "#CFBCDF"

# Set color palette
pal <- c(
  'Maize' = '#00BBF9',
  'Wheat' = '#FFD166',
  'Millet'= '#2B59C3',
  'Barley' = '#91785D' ,
  'Soybeans' = '#06D6A0',
  'Rice, paddy' = '#EF476F',
  'Other' = '#E8D7F1'

)

# Create a graticule
grat_short <- sf::st_graticule(lat = c(-89.9, seq(-90, 60, 20), 89.9))

ggplot() +
  geom_sf(
    world_map,mapping=aes(geometry=geometry),
    linewidth=0.05,
    #fill="#9CB4BF",
    fill="#1D3535",
    color="dimgrey"
  )+
  geom_sf(
    grat_short,
    mapping=aes(geometry=geometry),
    alpha=0.05,
    color=col_borders)+
  geom_circle(
    d3,mapping=aes(x0=X,y0=Y,r=rad),
    color=alpha("white",0.25),
    fill="#6C809A",alpha=0.5,
    linewidth=0.5)+
  geom_circle(d3,mapping=aes(x0=X,y0=Y,r=rad_crop,fill=Crop),color=alpha("black",0.0))+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()+
  theme(plot.background = element_rect(fill=col_back,color=NA))
