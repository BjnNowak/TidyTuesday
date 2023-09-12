library(tidyverse)
library(camcorder)
library(ggtext)
library(showtext)
library(maps)
library(sf)
library(cartogram)
library(ggforce)


# Fonts
font_add_google("Permanent Marker","marker")
font_add_google("Open Sans","open")
font_add_google("Bebas Neue","beb")

showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 32, 
  height = 16.6, 
  units = "cm", 
  dpi = 600 
)

# Load data&maps
################

# Load data
all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv')
# Load map
world_ne <- sf::read_sf("Data/Erosion/data/world_map/ne_110m_admin_0_countries_lakes.shp")

# Clean data
############

act <- all_countries%>%
  group_by(country_iso3,Category)%>%
  summarize(time=sum(hoursPerDayCombined))

act%>%group_by(Category)%>%summarize(mean(time))

pop <- all_countries%>%
  group_by(country_iso3)%>%
  summarize(pop=mean(population))

clean <- act%>%
  left_join(pop)%>%
  mutate(tot=pop*time)

clean_wide <- clean%>%
  mutate(cat = tolower(str_replace_all(Category, " ", "_")))%>%
  select(country_iso3,cat,tot)%>%
  pivot_wider(names_from=cat,values_from=tot)%>%
  mutate(sm=
           #somatic_maintenance+
           deliberate_neural_restructuring+experience_oriented+food_provision+
           maintenance_of_surroundings+nonfood_provision+organization+technosphere_modification
  )%>%
  mutate(others=
           maintenance_of_surroundings+nonfood_provision+technosphere_modification
        )



# Make cartograms
#################

# Join data and map
world<-world_ne%>%
  left_join(clean_wide,by=c("ISO_A3_EH"="country_iso3"))

world_proj <- sf::st_transform(world_ne, crs="ESRI:54030")
map_proj <- sf::st_transform(world, crs="ESRI:54030")%>%
  select(ISO_A3_EH,sm,others,experience_oriented,organization,food_provision,deliberate_neural_restructuring)%>%
  drop_na()


# Making Dorling cartogram based on total waking hours
dorl<-cartogram_dorling(map_proj, weight="sm", k = 5, m_weight = 1, itermax = 1000)

ggplot()+
  geom_sf(world_proj,mapping=aes(geometry=geometry))+
  geom_sf(dorl,mapping=aes(geometry=geometry))

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
    ratio_exp = experience_oriented/experience_oriented,
    ratio_org = organization/experience_oriented,
    ratio_foo = food_provision/experience_oriented,
    ratio_neu = deliberate_neural_restructuring/experience_oriented,
    ratio_oth = others/experience_oriented
  )%>%
  mutate(
    rad_exp=sqrt(rad*rad*ratio_exp),
    rad_org=sqrt(rad*rad*ratio_org),
    rad_foo=sqrt(rad*rad*ratio_foo),
    rad_neu=sqrt(rad*rad*ratio_neu),
    rad_oth=sqrt(rad*rad*ratio_oth)
    #rad_grass=sqrt(rad*rad*ratio_grass)
  )

# Function to draw circles
# Inspired by post on StackOverflow to define function to draw (half) circle:
# https://stackoverflow.com/questions/28185743/draw-a-half-circle-with-ggplot2

circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2)
{
  tt <- seq(start*pi, end*pi, length.out=npoints)
  df<-data.frame(
    x = center[1] + diameter / 2 * cos(tt), 
    y = center[2] + diameter / 2 * sin(tt)
  )
  df2<-bind_rows(
    df,
    tibble(x = center[1],y=center[2]),
  )

  return(df2)
}


# Apply function to all countries for arable land and grassland
dord<-d3%>%
  arrange(ISO_A3_EH)

t1 <- tibble(
  iso = rep(dord$ISO_A3_EH[1],101),
  X = rep(dord$X[1],101),
  Y = rep(dord$Y[1],101)
)


c_exp <- c(0.4,0.8)
c_org <- c(0,0.4)
c_foo <- c(1.6,2)
c_neu <- c(1.2,1.6)
c_oth <- c(0.8,1.2)

# Draw for exp
t2_exp <- circleFun(
  c(dord$X[1],dord$Y[1]),dord$rad_exp[1]*2, start=c_exp[1], end=c_exp[2]
)
# Draw for org
t2_org <- circleFun(
  c(dord$X[1],dord$Y[1]),dord$rad_org[1]*2, start=c_org[1], end=c_org[2]
)
# Draw for foo
t2_foo <- circleFun(
  c(dord$X[1],dord$Y[1]),dord$rad_foo[1]*2, start=c_foo[1], end=c_foo[2]
)
# Draw for neu
t2_neu <- circleFun(
  c(dord$X[1],dord$Y[1]),dord$rad_neu[1]*2, start=c_neu[1], end=c_neu[2]
)
# Draw for oth
t2_oth <- circleFun(
  c(dord$X[1],dord$Y[1]),dord$rad_oth[1]*2, start=c_oth[1], end=c_oth[2]
)

t_exp<-bind_cols(t1,t2_exp)
t_org<-bind_cols(t1,t2_org)
t_foo<-bind_cols(t1,t2_foo)
t_neu<-bind_cols(t1,t2_neu)
t_oth<-bind_cols(t1,t2_oth)


for (i in 2:dim(dord)[1]){
  t1 <- tibble(
    Area = rep(dord$ISO_A3_EH[i],101),
    X = rep(dord$X[i],101),
    Y = rep(dord$Y[i],101)
  )
  
  # Draw for exp
  t2_exp <- circleFun(
    c(dord$X[i],dord$Y[i]),dord$rad_exp[i]*2, start=c_exp[1], end=c_exp[2]
  )
  # Draw for org
  t2_org <- circleFun(
    c(dord$X[i],dord$Y[i]),dord$rad_org[i]*2, start=c_org[1], end=c_org[2]
  )
  # Draw for foo
  t2_foo <- circleFun(
    c(dord$X[i],dord$Y[i]),dord$rad_foo[i]*2, start=c_foo[1], end=c_foo[2]
  )
  # Draw for neu
  t2_neu <- circleFun(
    c(dord$X[i],dord$Y[i]),dord$rad_neu[i]*2, start=c_neu[1], end=c_neu[2]
  )
  # Draw for oth
  t2_oth <- circleFun(
    c(dord$X[i],dord$Y[i]),dord$rad_oth[i]*2, start=c_oth[1], end=c_oth[2]
  )
  
  temp_exp<-bind_cols(t1,t2_exp)
  temp_org<-bind_cols(t1,t2_org)
  temp_foo<-bind_cols(t1,t2_foo)
  temp_neu<-bind_cols(t1,t2_neu)
  temp_oth<-bind_cols(t1,t2_oth)
  
  t_exp<-t_exp%>%
    bind_rows(temp_exp)
  
  t_org<-t_org%>%
    bind_rows(temp_org)
  
  t_foo<-t_foo%>%
    bind_rows(temp_foo)
  
  t_neu<-t_neu%>%
    bind_rows(temp_neu)
  
  t_oth<-t_oth%>%
    bind_rows(temp_oth)
}

# Make plot

# Create a graticule
grat_short <- sf::st_graticule(lat = c(-89.9, seq(-90, 60, 20), 89.9))

col_world <- "#073B4C"
col_borders <- "grey80"
col_back <- "#1D201F"

col_exp <- "#70D6FF"
col_foo <- "#06D6A0"
col_oth <- "#A78D8B"  #"#FB5607"
col_org <- "#FF70A6" 
col_neu <- "#FCE762"


ggplot()+
  geom_sf(
    world_proj,mapping=aes(geometry=geometry),
    linewidth=0.05,
    #fill="#9CB4BF",
    fill="grey25",
    color="dimgrey"
  )+

  geom_circle(
    data = d3,
    aes(x0 = X, y0 = Y, r = rad),
    color=alpha("white",0.25),
    fill="#6C809A",alpha=0.5,
    linewidth=0.05
  )+
  
  geom_polygon(
    t_exp,
    mapping=aes(x,y,group=Area),
    fill=col_exp,color=NA
  )+ 
  geom_polygon(
    t_org,
    mapping=aes(x,y,group=Area),
    fill=col_org,color=NA
  )+
  geom_polygon(
    t_foo,
    mapping=aes(x,y,group=Area),
    fill=col_foo,color=NA
  )+
  geom_polygon(
    t_neu,
    mapping=aes(x,y,group=Area),
    fill=col_neu,color=NA
  )+
  geom_polygon(
    t_oth,
    mapping=aes(x,y,group=Area),
    fill=col_oth,color=NA
  )+
  
  geom_text(
    data = d3,
    aes(x = X, y = Y, label=ISO_A3_EH, size=sm),
    color="black",
    alpha=0.25, family="beb"
  )+
  
  geom_sf(
    grat_short,
    mapping=aes(geometry=geometry),
    alpha=0.15,
    color=col_borders)+
  
  scale_size(range=c(4,28))+
  
  theme_void()+
  theme(plot.background = element_rect(fill="#1D201F",color=NA))
