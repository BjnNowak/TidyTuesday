library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(sf)
library(units)
library(lubridate)
library(ggbeeswarm)

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
# Data about locust:
# https://locust-hub-hqfao.hub.arcgis.com/
# Info :
# https://www.france24.com/fr/20200125-des-essaims-de-criquets-d-une-ampleur-historique-ravagent-de-larges-zones-d-afrique-de-l-est

data <- read_sf('Data/Locust/map/Swarm_Master_with_small_grid_id.shp')
grd <- read_sf('Data/Locust/map/grid_swarm_small.shp')
world_ne <- sf::read_sf("Data/Erosion/data/world_map/ne_110m_admin_0_countries_lakes.shp")

clean_whole<-data%>%
  mutate(
    yr=lubridate::year(data$STARTDATE),
    ct=1
  )%>%
  group_by(id)%>%
  summarize(
    sm=sum(ct)
  )


whole<-grd%>%
  left_join(st_drop_geometry(clean_whole))

# Make graticule
grat <- sf::st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9))

# Change crs
target_crs<-'+proj=eck4'

worldmap_trans <- st_transform(world_ne, crs = target_crs)
whole_trans <- st_transform(whole, crs = target_crs)
grat_trans <- st_transform(grat, crs = target_crs)

# bbox
disp_win_wgs84 <- st_sfc(
  st_point(c(-38, -60)), st_point(c(120, 70)),
                         crs = 4326)

disp_win_trans <- st_transform(disp_win_wgs84, crs = target_crs)
disp_win_coord <- st_coordinates(disp_win_trans)

# Make plots

pal<-c(
  "#FFEBF2",
  "#FFADCA",
  "#FF70A2",
  "#FF337A",
  "#F50056"
)

ggplot()+
  geom_sf(
    worldmap_trans,
    mapping=aes(geometry=geometry),
    fill="#073B4C",color=alpha("#EEE5E9",0.2))+
  geom_sf(
    whole_trans,
    mapping=aes(geometry=geometry,fill=sm),
    color=NA
  )+
  geom_sf(
    grat_trans,
    mapping=aes(geometry=geometry),
    alpha=0.1,color="white")+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal,
    breaks = c(50,150,250,350),
    na.value = NA,
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "horizontal",
      barheight = unit(3, units = "mm") , 
      barwidth = unit(30, units = "mm"),
      title.position = 'top',
      label.position = "bottom"
    ))+
  coord_sf(
    xlim = disp_win_coord[,'X'], 
    ylim = disp_win_coord[,'Y'],
    datum = target_crs, expand = FALSE
  )+
  theme_void()+
  theme(
    legend.position = c(0.18,0.44),
    legend.text = element_text(family="fira",color="white",size=20),
    legend.title=element_blank(),
    panel.background = element_rect(fill="#1D201F",color=NA)
  )

res <- st_drop_geometry(data)%>%
  mutate(
    ct=1,
    yr=lubridate::year(data$STARTDATE)
  )%>%
  group_by(yr)%>%
  summarize(sm=sum(ct))


ggplot(res,aes(x=yr,y=sm))+
  geom_glowline(
    color="#F50056",shadowcolour="#FF70A2",
    size=1.5)+
  theme_void()+
  theme(
    #panel.background = element_rect(fill="#1D201F",color=NA)
  )

