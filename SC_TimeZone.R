library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)
library(glue)
library(solartime)
library(sf)

# Set fonts
font_add_google("Raleway","ral")
font_add_google("Pacifico","pac")
font_add_google("Jost","jo")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 40, 
  height = 30, 
  units = "cm", 
  dpi = 300 
)

# Load data
###########
transitions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/transitions.csv')
timezones <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezones.csv')
timezone_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezone_countries.csv')
countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/countries.csv')
# World map
world_ne <- sf::read_sf("Data/Erosion/data/world_map/ne_110m_admin_0_countries_lakes.shp")%>%
  sf::st_transform(crs="ESRI:54030")

# Data cleaning
###############
# Get last valid hour
tra <- transitions%>%
  group_by(zone)%>%
  mutate(da=lubridate::as_datetime(begin))%>%
  filter(da<"2023-04-01")%>%
  filter(begin==max(begin))

# Compute difference from solar time
# thanks to {solartime}
loc <- tra%>%
  left_join(timezones)%>%
  mutate(off_hour = offset/3600)%>%
  mutate(diff_solar = 60 * computeSolarToLocalTimeDifference(longitude, timeZone = off_hour, doy = 90))%>%
  select(zone,offset,latitude,longitude,diff_solar)%>%
  drop_na()
  
# Convert to sf object
loc_sf <- sf::st_as_sf(loc,coords=c("longitude","latitude"))
# Set crs then transform to same crs as world map
st_crs(loc_sf) = "EPSG:4326" 
loc_rob <- sf::st_transform(loc_sf,crs="ESRI:54030")

# Make plot
###########
# Create graticule
grat <- sf::st_graticule(lat = c(-89.9, seq(-90, 60, 20), 89.9))
# Set colors
col_back <- "#1D201F"
col_world <- "#073B4C"
pal <- c(
  "A"="#45CAFF", 
  "B"="#A2CFDD", 
  "C"="#D1D1CC", 
  "D"="#FFD3BA", 
  "E"="#FF7793", 
  "F"="#FF497F", 
  "G"="#FF1B6B"
)


ggplot()+
  geom_sf(
    world_ne,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("white",0.15))+
  geom_sf(
    loc_rob,
    mapping=aes(geometry=geometry,color=diff_solar),
    size=3
  )+
  geom_sf(
    grat,mapping=aes(geometry=geometry),
    color=alpha("white",0.5))+
  binned_scale(
    aesthetics = "color",
    scale_name = "stepsn", 
    palette = function(x) pal,
    breaks = seq(-50,50,20),
    labels = glue::glue("{seq(-50,50,20)} min"),
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "horizontal",
      barheight = unit(4, units = "mm") , 
      barwidth = unit(200, units = "mm"),
      title.position = 'top',
      label.position = "bottom"
    )
  )+
  coord_sf(crs= "+proj=vandg4")+
  labs(
    title = "Right on time ?",
    subtitle = "This map shows the **difference between local time and solar time** on Tuesday March 28, 2023",
    color="Deviation from solar time",
    caption="**Data** IANA tz database | **Plot** @BjnNowak")+
  theme_void()+
  theme(
    plot.margin = margin(1,0,1,0,"cm"),
    plot.title = element_text(
      family = "pac",color="white",
      size=90,hjust=0.5,face="bold",
      margin=margin(0.5,0,0.5,0,"cm")
    ),
    plot.subtitle = element_markdown(
      family = "jo",color="white",
      size=55,hjust=0.5
    ),
    plot.background = element_rect(fill=col_back,color=NA),
    legend.position = "bottom",
    legend.title = element_text(
      family = "ral",color="white",
      size=60,hjust=0.5,face="bold",
      margin = margin(0.5,0,-0.5,0,"cm"),
    ),
    legend.text = element_text(
      family = "jo",color="white",
      size=40,margin=margin(-0.5,0,0,0,"cm")),
    plot.caption= element_markdown(
      family = "jo",color="white",
      size=35,hjust=1,margin = margin(0.5,1,0,0,"cm"))
  ) 


