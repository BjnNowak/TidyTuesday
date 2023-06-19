library(scico)
library(sf)
library(tidyverse)
library(frex)
library(camcorder)
library(showtext)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Bitter","bit")
font_add_google("Raleway","ral")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Load hexagonal basemap
map<-get_map()%>%
  # Load crop distribution
  left_join(get_static_layer(layer="crops"))%>%
  # Load organic crop distribution
  left_join(get_static_layer(layer="organic"))

wh<-ggplot(map,aes(fill=soft_wheat_area_km2,color=soft_wheat_area_km2))+
  geom_sf()+
  geom_sf(
    map%>%st_centroid(),
    mapping=aes(size=organic_soft_wheat_area_km2),
    pch=21,fill=NA,alpha=0.5,color="white"
  )+
  scale_size(range=c(1,7))+
  scale_fill_scico(palette="batlow",na.value = "grey75",direction=1,begin=0,end=0.75)+
  scale_color_scico(palette="batlow",na.value = "grey75",direction=1,begin=0,end=0.75)+
  guides(fill="none",size="none",color="none")+
  labs(title = "Soft wheat")+
  theme_void()+
  theme(plot.title=element_text(size=60,family="bit",hjust=0.5,face="bold"))
wh
