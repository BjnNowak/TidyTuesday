library(rnaturalearth)
library(sf)
library(tidyverse)
library(camcorder)
library(scico)
library(patchwork)
library(showtext)
library(ggtext)

# install Moma palette
# https://github.com/BlakeRMills/MoMAColors
#devtools::install_github("BlakeRMills/MoMAColors")
library(MoMAColors)

# Set fonts
font_add_google("Abril Fatface","abril")
font_add_google("Concert One","concert")
font_add_google("Raleway","ral")

showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

# 1. Load basemap
world_110 <- ne_countries(
  scale = 110,
  type = "countries",
  returnclass = "sf"
)

world_110<-world_110%>%filter(name!='Antarctica')%>%filter(name!='Seven seas (open ocean)')

# Graticule
grat <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9))

# Tibble with all proj
prj<-tibble(x=seq(-180,180,1))%>%
  mutate(proj=glue::glue("+proj=aeqd +lon_0={x} +lat_0=90"))

# Color palette
pal_col<-moma.colors("Klein", n=10, type="discrete")

# Function to create map
fun_proj<-function(proj){

pl<-ggplot()+
  geom_sf(
    world_110%>%filter(name!='Antarctica')%>%filter(name!='Seven seas (open ocean)'),
    mapping=aes(geometry=geometry, fill=continent),
    color=alpha("white",0.05)
  )+
  geom_sf(
    grat,mapping=aes(geometry=geometry),
    color=alpha('white',0.1)
  )+
  coord_sf(crs=proj)+
  guides(fill='none')+
  scale_fill_manual(values=pal_col)+
  labs(
    title = "North is not always up",
    subtitle = "The azimuthal equidistant projection is centred on the North Pole",
    caption="**Plot**  Benjamin Nowak"
  )+
  theme_void()+
  theme(
    plot.margin=margin(0.5,0,0.5,0,'cm'),
    plot.background = element_rect(fill="#0d1b2a",color=NA),
    #plot.background = element_rect(fill=pal[9],color=NA),
    plot.title =  element_markdown(size=30,color="white",hjust=0.5,family='abril'),
    plot.subtitle =  element_markdown(size=20,color="white",hjust=0.5,family='ral'),
    plot.caption = element_markdown(size=15,color="white",hjust=0.5,family='ral')
  )

return(pl)

}

# test on one slide
#fun_proj(prj$proj[prj$x==0])

# make animation
for (i in 1:dim(prj)[1]){
  print(fun_proj(prj$proj[dim(prj)[1]-i]))
}

# Save
gg_playback(
  name = file.path(tempdir(), "recording", "north.gif"),
  first_image_duration = 1,
  last_image_duration = 1,
  frame_duration = 0.05
  #image_resize = 1200
)
