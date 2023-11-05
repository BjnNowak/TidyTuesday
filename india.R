# Load packages
library(rnaturalearth) 
library(sf)            
library(tidyverse)
library(camcorder)
library(showtext)

# Set fonts
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Fira Sans","fira")
font_add_google("Jost","jost")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 36, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Load basemap
world <- ne_countries(
  scale = 110,
  type = "countries",
  returnclass = "sf"
)



# India
india<-world%>%
  mutate(col=case_when(
    name=="India"~"India",
    TRUE~"Outdia"
  ))

grat <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9))

ggplot(india,aes(fill=col))+
  geom_sf(color=alpha('black',1),lwd=0.5)+
  geom_sf(color=alpha('black',0.25))+
  geom_sf(
    grat,
    mapping=aes(geometry=geometry),
    alpha=0.25,inherit.aes = FALSE
  )+
  coord_sf(crs="+proj=vandg4")+
  scale_fill_manual(values=c('#f6373c','#c5da41'))+
  labs(fill='')+
  guides(fill = guide_legend(byrow = TRUE))+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#2f9bdc",color=NA),
    legend.title = element_blank(),
    legend.text = element_text(family='ral',size=80,color="white",face="bold"),
    legend.position=c(0.15,0.20),
    legend.key.size = unit(3,"line"),
    legend.spacing.y = unit(0.5, 'cm')
  )
