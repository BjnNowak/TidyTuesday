library(tidyverse)
library(showtext)
library(camcorder)
library(stringi)
library(sf)
library(bertin)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Set font
font_add(family = "WeePeople", regular = "fonts/weepeople.ttf")
showtext_auto()

# Create the dot map
departments_valued<-make_points(
  polygon=france_departments,
  n=55,
  square=F
)%>%
  mutate(density_cl=case_when(
    density<40~40,
    density<80~80,
    density<120~120,
    density<160~160,
    TRUE~161
  ))

# Add random letter to each point
departments_valued_lbl<-departments_valued%>%
  bind_cols(lbl=stringi::stri_rand_strings(
    n=dim(departments_valued)[1],
    length=1, 
    pattern = "[A-Za-z]"
  ))

# Make map
ggplot(departments_valued_lbl,aes(size=as.factor(density_cl),label=lbl))+
  # French departments as background
  geom_sf(
    france_departments,
    mapping=aes(geometry=geometry),
    fill="grey95",color=alpha("grey60",0.5),
    inherit.aes=FALSE
  )+
  geom_sf_text(
    family="WeePeople",
    alpha=0.75
  )+
  scale_size_manual(
    values=seq(4,22,4),
    labels=c("<40","<80","<120","<160","≥160"))+
  guides(size='none')+
  coord_sf(crs='EPSG:2154')+
  theme_void()
