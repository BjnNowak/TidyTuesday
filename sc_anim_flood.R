# Clear space 
rm(list=ls())
gc()

library(tidyverse)
library(camcorder)
library(ggtext)
library(showtext)
library(maps)
library(sf)
library(ggiraph)
library(cowplot)
library(magick)
library(raster)
library(rasterVis)
library(rgdal)
library(grid)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()

font_add_google("Permanent Marker","marker")
font_add_google("Open Sans","open")
showtext_auto()

#com <- readOGR(dsn='Data', layer="Com63")

com<-read_sf(
  dsn = "Data", 
  layer = "Com63"
)%>%
  filter(INSEE_DEP==63)

com_diss<-com%>%
  group_by(INSEE_DEP)%>%
  summarise()%>%
  ungroup()

water<-read_sf(
  dsn = "Data", 
  layer = "COURS_D_EAU"
)

# Intersection
water_clip <- st_intersection(water, com_diss)

# Filter
water_sel <- water_clip%>%
  filter(TOPONYME=="l'Allier")

# Buffer
water_buffer <- st_buffer(water_sel, 5000)

# Intersection (for buffer)
water_buff_clip <- st_intersection(water_buffer, com_diss)

# Difference
com_diff<-st_difference(com_diss,water_buff_clip)

# Get extent
x_min <- extent(com)[1]
x_max <- extent(com)[2]
y_min <- extent(com)[3]
y_max <- extent(com)[4]

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

# Set colors
col_fill <- '#e9d8a6'
col_col <- '#001219'
col_water <- "#94d2bd"

# Plots
p1<-ggplot()+
  geom_sf(
    com,mapping=aes(geometry=geometry),
    fill=col_fill,color=col_col,size=0.4
  )+
  labs(
    title="1. Basemap",
    subtitle='Puy-de-Dôme department (France)'
  )+
  scale_x_continuous(limits=c(x_min,x_max))+
  scale_y_continuous(limits=c(y_min,y_max))+
  coord_sf() +
  theme_map() +
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(hjust = 0.5,size=40,family='marker',lineheight=0.45,color=col_col),
    plot.subtitle = element_markdown(hjust = 0.5,size=30,family='open',lineheight=0.45,color=col_col)
  ) 
p1

p2<-ggplot()+
  geom_sf(
    com_diss,mapping=aes(geometry=geometry),
    fill=col_fill,color=col_col,size=0.4
  )+
  labs(
    title="2. Dissolve",
    subtitle="Merge all municipalities"
  )+
  coord_sf() +
  theme_map() +
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(hjust = 0.5,size=40,family='marker',lineheight=0.45,color=col_col),
    plot.subtitle = element_markdown(hjust = 0.5,size=30,family='open',lineheight=0.45,color=col_col)
  ) 

p2

p3<-ggplot()+
  geom_sf(
    com_diss,mapping=aes(geometry=geometry),
    fill=col_fill,color=col_col,size=0.4
  )+
  geom_sf(
    water,mapping=aes(geometry=geometry),
    color='#0a9396',size=0.4
  )+
  labs(
    title="3. Add layer",
    subtitle="French rivers layer"
  )+
  scale_x_continuous(limits=c(x_min,x_max))+
  scale_y_continuous(limits=c(y_min,y_max))+
  coord_sf() +
  theme_map() +
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(hjust = 0.5,size=40,family='marker',lineheight=0.45,color=col_col),
    plot.subtitle = element_markdown(hjust = 0.5,size=30,family='open',lineheight=0.45,color=col_col)
  ) 
p3

p4<-ggplot()+
  geom_sf(
    com_diss,mapping=aes(geometry=geometry),
    fill=col_fill,color=col_col,size=0.4
  )+
  geom_sf(
    water_clip,mapping=aes(geometry=geometry),
    color='#0a9396',size=0.3
  )+
  labs(
    title="4. Intersection",
    subtitle="Crop rivers by department boundaries"
  )+
  scale_x_continuous(limits=c(x_min,x_max))+
  scale_y_continuous(limits=c(y_min,y_max))+
  coord_sf() +
  theme_map() +
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(hjust = 0.5,size=40,family='marker',lineheight=0.45,color=col_col),
    plot.subtitle = element_markdown(hjust = 0.5,size=30,family='open',lineheight=0.45,color=col_col)
  ) 
p4

p5<-ggplot()+
  geom_sf(
    com_diss,mapping=aes(geometry=geometry),
    fill=col_fill,color=col_col,size=0.4
  )+
  geom_sf(
    water_sel,mapping=aes(geometry=geometry),
    color='#0a9396',size=0.4
  )+
  labs(
    title="5. Filter",
    subtitle="Select main river"
  )+
  scale_x_continuous(limits=c(x_min,x_max))+
  scale_y_continuous(limits=c(y_min,y_max))+
  coord_sf() +
  theme_map() +
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(hjust = 0.5,size=40,family='marker',lineheight=0.45,color=col_col),
    plot.subtitle = element_markdown(hjust = 0.5,size=30,family='open',lineheight=0.45,color=col_col)
  ) 
p5

p6<-ggplot()+
  geom_sf(
    com_diss,mapping=aes(geometry=geometry),
    fill=col_fill,color=col_col,size=0.4
  )+
  geom_sf(
    water_buffer,mapping=aes(geometry=geometry),
    fill='#94d2bd',color='#005f73',size=0.4
  )+
  geom_sf(
    water_sel,mapping=aes(geometry=geometry),
    color='#0a9396',size=0.4
  )+
  labs(
    title="6. Buffer",
    subtitle="Fixed distance buffer around the river"
  )+
  scale_x_continuous(limits=c(x_min,x_max))+
  scale_y_continuous(limits=c(y_min,y_max))+
  coord_sf() +
  theme_map() +
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(hjust = 0.5,size=40,family='marker',lineheight=0.45,color=col_col),
    plot.subtitle = element_markdown(hjust = 0.5,size=30,family='open',lineheight=0.45,color=col_col)
  ) 
p6

p7<-ggplot()+
  geom_sf(
    com_diss,mapping=aes(geometry=geometry),
    fill=col_fill,color=col_col,size=0.4
  )+
  geom_sf(
    water_buff_clip,mapping=aes(geometry=geometry),
    fill='#94d2bd',color='#005f73',size=0.4
  )+
  labs(
    title="7. Intersection",
    subtitle="Crop buffer by department boundaries"
  )+
  scale_x_continuous(limits=c(x_min,x_max))+
  scale_y_continuous(limits=c(y_min,y_max))+
  coord_sf() +
  theme_map() +
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(hjust = 0.5,size=40,family='marker',lineheight=0.45,color=col_col),
    plot.subtitle = element_markdown(hjust = 0.5,size=30,family='open',lineheight=0.45,color=col_col)
  ) 
p7

p8a<-ggplot()+
  #geom_sf(
  #  water_buff_clip,mapping=aes(geometry=geometry),
  #  color='#3DA5D9',size=0.4
  #)+
  geom_sf(
    com_diff,mapping=aes(geometry=geometry),
    fill=col_fill,color=col_col,size=0.4
  )+

  labs(
    title="8. Difference",
    subtitle="No risk of flooding"
  )+
  scale_x_continuous(limits=c(x_min,x_max))+
  scale_y_continuous(limits=c(y_min,y_max))+
  coord_sf() +
  theme_map() +
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(hjust = 0.5,size=40,family='marker',lineheight=0.45,color=col_col),
    plot.subtitle = element_markdown(hjust = 0.5,size=30,family='open',lineheight=0.45,color=col_col)
  ) 
p8a

p8b<-ggplot()+
  geom_sf(
    water_buff_clip,mapping=aes(geometry=geometry),
    fill='#94d2bd',color='#005f73',size=0.4
  )+
  #geom_sf(
  #  com_diff,mapping=aes(geometry=geometry),
  #  fill=col_fill,color=col_col,size=0.4
  #)+
  
  labs(
    title="8. Difference",
    subtitle="Risk of flooding"
  )+
  scale_x_continuous(limits=c(x_min,x_max))+
  scale_y_continuous(limits=c(y_min,y_max))+
  coord_sf() +
  theme_map() +
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(hjust = 0.5,size=40,family='marker',lineheight=0.45,color=col_col),
    plot.subtitle = element_markdown(hjust = 0.5,size=30,family='open',lineheight=0.45,color=col_col)
  ) 
p8b


# Animation
#p1
#p2
#p3
#p4
#p5
#p6
#p7
#p8a
#p8b


imgs <- list.files(file.path(tempdir(),"recording"), full.names = TRUE)
img_list <- lapply(imgs, image_read)
## join the images together
img_joined <- image_join(img_list)
## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 0.5)
img_animated
