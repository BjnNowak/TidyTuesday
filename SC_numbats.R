library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(tm)
library(sf)
library(rgdal)
library(raster)
library(stars)
library(terra)
library(tidyterra)


# Set fonts
font_add_google("Open Sans","open")
font_add_google("Source Sans Pro","pro")
font_add_google("Fira Sans","fira sans")
font_add_google("Fira Sans Condensed","fira")
font_add_google("Outfit","out")
font_add_google("Staatliches","staat")

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

numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

sources <- numbats%>%
  mutate(ct=1)%>%
  group_by(dataResourceName)%>%
  summarize(sm=sum(ct))

# load map
world <- sf::read_sf("Data/Erosion/data/world_map/ne_110m_admin_0_countries_lakes_guy.shp")
ndvi <-
  raster::raster("Data/Erosion/data/world_map/ndvi_australia_2020_modis_1000m.tif")
summary(ndvi)

# Convert obs to sf object with right crs
sf_num <- st_as_sf(numbats%>%select(decimalLatitude,decimalLongitude)%>%drop_na(), coords = c("decimalLongitude","decimalLatitude"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_num_aus <- sf_num%>%st_transform(3112)

# projecting australia map to australian crs
australia<-world%>%filter(ADMIN=="Australia")%>%st_transform(3112)
# same for ndvi map
ndvi_aus <- projectRaster(ndvi,crs = crs(australia))
# convert to terra object
ndvi_terra <- terra::rast(ndvi_aus)

# classification for ndvi values
ndvi_terra_clean <- ndvi_terra%>%
  filter(NDVI>0)%>%
  mutate(nd=NDVI/10000)%>%
  mutate(pal=case_when(
    nd<0.1~"A",
    nd<0.2~"B",
    nd<0.3~"C",
    nd<0.4~"D",
    nd<0.5~"E",
    nd<0.6~"F",
    nd<0.7~"G",
    nd<0.8~"H",
    TRUE~"I"
  ))%>%drop_na()

# color palette
pal_nd<-c(
  "A"="#F0F9A7",
  "B"="#D6F5A9",
  "C"="#BCF1AB",
  "D"="#A2EDAD",
  "E"="#87E8AE",
  "F"="#6DE4B0",
  "G"="#53E0B2",
  "H"="#1ED7B5",
  "I"="#1BC3A5"
)


# remove un
#rm()


# create a graticule to overlay on map
grat = st_graticule(australia)
grat_aus <- st_transform(grat, 3112)%>%
  filter(degree!=-10)%>%
  filter(degree!=110)

# create an hexagonal grid for country that will be used to count number of obs
grid_aus<-st_make_grid(
  australia,
  n=c(20,20),
  square = FALSE
)
# convert to sf object
grid_sf <- st_sf(grid_aus)
# count number of observations per hexagon
grid_sf$ct<-lengths(st_intersects(grid_sf, sf_num_aus))
# extract centroid to label hexagons
grid_pts<-grid_sf%>%st_centroid()




###############################################"""

# make plot

tit_leg<-"<b>Normalized Difference Vegetation Index (NDVI)</b><br><span style='font-size:38px; font-weight:light; color:white;'>High  NDVI  values  indicate  well  developed  vegetation  cover</span>"

p1<-ggplot() +
  #geom_sf(australia,mapping=aes(geometry=geometry))+
  tidyterra::geom_spatraster(
    data = ndvi_terra_clean%>%drop_na() , aes(fill = pal),
    na.rm = TRUE
  )+
  #geom_sf(sf_num,mapping=aes(geometry=geometry),size=3,alpha=0.1)+
  geom_sf(
    grat_aus,
    mapping=aes(geometry=geometry),
    alpha=1,linewidth=0.05,color="white"
  )+
  geom_sf(
    data=st_intersection(grid_sf,australia),
    mapping=aes(geometry=grid_aus),
    fill=NA,color=alpha("dimgrey",0.1)
  )+
  geom_sf(
    grid_sf%>%filter(ct>0),
    mapping=aes(geometry=grid_aus),
    fill=NA,color="dimgrey"
  )+
  geom_sf_text(
    grid_pts%>%filter(ct>0),
    mapping=aes(geometry=grid_aus,alpha=ct,label=ct),
    family="fira",size=9,color="black"
  )+
  #geom_raster(data = ndvi_df%>%filter(NDVI>0) , aes(x = x, y = y, fill = pal))  
  
  scale_alpha(range = c(0.4, 1))+
  scale_fill_manual(
    values=pal_nd,na.translate = FALSE,
    label=c("<0.1",seq(0.1,0.7,0.1),">0.7"),
    guide = guide_legend(
      nrow=1,
      direction = "horizontal",
      keyheight = unit(5, units = "mm") , 
      keywidth = unit(12, units = "mm"),
      title.position = 'top',
      label.position = "bottom"
    )
  )+
  
  labs(
    fill=tit_leg,
    #title="Numbat repartition is not related to vegetation",
    subtitle="a. The **values in the hexagons** show<br>the **number of numbats observations** since 1856",
    #caption="**Data** Atlas of Living Australia and MODIS | **Plot** @BjnNowak"
  )+
  guides(alpha="none")+
  
  theme_void()+
  theme(
    plot.background = element_rect(fill="#295270",color=NA),
    plot.margin = margin(1,0.75,1,0.75,"cm"),
    legend.position="bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.title=element_markdown(
      size=36,family = "out",
      hjust=0.5,lineheight = 0.4,color="white",
      margin=margin(0,0,-0.25,0,"cm")
    ),
    legend.text=element_text(
      size=28,family = "out",hjust=0.5,
      color="white",
      lineheight = 0.5,margin=margin(-0.25,0,0,0,"cm")
    ),
    plot.title = element_text(
      size=80,family = "staat",hjust=0.5,
      color="white",face="bold",
      lineheight = 0.5,margin=margin(0.25,0,0,0,"cm")
    ),
    plot.subtitle = element_markdown(
      size=30,family = "fira sans",hjust=0,
      color="white",
      lineheight = 0.5,margin=margin(0,0,1,0,"cm")
    ),
    plot.caption=element_markdown(
      size=30,family = "pro",
      hjust=0.5,lineheight = 0.5,color="white",
      margin=margin(0.8,0,0,0,"cm")
    )
  )
  

#############################################

# reg zooms

buff_max<-grid_pts%>%filter(ct==max(ct)|ct==176|ct==30)%>%st_buffer(100000)

buff_max<-grid_sf%>%filter(ct==max(ct)|ct==176|ct==30)

p2<-ggplot() +
  #geom_sf(australia,mapping=aes(geometry=geometry))+
  tidyterra::geom_spatraster(
    data = ndvi_terra_clean%>%crop(buff_max,mask=TRUE)%>%drop_na() , aes(fill = pal),
    na.rm = TRUE
  )+
  #geom_sf(sf_num,mapping=aes(geometry=geometry),size=3,alpha=0.1)+
  geom_sf(
    sf_num_aus%>%st_crop(buff_max),
    mapping=aes(geometry=geometry),
    size=1,color="white"
  )+
  geom_sf_text(
    buff_max,
    mapping=aes(geometry=grid_aus,alpha=ct,label=ct),
    family="fira",size=9,color="black"
  )+
  geom_sf(
    buff_max,
    mapping=aes(geometry=grid_aus),
    fill=NA,color="dimgrey"
  )+
  #geom_raster(data = ndvi_df%>%filter(NDVI>0) , aes(x = x, y = y, fill = pal))  
  
  scale_alpha(range = c(0.4, 1))+
  scale_fill_manual(
    values=pal_nd, na.value = NA
  )+
  guides(fill="none",alpha="none")+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#295270",color=NA),
    plot.margin = margin(1,0.75,1,0.75,"cm"),
    legend.position="bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.title=element_markdown(
      size=40,family = "out",
      hjust=0.5,lineheight = 0.4,color="white",
      margin=margin(0,0,-0.25,0,"cm")
    ),
    legend.text=element_text(
      size=32,family = "out",hjust=0.5,
      color="white",
      lineheight = 0.5,margin=margin(-0.25,0,0,0,"cm")
    ),
    plot.title = element_text(
      size=80,family = "staat",hjust=0.5,
      color="white",face="bold",
      lineheight = 0.5,margin=margin(0.25,0,0,0,"cm")
    ),
    plot.subtitle = element_markdown(
      size=40,family = "fira sans",hjust=0.5,
      color="white",
      lineheight = 0.5,margin=margin(0.5,0,1,0,"cm")
    ),
    plot.caption=element_markdown(
      size=30,family = "pro",
      hjust=0.5,lineheight = 0.5,color="white",
      margin=margin(0.8,0,0,0,"cm")
    )
  )

p2 


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

p1 + p2 + 
  plot_layout(guides = 'collect')+
  plot_annotation(
    title="Numbat repartition is not related to vegetation",
    caption = "**Data** Atlas of Living Australia and MODIS | **Plot** @BjnNowak",
    theme = theme(
      plot.title = element_text(
        size=80,family = "staat",hjust=0.5,
        color="white",face="bold",
        lineheight = 0.5,margin=margin(0.25,0,0,0,"cm")
      ),
      plot.caption=element_markdown(
        size=30,family = "pro",
        hjust=0.5,lineheight = 0.5,color="white",
        margin=margin(0,0,0,0,"cm")
      )
    )
  )&
  theme(
    plot.background = element_rect(fill="#295270",color=NA),
    legend.position="bottom"
  )
