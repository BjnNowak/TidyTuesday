library(terra)
library(sf)
library(tidyverse)
library(bertin) # Dl here: https://github.com/BjnNowak/bertin
library(camcorder)
library(showtext)
library(patchwork)
library(ggtext)

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
  width = 60, 
  height = 50, 
  units = "cm", 
  dpi = 300 
)

wrld<-read_sf('Data/GreenCountry/Map/world-administrative-boundaries.shp')

rst<-tibble(
  nm=c(
    # 2020
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_Jan_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_Feb_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_Mar_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_Apr_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_May_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_Jun_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_Jul_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_Aug_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_Sep_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_Oct_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_Nov_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Europe/median_NDVI_Europe_Dec_2020.tif'
  )
)

# Tibble with mont names
tt<-tibble(
  nm=c(
    "January","February","March","April",
    "May","June","July","August",
    "September","October","November","December"
  )
)%>%mutate(nm=glue::glue("{nm} 2020"))

# ndvi color palette
pal_ndvi <- rev(c(
  "#007f5f","#2b9348","#55a630","#80b918","#aacc00","#bfd200",
  "#d4d700","#dddf00","#eeef20","#ffff3f"
))

# Europe bounding box
xlims<-c(1691000,6500000)
ylims<-c(960000,5400000)

# Plot caption
cap <- tibble(
  lab="**Data** MODIS **| Plot** @BjnNowak"
)

# Make plots
for (i in 1:12){

  vec_agg<-make_points_rst(
    rst=rast(rst$nm[i]),
    n=150
  )%>%
  st_transform("EPSG:3035")

  map<-ggplot()+
    geom_sf(
      wrld,
      mapping=aes(geometry=geometry),
      fill=NA,color="white"
    )+
  
    annotate(
      geom="text",
      x = 1691000,
      y = 4500000, label ="Vegetation cover",
      color="white",fontface="bold",hjust=0,
      family = "ral", size = 50
    )+
    annotate(
      geom="text",
      x = 1691000,
      y = 4250000, label =tt$nm[i],
      color="white",hjust=0,
      family = "ral", size = 50
    )+
    geom_richtext(
      data=cap,
      aes(label=lab),
      x = 1691000, 
      y = 1200000,
      color="white",hjust=0,
      family = "fira", size = 30,
      fill = NA, label.color = NA, 
      label.padding = grid::unit(rep(0, 4), "pt") 
    )+
  geom_sf(
    vec_agg,
    mapping=aes(size=NDVI,alpha=NDVI,geometry=geometry),
    color=pal_ndvi[8]
  )+
  scale_size_binned(
    range=c(0,5),
    limits=c(0.3,1),
    breaks=seq(0.4,0.8,0.1)
  )+
  scale_alpha_binned(
    range=c(0.4,0.9),
    limits=c(0.2,1),
    breaks=seq(0.4,0.8,0.1)
  )+
  
  scale_x_continuous(limits=xlims)+
  scale_y_continuous(limits=ylims)+
  
  guides(
    size="none",
    alpha="none",
  )+
  
  coord_sf(crs="EPSG:3035")+
  
  theme_void()+
  theme(
    plot.margin = margin(2,2,2,2,"cm"),
    plot.background = element_rect(
      fill="#0D1F22",color="NA"),
  )

print(map)

}

# Make animation
gg_playback(
  name = file.path(tempdir(), "recording", "ndvi.gif"),
  first_image_duration = 1,
  last_image_duration = 1,
  frame_duration = 2,
  image_resize = 1200
)
