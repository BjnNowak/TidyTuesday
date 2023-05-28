library(tidyverse)
library(terra)
library(sf)
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
  width = 10, 
  height = 15, 
  units = "cm", 
  dpi = 300 
)

# Old palette
pal_ndvi <- c(
  '#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
  '#66A000', '#529400', '#3E8601', '#207401', '#056201', '#004C00', '#023B01',
  '#012E01', '#011D01', '#011301'
)

# Load NDVI time series
dt<-read_delim('https://raw.githubusercontent.com/BjnNowak/TidyTuesday/main/data/ndvi_france_2019_to_2021.csv',delim=";")

# Load world map -> only file not available online
wrld<-read_sf('Data/GreenCountry/Map/world-administrative-boundaries.shp')

# Links to seasonnal rasters
rst<-tibble(
  nm=c(
    # 2019
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Ja_to_Ma_2019.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Av_to_Ju_2019.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Ju_to_Se_2019.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Oc_to_De_2019.tif',
    # 2020
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Ja_to_Ma_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Av_to_Ju_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Ju_to_Se_2020.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Oc_to_De_2020.tif',
    # 2021
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Ja_to_Ma_2021.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Av_to_Ju_2021.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Ju_to_Se_2021.tif',
    'https://github.com/BjnNowak/TidyTuesday/raw/main/map/ndvi/Ireland/median_NDVI_Ireland_Oc_to_De_2021.tif'
  )
)

# Clean data
clean<-dt%>%
  group_by(Country,Year)%>%
  mutate(day=row_number())%>%
  mutate(Country=case_when(
    Country=="UK"~"United Kingdom",
    TRUE~Country)
  )%>%
  filter(Country=="Ireland")%>%
  ungroup()

# Format tibble for animation
tib<-tibble(
  i = seq(1,12,1),
  year = c(rep(2019,4),rep(2020,4),rep(2021,4)),
  season = c(
    "Winter 2019","Spring 2019","Summer 2019","Fall 2019",
    "Winter 2020","Spring 2020","Summer 2020","Fall 2020",
    "Winter 2021","Spring 2021","Summer 2021","Fall 2021"
  ),
  start_grey = rep(1,12),
  end_grey = rep(c(0,90,181,273),3),
  start_col=rep(c(1,91,182,274),3),
  end_col=rep(c(90,181,273,365),3)
)

# to add ticks to plot
tck<-tibble(
  y=seq(0.6,0.1,0.1)
)


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 30, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Loop
for (i in 1:12){

# Time series plot
pl<-ggplot()+
  annotate(
    "text",x=20,
    y=.99,label="Vegetation\nin Ireland",
    family="ral",size=16,hjust=0,
    color="white",fontface="bold",
    lineheight=0.45
  )+
  annotate(
    "text",x=20,
    y=.95,label=tib$season[i],
    family="ral",size=13,hjust=0,
    color="forestgreen",fontface="bold"
  )+
  annotate(
    "segment",x=-20,xend=-20,y=.4,yend=.8,lwd=0.25,
    color="white"
  )+
  annotate(
    "text",x=-79,y=.8,label="Normalized Difference Vegetation Index (NDVI)",
    family="fira",size=12,angle=90,
    color="grey85"
  )+
  geom_segment(
    tck,
    mapping=aes(y=y,yend=y),
    x=-20,xend=-15,
    lwd=0.25,color="white"
  )+
  geom_text(
    tck,
    mapping=aes(y=y,label=y),
    x=-35,
    family="fira",size=10,
    angle=90,color="grey85"
  )+
  
  geom_line(
    clean%>%filter(Year<tib$year[i]),
    mapping=aes(x=day,y=NDVI,group=Year),
    color="white",
    lwd=0.1,alpha=0.5
  )+
  geom_line(
    clean%>%filter(Year==tib$year[i])%>%filter(day<=tib$end_grey[i]),
    mapping=aes(x=day,y=NDVI),lwd=0.25,color="white"
  )+
  geom_line(
    clean%>%filter(Year==tib$year[i])%>%filter(day>=tib$start_col[i]&day<=tib$end_col[i]),
    mapping=aes(x=day,y=NDVI),color="forestgreen",lwd=1.5
  )+
  
  scale_y_continuous(limits=c(0.4,0.8))+
  scale_x_continuous(limits=c(-80,365))+
  theme_void()

# Map
map<-ggplot()+
  geom_sf(
    wrld%>%filter(name=="Ireland"),
    mapping=aes(geometry=geometry),
    fill=pal_ndvi[1]
  )+
  tidyterra::geom_spatraster(
    data=terra::rast(rst$nm[i])%>%filter(NDVI>0),aes(fill = NDVI),
    na.rm=TRUE,mask=TRUE,
    maxcell = 50e+05
  )+
  scale_fill_gradientn(
    colors=pal_ndvi,
    na.value = NA,
    limits=c(0,1),
    breaks=seq(0.1,0.9,0.2)
  )+
  guides(fill=guide_colourbar(
    title.position = "top",
    label.position = "right",
    barwidth = unit(0.5,"cm"),
    barheight = unit(4,"cm"),
    nbin = 300,
    raster = TRUE
  ))+
  labs(fill="**Vegetation Index**<br>NDVI")+
  theme_void()+
  theme(
    #plot.margin = margin(1,2,1,2,"cm"),
    plot.background = element_rect(
      fill="#0D1F22",color="NA"),
    legend.title = element_markdown(
      size=30,family="ral",hjust=0,
      color="white",lineheight=0.45,
      margin=margin(0,0,-0.2,0,"cm")
    ),
    legend.text = element_text(
      size=30,family="jost",color="white",
      hjust=0.5,margin=margin(0,0,0,-0.2,"cm"))
  )

# Composition
layout <- c(
  area(t = 1, l = 5, b = 10, r = 15),
  area(t = 2, l = 1, b = 9, r = 4.5)
)

comp<-map+pl+
  plot_layout(design = layout)&
  theme(plot.margin = unit(c(.2,.6,.2,.6), "cm")) &
  plot_annotation(
    theme = theme(plot.background = element_rect(color  = NA, fill ="#0D1F22")))

print(comp)

}

# Make animation
gg_playback(
  name = file.path(tempdir(), "recording", "ndvi.gif"),
  first_image_duration = 1,
  last_image_duration = 1,
  frame_duration = 2,
  image_resize = 1200
)


