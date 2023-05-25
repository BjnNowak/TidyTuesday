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
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

wrld<-read_sf('Data/GreenCountry/Map/world-administrative-boundaries.shp')

fr_uncomp<-terra::rast('Data/GreenCountry/Map/median_NDVI_France_2019.tif')
# Aggregated SpatRaster (to compute mean of long/lat)
fr <- aggregate(fr_uncomp, fact=10)

# Old palette
pal_ndvi <- c(
  '#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
  '#66A000', '#529400', '#3E8601', '#207401', '#056201', '#004C00', '#023B01',
  '#012E01', '#011D01', '#011301'
)

# new palette
pal_ndvi <- rev(c(
  "#007f5f","#2b9348","#55a630","#80b918","#aacc00","#bfd200",
  "#d4d700","#dddf00","#eeef20","#ffff3f"
))

map<-ggplot()+
  geom_sf(
    wrld%>%filter(name=="France"),
    mapping=aes(geometry=geometry),
    fill=pal_ndvi[1]
  )+
  tidyterra::geom_spatraster(
    data=fr_uncomp%>%filter(NDVI>0),aes(fill = NDVI),
    na.rm=TRUE,mask=TRUE,
    maxcell = 500e+05
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
    plot.background = element_rect(fill="#0D1F22",color="white"),
    legend.title = element_markdown(
      size=30,family="ral",hjust=0,
      color="white",lineheight=0.45,
      margin=margin(0,0,-0.2,0,"cm")
    ),
    legend.text = element_text(
      size=30,family="jost",color="white",
      hjust=0.5,margin=margin(0,0,0,-0.2,"cm"))
  )


map

# Get mean by line/latitude
cells <- cellFromRowCol(fr, 1, 1:ncol(fr))
lat_mean<-tibble(
  y=xyFromCell(fr, cells)[1,2],
  NDVI=mean(fr[cells]%>%drop_na()%>%pull(NDVI))
)

for(i in 2:nrow(fr)){
  cells <- cellFromRowCol(fr, i, 1:ncol(fr))
  temp<-tibble(
    y=xyFromCell(fr, cells)[1,2],
    NDVI=mean(fr[cells]%>%drop_na()%>%pull(NDVI))
  )
  lat_mean<-lat_mean%>%
    bind_rows(temp)
}

lat_mean<-lat_mean%>%drop_na()

yoff=lat_mean$y[1]-lat_mean$y[2]

lat_ndvi<-ggplot(lat_mean%>%filter(NDVI>0))+
  geom_rect(
    aes(ymin=y,ymax=y+yoff,xmin=-NDVI,xmax=0),
    fill=pal_ndvi[5],color=pal_ndvi[5]
  )+
  scale_x_continuous(limits=c(-1,0))+
  scale_y_continuous(limits=c(40.5,52))+
  theme_void()+
  theme(plot.background = element_rect(fill="#0D1F22",color=NA))


lat_ndvi



# Get mean by column/longitude
cells <- cellFromRowCol(fr, 1:nrow(fr), 1)
long_mean<-tibble(
  x=xyFromCell(fr, cells)[1,1],
  NDVI=mean(fr[cells]%>%drop_na()%>%pull(NDVI))
)

for(i in 2:ncol(fr)){
  cells <- cellFromRowCol(fr, 1:nrow(fr), i)
  temp<-tibble(
    x=xyFromCell(fr, cells)[1,1],
    NDVI=mean(fr[cells]%>%drop_na()%>%pull(NDVI))
  )
  long_mean<-long_mean%>%
    bind_rows(temp)
}

xoff=long_mean$x[2]-long_mean$x[1]


long_ndvi<-ggplot(long_mean%>%filter(NDVI>0),aes(x=x,y=-NDVI))+
  geom_rect(
    aes(ymin=-NDVI,ymax=0,xmin=x,xmax=x+xoff),
    fill=pal_ndvi[5],color=pal_ndvi[5])+
  scale_y_continuous(limits=c(-1.2,0))+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#0D1F22",color=NA)
  )

long_ndvi



layout <- c(
  area(t = 1, l = 1, b = 7, r = 2),
  area(t = 8, l = 3, b = 9, r = 10),
  area(t = 1, l = 3, b = 7, r = 10)
)

lat_ndvi+long_ndvi+map+ 
  plot_layout(design = layout)&
  theme(plot.margin = unit(c(.2,.4,.2,.2), "cm")) &
  plot_annotation(
    theme = theme(plot.background = element_rect(color  = NA, , fill ="#0D1F22")))
