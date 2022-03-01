
library(tidyverse)
library(maps)
library(raster)
library(sf)
library(sp)
library(rgdal)
library(cartogram)
library(showtext)
library(ggtext)
library(camcorder)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

# Color palette
pal_ukrain <- c(
  '#FFD700',
  '#C0B72E',
  '#40778A',
  '#0057B7'
)

# Fonts
font_add_google("Permanent Marker","marker")
font_add_google("Open Sans","open")
showtext_auto()

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

cars <- readr::read_delim('https://raw.githubusercontent.com/BjnNowak/TidyTuesday/main/data/car_us.csv',delim=';')%>%
  dplyr::select(NAME_1=state,total)


# Keep only electric stations
elec <- stations %>%
  filter(FUEL_TYPE_CODE=='ELEC')

# Convert tibble to shapefile
elec_sp <- st_as_sf(elec, coords = c("LONGITUDE", "LATITUDE"), crs = CRS("+init=epsg:4326"))
spdf <- as_Spatial(elec_sp)

# Get shapefile of US states
x <- getData('GADM', country='USA', level=1)
crs(x)

# Count number of electric stations per state
cou <- over(spdf, x)

sta<-cou%>%
  mutate(ct=1)%>%
  group_by(NAME_1)%>%
  summarize(
    Stations = sum(ct)
  )%>%
  left_join(cars)%>%
  mutate(
    total=as.numeric(total),
    ratio=(Stations*10000)/total
  )

mx <- max(sta$ratio)
mn <- min(sta$ratio)

x_ext <- merge(x,sta,by='NAME_1')
crs(x_ext)
# Convert to projeted CRS (UTM, Zone 10)
x_proj <- spTransform(x_ext, CRS("+init=epsg:32610"))

# Remove non contiguous states
x_proj<-x_proj[x_proj$NAME_1!='Alaska',]
x_proj<-x_proj[x_proj$NAME_1!='Hawaii',]

# Make cartogram
us_cartogram <- cartogram_cont(x_proj, "ratio", itermax=8)

xmin<-extent(us_cartogram)[1]
ymin<-extent(us_cartogram)[3]
ymax<-extent(us_cartogram)[4]

off_sub <- (ymax-ymin)/12

# Convert shp to sf 
carto_sf<-st_as_sf(us_cartogram)

# Make plot
ggplot() +
  geom_sf(data = carto_sf, aes(fill = ratio) , size=0.3,color='white') +
  scale_fill_stepsn(
    colours=rev(pal_ukrain),
    breaks=seq(4,12,by=4),
    limits=c(0,16)
    #label=c('4','8','12')
  )+
  labs(
    fill="Number of electric stations<br>per 100,000 cars",
    caption="**Data:** US Dot | **Plot:** @BjnNowak"
  )+
  annotate(
    geom='text',
    x=xmin,y=ymax,
    label='Electric stations in the USA',
    hjust=0,family='marker',size=13,color='#1C1C1C'
  )+
  annotate(
    geom='text',
    x=xmin,y=ymax-off_sub,
    label=
'Size of each state is proportional to the number\n
of stations, weighted by the total number\n
of cars (combustion engine or electric).\n
Vermont leads the way, with 15 stations\n
par 100,000 cars.',
    hjust=0,family='open',size=8,
    lineheight=0.2,vjust=1,color='#1C1C1C'
  )+
  guides(fill = guide_colorbar(
    barheight = unit(4, units = "mm"),
    barwidth = unit(50, units = "mm"),
    direction = "horizontal",
    ticks.colour = NA,
    ticks.linewidth = 2,
    title.position = "top",
    label.position = "bottom",
    title.hjust = 0.5),
    color='none')+
  coord_sf() +
  theme_void()+
  theme(
    plot.background=element_rect(fill='#FCFCFC',color=NA),
    plot.margin =  margin(0.3,0.2,0.3,0.2, unit = "cm"),
    legend.position='bottom',
    plot.caption=element_markdown(size=20,family='open',color='grey25'),
    legend.title=element_markdown(size=30,family='open',color='#1C1C1C',lineheight=0.45),
    legend.text=element_text(size=25,family='open',color='#1C1C1C'),
    legend.spacing.x = unit(0, "cm"),
    legend.spacing.y = unit(0.25, "cm")
  )



