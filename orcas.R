library(tidyverse)
library(rgeoboundaries)
library(rmapshaper)
library(sf)
library(terra)
library(tidyterra)
library(gdistance)
library(camcorder)
library(showtext)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 16, 
  height = 9, 
  units = "cm", 
  dpi = 300 
)

# Load orcas data
orcas <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-15/orcas.csv')

# Create sf object with start/end position
clean<-orcas%>%
  dplyr::select(begin_longitude,begin_latitude,end_longitude,end_latitude)%>%
  drop_na()%>%
  mutate(id=row_number())
  
start<-clean%>%
  mutate(pos="start")%>%
  dplyr::select(id,pos,begin_longitude,begin_latitude)%>%
  sf::st_as_sf(coords = c("begin_longitude","begin_latitude"),crs='EPSG:4326')%>%
  dplyr::select(id,pos) 

end<-clean%>%
  mutate(pos="end")%>%
  dplyr::select(id,pos,end_longitude,end_latitude)%>%
  sf::st_as_sf(coords = c("end_longitude","end_latitude"),crs='EPSG:4326')%>%
  dplyr::select(id,pos)

orcas_sf<-start%>%
  bind_rows(end)

# Extract bounding box
bb<-st_bbox(orcas_sf)

#############################################################################

# Extract country borders data
us_ca_map <- rgeoboundaries::gb_adm0(country = c("USA", "Canada")) %>% 
  rmapshaper::ms_simplify(0.5)

# Crop countries by orcas bb
sea<-crop(vect(us_ca_map),bb)
# Convert to sf
sea_sf<-sea%>%st_as_sf()

# Convert vector to raster and set highly
# diffferent pixel values based on sea/not sea
r <- rast(sea_sf, ncols=500, nrows=500)
rr <-rasterize(sea_sf, r, "shapeISO")%>%
  mutate(shapeISO=case_when(
    shapeISO%in%c('CAN','USA')~1,
    TRUE~1000
  ))

ggplot()+
  geom_spatraster(data=rr)

# Compute transition matrix from raster pixels
r_trans <- gdistance::transition(
  raster(rr), 
  transitionFunction=mean, 
  directions = 16
)
r_trans <- geoCorrection(r_trans)

# Shortest path between start and end for the first line:
distance <- gdistance::shortestPath(
  r_trans, 
  c(clean%>%filter(id==1)%>%pull(begin_longitude),clean%>%filter(id==1)%>%pull(begin_latitude)), 
  c(clean%>%filter(id==1)%>%pull(end_longitude),clean%>%filter(id==1)%>%pull(end_latitude)), 
  output = "SpatialLines"
)%>%
  st_as_sf()

# Same for the other lines
for (i in 2:dim(clean)[1]) {
  
  temp <- shortestPath(
    r_trans, 
    c(clean%>%filter(id==i)%>%pull(begin_longitude),clean%>%filter(id==i)%>%pull(begin_latitude)), 
    c(clean%>%filter(id==i)%>%pull(end_longitude),clean%>%filter(id==i)%>%pull(end_latitude)), 
    output = "SpatialLines"
  )%>%
    st_as_sf()
  
  distance<-distance%>%
    bind_rows(temp)
  
  print(i)

}

# test if path is only sea
tst <- lengths(st_intersects(distance, us_ca_map)) > 0
# compute distance
dist <- st_length(distance)


distance_clean<-distance%>%
  bind_cols(id=start%>%pull(id))%>%
  bind_cols(inter=tst)%>%
  bind_cols(dist=as.numeric(dist))%>%
  filter(tst==F)

id_clean<-distance_clean%>%
  pull(id)

land <- '#00171F'
sea <- '#007EA7'
orc <- '#FCBF49'

ggplot()+
  geom_sf(
    data=us_ca_map,
    mapping=aes(geometry=geometry),
    fill=land, color=NA
  )+
  geom_sf(
    distance_clean,
    mapping=aes(alpha=dist,geometry=geometry),
    color=orc
  )+
  annotate(
    "text",
    x=bb$xmin, y=bb$ymin+0.07,
    label="Orcas movement in the Salish Sea",
    vjust=0, hjust=0, size=12,
    color=orc, family="ral", fontface='bold'
  )+
  annotate(
    "text",
    x=bb$xmin, y=bb$ymin,
    label="Data Center for Whale Research | Plot Benjamin Nowak",
    vjust=0, hjust=0, size=6,
    color="white", family="fira"
  )+
  coord_sf(
    xlim = c(bb$xmin, bb$xmax + 0.3), ylim = c(bb$ymin, bb$ymax - 0.8)
  )+
  scale_alpha(range=c(0.25,0.8))+
  guides(alpha='none')+
  theme_void()+
  theme(
    plot.background = element_rect(fill = sea, color=NA)
  )
