library(sf)
library(tidyverse)
library(rnaturalearth)
library(showtext)
library(camcorder)

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
  width = 40, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# To read:
# https://popups.uliege.be/2030-6318/index.php?id=2924&file=1&pid=2918
# https://france3-regions.francetvinfo.fr/grand-est/faut-il-se-mefier-coccinelles-asiatiques-1652776.html

# map
world <- ne_countries(scale = 110, type = "countries", returnclass = "sf")%>%
  # Convert WGS84 to projected crs (here Robinson)
  sf::st_transform(crs="ESRI:54030")

# website for data : www.gbif.org
sept <- read_delim('Data/MapChallenge2023/coccinella_septempunctata.csv',delim=';')
bi <- read_delim('Data/MapChallenge2023/adalia_bipunctata.csv',delim=';')
# Coccinelle asiatique: harmonia axyridis
harm <- read_delim('Data/MapChallenge2023/harmonia.csv',delim=';')

map <- harm%>%
  drop_na(decimalLongitude)%>%
  drop_na(decimalLatitude)%>%
  st_as_sf(coords=c('decimalLongitude','decimalLatitude'))

st_crs(map)=4326

map<-map%>%
  # Convert WGS84 to projected crs (here Robinson)
  sf::st_transform(world_ne, crs="ESRI:54030")
 
#ggplot()+
#  geom_sf(world,mapping=aes(geometry=geometry))+
#  geom_sf(map,mapping=aes(geometry=geometry))

grid_world<-st_make_grid(
  world,
  n=c(20,20),
  square = FALSE
)%>%
  st_sf()

ggplot(grid_world)+
  geom_sf()+
  geom_sf(world,mapping=aes(geometry=geometry))

# count number of observations per hexagon
grid_world$ct<-lengths(st_intersects(grid_world, map))

# extract centroid to label hexagons
grid_pts<-grid_world%>%
  filter(ct>0)%>%
  st_centroid()%>%
  mutate(ct_cl=case_when(
    ct<100~1,
    ct<510~2,
    ct<2000~3,
    ct<5000~4,
    TRUE~5
  ))

# Create graticule
grat <-  sf::st_graticule(lat = c(-89.9, seq(-90, 60, 30), 89.9))

# background
# vectors of latitudes and longitudes that go once around the 
# globe in 1-degree steps
lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

# turn into correctly projected sf collection
wintri_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>%
  lwgeom::st_transform_proj(crs = "ESRI:54030") # transform to Robinson



col_cox <- "#c23504"
col_back <- "#1a1419"


summary(grid_pts$ct)



ggplot()+
  #geom_sf(
  #  wintri_outline,mapping=aes(geometry=geometry),
  #  fill="white",color=alpha("white",0.5)
  #)+
  geom_sf(
    world%>%filter(name_en!="Antarctica"),mapping=aes(geometry=geometry),
    fill=col_cox,color=alpha("white",0.5)
  )+
  geom_sf(
    grid_pts,
    mapping=aes(geometry=geometry,size=ct_cl),
    color=col_back
  )+
  geom_sf(grat,mapping=aes(geometry=geometry),col=alpha("white",0.25))+
  # round crs
  coord_sf(crs="+proj=vitk1 +lat_1=65 +lat_2=55")+
  #coord_sf(crs="+proj=aeqd +lon_0=30 +lat_0=90")+
  #coord_sf(crs='+proj=lagrng')+
  scale_size(range=c(3,7))+
  guides(size='none')+
  theme_void()+
  theme(plot.background = element_rect(fill=NA,color=NA))
