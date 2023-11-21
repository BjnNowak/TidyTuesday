library(frex)
library(sf)
library(tidyverse)
library(showtext)
library(camcorder)
library(rnaturalearth)

# Set fonts
font_add_google("Bebas Neue","beb")
font_add_google("Barlow Condensed","bar")
font_add_google("Staatliches","play")
font_add_google("Bitter","bit")
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

france <- ne_states(country = "France", returnclass = "sf") %>% 
  filter(!name %in% c("Guyane française", "Martinique", "Guadeloupe", "La Réunion", "Mayotte"))%>%
  st_transform(crs=2154)%>% # France
  mutate(ct=1)%>%
  group_by(iso_a2)%>%
  summarize(sm=sum(ct))

ggplot(france)+
  geom_sf()

grd<-france%>%
  st_make_grid(n=c(1000,1000))%>%
  st_as_sf()%>%
  st_intersection(france)%>%
  mutate(sq_id=row_number())

grd$area_km2<-as.numeric(st_area(grd))/1000000

cent<-grd%>%
  #st_transform(crs=2154)%>%
  st_transform(crs=4326)%>%
  st_centroid()%>%
  dplyr::mutate(
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  )%>%
  arrange(lon,lat)%>%
  mutate(id=row_number())%>%
  st_drop_geometry()

grd_clean<-grd%>%
  left_join(cent)%>%
  arrange(id)%>%
  mutate(
    tot=sum(area_km2),
    sm=cumsum(area_km2),
    ratio=sm/tot
  )

tot_land <- 26.9
arable <- 16.9 
sth <- 9.3
permanent <- 0.7

wheat <- 4.891
corn <- 1.317+1.521
barley<-1.796
rapeseed <- 0.989
sunflower <- 0.666
temporary <- 3.6

arable-wheat-corn-barley-rapeseed-sunflower-temporary

pp<-7.9

ratio_wheat <- wheat/arable
ratio_maize <- corn/arable
ratio_barley <- barley/arable
ratio_rapeseed <- rapeseed/arable
ratio_sunflower <- sunflower/arable
ratio_temporary <- temporary/arable

ratio_pp <- pp/sth

ratio_arable <- arable/tot_land
ratio_sth <- sth/tot_land
ratio_permanent <- permanent/tot_land

grd_ratio <- grd_clean%>%
  mutate(main=case_when(
    ratio<ratio_arable~"arable",
    ratio<(ratio_arable+ratio_sth)~"sth",
    ratio<1.01~"permanent"
  ))

grd_ratio_plot<-grd_ratio%>%
  group_by(main)%>%
  summarize(sm=sum(sm))%>%
  ungroup()

ggplot(grd_ratio_plot,aes(fill=main))+
  geom_sf()+
  scale_fill_manual(values=c("blue","red","white"))+
  coord_sf(crs=2154)+
  theme_void()

cent_arable<-grd_ratio%>%
  filter(main=='arable')%>%
  #st_transform(crs=2154)%>%
  st_transform(crs=4326)%>%
  st_centroid()%>%
  dplyr::mutate(
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  )%>%
  arrange(lat,lon)%>%
  mutate(id=row_number())%>%
  st_drop_geometry()

grd_arable<-grd_ratio%>%
  select(sq_id,area_km2,main)%>%
  filter(main=='arable')%>%
  left_join(cent_arable)%>%
  arrange(id)%>%
  mutate(
    tot=sum(area_km2),
    sm=cumsum(area_km2),
    ratio=sm/tot
  )%>%
  mutate(second=case_when(
    ratio<ratio_wheat~"wheat",
    ratio<(ratio_wheat+ratio_maize)~"maize",
    ratio<(ratio_wheat+ratio_maize+ratio_barley)~"barley",
    ratio<(ratio_wheat+ratio_maize+ratio_barley+ratio_rapeseed)~"rapeseed",
    ratio<(ratio_wheat+ratio_maize+ratio_barley+ratio_rapeseed+ratio_sunflower)~"sunflower",
    ratio<(ratio_wheat+ratio_maize+ratio_barley+ratio_rapeseed+ratio_sunflower+ratio_temporary)~"temporary",
    ratio<1.01~"others"
  ))%>%
  group_by(second)%>%
  summarize(sum(sm))%>%
  ungroup()

cent_sth<-grd_ratio%>%
  filter(main=='sth')%>%
  #st_transform(crs=2154)%>%
  st_transform(crs=4326)%>%
  st_centroid()%>%
  dplyr::mutate(
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  )%>%
  arrange(lat,lon)%>%
  mutate(id=row_number())%>%
  st_drop_geometry()

grd_sth<-grd_ratio%>%
  select(sq_id,area_km2,main)%>%
  filter(main=='sth')%>%
  left_join(cent_sth)%>%
  arrange(id)%>%
  mutate(
    tot=sum(area_km2),
    sm=cumsum(area_km2),
    ratio=sm/tot
  )%>%
  mutate(second=case_when(
    ratio<ratio_pp~"pp",
    ratio<1.01~"others_grass"
  ))%>%
  group_by(second)%>%
  summarize(sum(sm))%>%
  ungroup()


alpha_pal<-c(
  "wheat"=0.05,
  "maize"=0.45,
  "barley"=0.05,
  "rapeseed"=0.45,
  "sunflower"=0.05,
  "temporary"=0.45,
  "others"=0.05,
  "pp"=0.05,
  "others_grass"=0.75
)

col_pal<-c(
  'arable'= "#F9C74F",
  'permanent'='#BD4089',
  "sth"='#90BE6D'
)

ggplot()+
  geom_sf(
    grd_ratio_plot,mapping=aes(fill=main,geometry=x),
    color=alpha("black",0),lwd=0.05)+
  geom_sf(
    grd_arable,mapping=aes(alpha=second,geometry=x),
    color=alpha("white",0.1)  
  )+
  geom_sf(
    grd_sth,mapping=aes(alpha=second,geometry=x),
    color=alpha("white",0.1)  
  )+
  scale_fill_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  coord_sf(crs=2154)+
  guides(fill='none',alpha='none')+
  theme_void()


col_pal<-c(
  'arable'= "blue",
  'permanent'='red',
  "sth"='white'
)

ggplot()+
  geom_sf(
    grd_ratio_plot,mapping=aes(geometry=x),
    color=alpha("black",1),lwd=0.5)+
  geom_sf(
    grd_ratio_plot,mapping=aes(fill=main,geometry=x),
    color=alpha("black",0),lwd=0.25)+
  geom_sf(
    grd_arable,mapping=aes(alpha=second,geometry=x),
    color=alpha("white",0)  
  )+
  geom_sf(
    grd_sth,mapping=aes(alpha=second,geometry=x),
    color=alpha("white",0), lwd=1  
  )+
  scale_fill_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  coord_sf(crs=2154)+
  guides(fill='none',alpha='none')+
  theme_void()

