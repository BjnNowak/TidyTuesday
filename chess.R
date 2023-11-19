library(rnaturalearth)
library(sf)
library(tidyverse)
library(camcorder)
library(scico)
library(patchwork)
library(showtext)

# Set fonts
font_add_google("Abril Fatface","abril")
font_add_google("Concert One","concert")

showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)


france <- ne_states(country = "Portugal", returnclass = "sf") %>% 
  #filter(!name %in% c("Guyane française", "Martinique", "Guadeloupe", "La Réunion", "Mayotte"))%>%
  #filter(!name %in% c("Alaska", "Hawaii"))%>%
  #filter(!name %in% c("Madeira", "Azores"))%>%
  #st_transform(crs=2154)%>% # France
  #st_transform(crs=5243)%>% # Germany
  #st_transform(crs=9147)%>% # Chili
  st_transform(crs=5070)%>% # USA
  mutate(ct=1)
  group_by(iso_a2)%>%
  summarize(sm=sum(ct))

ggplot(france)+
  geom_sf()+
  geom_sf_label(aes(label=name))

# for balanced 10*10 (France)
blk <- c(
  seq(1,9,2),seq(12,20,2),seq(21,29,2),
  seq(32,40,2),seq(41,49,2),seq(52,60,2),
  seq(61,69,2),seq(72,80,2),seq(81,89,2),
  seq(92,100,2)
)

# for 8*10 (Germany, Italy, Chile)
blk <- c(
  seq(1,7,2),seq(10,16,2),seq(17,23,2),
  seq(26,32,2),seq(33,39,2),seq(42,48,2),
  seq(49,55,2),seq(58,64,2),seq(65,71,2),
  seq(74,80,2)
)

# for 10*6 (USA)
blk <- c(
  seq(1,9,2),seq(12,20,2),seq(21,29,2),
  seq(32,40,2),seq(41,49,2),seq(52,60,2),
  seq(61,69,2),seq(72,80,2),seq(81,89,2),
  seq(92,100,2)
)

# For 8*10 (Germany)
country_grid<-st_make_grid(
    france,
    #square=FALSE,
    n=c(10,6)
  )%>%
  st_as_sf()%>%
  mutate(id=row_number())%>%
  mutate(col=case_when(
    id%in%blk~'black',
    TRUE~'white'
  ))

  
  
ggplot(country_grid)+
  geom_sf(aes(fill=col))+
  geom_sf_text(aes(label=id),color="grey50")+
  scale_fill_manual(values=c('black','white'))

inter <- st_intersection(france,country_grid)

ggplot(inter,aes(fill=col))+
  geom_sf()+
  scale_fill_manual(values=c('black','white'))+
  guides(fill='none')+
  theme_void()
