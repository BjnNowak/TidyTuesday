
# load libraries 
library(tidyverse)
library(sf)
library(tmap)

# Import windows font
font_import()
loadfonts(device="win")

# Department limits
dep <- read_sf('Map/Com63_Diss.shp')
#com <- read_sf('Map/Com63.shp')
# Common Agricultural Policy field register
rpg <- read_sf('Map/RPG_63_2018.shp')

# Crop selection
multi <- rpg%>%
  mutate(crop=case_when(
    CODE_CULTU=='PPH'~'Permanent grassland',
    (CODE_CULTU=='PTR')|(CODE_CULTU=='RGA')~'Temporary grassland',
    CODE_CULTU=='BTH'~'Winter wheat',
    CODE_CULTU=='MIE'~'Silage maize',
    CODE_CULTU=='MIS'~'Grain maize',
    CODE_CULTU=='SOG'~'Sorghum',
    (CODE_CULTU=='ORP')|(CODE_CULTU=='ORH')~'Barley',
    CODE_CULTU=='LEC'~'Lentil',
    CODE_CULTU=='SOJ'~'Soybean',
    CODE_CULTU=='LEC'~'Lentil',
    CODE_CULTU=='TRN'~'Sunflower',
    (CODE_CULTU=='CZP')|(CODE_CULTU=='CZH')~'Rapeseed',
    TRUE~'NA'
  ))%>%
  filter(crop!='NA')

# Make maps
anim<-tm_shape(multi)+
  tm_fill(col='#2b9348')+
  tm_facets(along = "crop", free.coords = FALSE)+
tm_shape(dep)+
  tm_borders(lwd=2,col='#00291F')+
  tm_layout(
    frame = FALSE,
    fontfamily='Playfair Display ExtraBold',
    main.title.position=c('left','TOP'),
    main.title.color='#00291F')

# Save animation
tmap_animation(anim, filename = "crop_anim.gif", delay = 100)
