library(tidyverse)
library(sf)
library(scico)
library(rnaturalearth)
library(camcorder)
library(showtext)
library(ggtext)
library(terra)
library(tidyterra)
library(patchwork)


# Set fonts
font_add_google("Open Sans","open")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 40, 
  height = 40, 
  units = "cm", 
  dpi = 600 
)

ne<-ne_countries(type = "countries", scale = "large", returnclass='sf')%>%
  st_transform(crs='EPSG:2154')%>%
  filter(admin!="Antarctica")

ne_france<-ne_countries(
  country="france",
  type = "countries", scale = "large", returnclass='sf'
)%>%
  st_transform(crs='EPSG:2154')

ne_france_dep<-ne_states(
  country="france",
  returnclass='sf'
)%>%
  st_transform(crs='EPSG:2154')

ne_france_region<-ne_states(
  country="france",
  returnclass='sf'
)%>%
  st_transform(crs='EPSG:2154')%>%
  mutate(ct=1)%>%
  group_by(region)%>%
  summarize(sm=sum(ct))%>%
  mutate(rdt_ble_2023=case_when(
    region=="Auvergne-Rhône-Alpes"~65.2,
    region=="Bourgogne-Franche-Comté"~62.3,
    region=="Centre-Val de Loire"~71.9,
    region=="Bretagne"~73.8,
    region=="Corse"~40.0,
    region=="Grand Est"~73.7,
    region=="Hauts-de-France"~88.8,
    region=="Île-de-France"~82.3,
    region=="Normandie"~84.0,
    region=="Nouvelle-Aquitaine"~64.9,
    region=="Occitanie"~52.9,
    region=="Pays de la Loire"~74.3,
    region=="Provence-Alpes-Côte-d'Azur"~40,
    TRUE~NA
  ))

dpt<-read_sf('map/france_sport.gpkg')%>%
  mutate(dep=str_remove(cd_dprt, "^0+"))


# Load rasters
##############

# Wheat
whe<-terra::rast('map/wheat_harvested_france.tif')%>%
  mutate(val=wheat_harvested_france)%>%
  project('EPSG:2154')

# Maize
mai<-terra::rast('map/maize_harvested_france.tif')%>%
  mutate(val=maize_harvested_france)%>%
  project('EPSG:2154')

# Barley
bar<-terra::rast('map/barley_harvested_france.tif')%>%
  mutate(val=barley_harvested_france)%>%
  project('EPSG:2154')

#whe<-terra::crop(x=whe,y=ne_france%>%st_buffer(-10000),snap="in")

# Rapeseed
rap<-terra::rast('map/rape_harvested_france.tif')%>%
  mutate(val=rape_harvested_france)%>%
  project('EPSG:2154')

# Rapeseed
sun<-terra::rast('map/sunflower_harvested_france.tif')%>%
  mutate(val=sunflower_harvested_france)%>%
  project('EPSG:2154')

# Grape
gra<-terra::rast('map/grape_harvested_france.tif')%>%
  mutate(val=grape_harvested_france)%>%
  project('EPSG:2154')

# Sugarbeet
sug<-terra::rast('map/sugarbeet_harvested_france.tif')%>%
  mutate(val=sugarbeet_harvested_france)%>%
  project('EPSG:2154')

# Triticale
tri<-terra::rast('map/triticale_harvested_france.tif')%>%
  mutate(val=triticale_harvested_france)%>%
  project('EPSG:2154')

# Triticale
pot<-terra::rast('map/potato_harvested_france.tif')%>%
  mutate(val=potato_harvested_france)%>%
  project('EPSG:2154')

pal<-rev(scico(10, palette = 'davos'))
#pal<-rev(scico(8, palette = 'nuuk'))




col_ne <- "#F0E6D1"
#bck <- "#E0EBF5"
bck <- NA

alp <- 1

bbox_x <- c(0,1338658.495235)
bbox_y <- c(6017316.715989,7188794.494467)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 40, 
  height = 40, 
  units = "cm", 
  dpi = 600 
)

fun_plot <- function(data){
  
  clean<-data%>%
    mutate(crop_cl=case_when(
      val<75~"A",
      val<150~"B",
      val<225~"C",
      val<300~"D",
      val<375~"E",
      val<450~"F",
      val<525~"G",
      val<600~"H",
      val<675~"I",
      val<10000~"J",
      T~"A"
    ))%>%
    drop_na()
  
  msk<-terra::mask(
    clean,ne_france%>%st_buffer(-2000),touches=F
  )
  
  plot<-ggplot()+
    geom_sf(
      data=ne_france,
      mapping=aes(geometry=geometry),
      fill=alpha(pal[1],alp),color=NA
    )+
    tidyterra::geom_spatraster(
      msk%>%drop_na(crop_cl), 
      mapping=aes(fill = crop_cl),
      na.rm = TRUE,
      maxcell = 500e+05,
      alpha=alp
    )+
    geom_sf(
      data=ne_france_dep,
      mapping=aes(geometry=geometry),
      fill=NA,color="dimgrey",linewidth=0.05
    )+
    geom_sf(
      data=ne_france_region,
      mapping=aes(geometry=geometry),
      fill=NA,color="dimgrey",linewidth=0.15
    )+
    geom_sf(
      data=ne_france,
      mapping=aes(geometry=geometry),
      fill=NA,color="black",linewidth=0.25
    )+
    scale_fill_manual(values=pal,na.value = NA)+
    scale_x_continuous(limits=bbox_x)+
    scale_y_continuous(limits=bbox_y)+
    guides(color='none',size='none',fill='none')+
    theme_void()+
    theme(
      plot.background = element_rect(fill = bck, color = NA)
    )
  print(plot)
}

# 1
p_whe<-fun_plot(whe)
# 2
p_bar<-fun_plot(data=bar)
# 3
p_mai<-fun_plot(mai)
# 4
p_rap<-fun_plot(rap)
# 5
p_sun<-fun_plot(sun)
# 6
p_gra<-fun_plot(gra)
# 7
p_sug<-fun_plot(sug)
# 8
p_tri<-fun_plot(tri)
# 9
p_pot<-fun_plot(pot)



# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 160, 
  height = 40, 
  units = "cm", 
  dpi = 300,
  limitsize = FALSE
)

p_whe+p_bar+p_mai+p_rap+
  p_sun+p_gra+p_sug+p_pot+
  plot_layout(ncol = 4)&
  theme(
    plot.background=element_rect(fill=NA,color=NA)
  )
