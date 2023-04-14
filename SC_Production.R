library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(sf)
library(terra)
library(raster)
library(cartogram)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Pacifico","pac")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Jost","jo")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21, 
  height = 21, 
  units = "cm", 
  dpi = 300 
)

# Data
# https://essd.copernicus.org/articles/15/1357/2023/essd-15-1357-2023.pdf
# https://datacatalog.worldbank.org/search/dataset/0061507

# Total value per 100 km2
fconv <- 0.91 #conversion dollar -> euro

crp <- sf::read_sf('Data/Production/Maps/com_crop.shp')%>%
  mutate(crp_ha=(fconv*X_median*X_count)/SUPERFICIE)%>% 
  dplyr::mutate(crp_ha = replace_na(crp_ha, 0))%>%
  dplyr::select(INSEE_COM,crp_ha,geometry)

frs <- sf::read_sf('Data/Production/Maps/com_for.shp')%>%
  mutate(frs_ha=(fconv*X_median*X_count)/SUPERFICIE)%>% 
  dplyr::mutate(frs_ha = replace_na(frs_ha, 0))%>%
  dplyr::select(INSEE_COM,frs_ha,geometry)

liv <- sf::read_sf('Data/Production/Maps/com_liv.shp')%>%
  mutate(liv_ha=(fconv*X_median*X_count)/SUPERFICIE)%>% 
  dplyr::mutate(liv_ha = replace_na(liv_ha, 0))%>%
  dplyr::select(INSEE_COM,liv_ha,geometry)

fsh <- sf::read_sf('Data/Production/Maps/com_fsh.shp')%>%
  mutate(fsh_ha=(fconv*X_median*X_count)/SUPERFICIE)%>% 
  dplyr::mutate(fsh_ha = replace_na(fsh_ha, 0))%>%
  dplyr::select(INSEE_COM,fsh_ha,geometry)

all <- crp%>%
  left_join(st_drop_geometry(frs))%>%
  left_join(st_drop_geometry(liv))%>%
  left_join(st_drop_geometry(fsh))%>%
  mutate(all_ha=crp_ha+frs_ha+liv_ha+fsh_ha)

ctg <- all%>%
  group_by(INSEE_COM)%>%
  mutate(mx=max(c(crp_ha,frs_ha,liv_ha,fsh_ha)))%>%
  ungroup()%>%
  mutate(ctg=case_when(
    mx==crp_ha~"Crop",
    mx==frs_ha~"Forest",
    mx==liv_ha~"Livestock",
    mx==fsh_ha~"Fishery"
  ))




# Color palettes
pal_liv <- c(
  "#FFEBF2",
  "#FFADCA",
  "#FF70A2",
  "#FF4788",
  "#FF1B6B"
)

pal_crp <-c(
  "#FFEAD6",
  "#FFD5AD",
  "#FFC085",
  "#FFA047",
  "#FF7D00"
)

pal_frs <-c(
  "#E2F4E1",
  "#B7E3B5",
  "#8CD388",
  "#60C25B",
  "#43A43D"
)

pal_fsh <-c(
  "#D6F5FF",
  "#ADEBFF",
  "#71DBFE",
  "#34CBFE",
  "#01BEFE"
)

pal_ctg <- c(
  "Crop"=pal_crp[4],
  "Livestock"=pal_liv[4],
  "Forest"=pal_frs[4],
  "Fishery"=pal_fsh[4]
)

custom_theme<-theme(
  #plot.background = element_rect(fill="#1D201F",color=NA),
  plot.margin=margin(1,1,1,1,"cm"),
  plot.title = element_text(
    family="ral",size=60,face="bold",
    hjust=0.5,color="white"
  ),
  legend.position = "right",
  legend.title = element_markdown(
    size=45,family="bit",lineheight = 0.35,hjust=0,
    margin=margin(0,0,-0.4,0,"cm"),color="white"
  ),
  legend.text = element_text(
    size=45,family="jo",angle=0,hjust=1,
    margin=margin(0,0,0,-0.4,"cm"),color="white"
  )
)

crop<-ggplot()+
  geom_sf(
    #all,
    crp,
    #fsh,
    mapping=aes(fill=crp_ha),
    color=alpha("white",0),
    size=0.01
  )+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal_crp,
    na.value = "white",
    breaks = seq(200,1600,400),
    labels = glue::glue("{seq(200,1600,400)} €"),
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "vertical",
      barheight = unit(50, units = "mm") , 
      barwidth = unit(6, units = "mm"),
      title.position = 'top',
      label.position = "right"
    )
  )+
  coord_sf(crs = sf::st_crs(2154), datum= NA)+
  labs(
    title = "Crop production value",
    fill="**GDP**<br><span style='font-size:60px; color:grey80;'>euros per hectare</span>"
  )+
  theme_void()+
  custom_theme
crop

livestock<-ggplot()+
  geom_sf(
    #frs,
    liv,
    mapping=aes(fill=liv_ha),
    color=alpha("white",0),
    size=0.01
  )+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal_liv,
    na.value = "white",
    breaks = seq(200,1600,400),
    labels = glue::glue("{seq(200,1600,400)} €"),
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "vertical",
      barheight = unit(50, units = "mm") , 
      barwidth = unit(6, units = "mm"),
      title.position = 'top',
      label.position = "right"
    )
  )+
  coord_sf(crs = sf::st_crs(2154), datum= NA)+
  labs(
    title = "Livestock production value",
    fill="**GDP**<br><span style='font-size:60px; color:grey80;'>euros per hectare</span>"
  )+
  theme_void()+
  custom_theme

livestock

forest<-ggplot()+
  geom_sf(
    frs,
    mapping=aes(fill=frs_ha),
    color=alpha("white",0),
    size=0.01
  )+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal_frs,
    na.value = "white",
    breaks = seq(200,1600,400),
    labels = glue::glue("{seq(200,1600,400)} €"),
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "vertical",
      barheight = unit(50, units = "mm") , 
      barwidth = unit(6, units = "mm"),
      title.position = 'top',
      label.position = "right"
    )
  )+
  coord_sf(crs = sf::st_crs(2154), datum= NA)+
  labs(
    title = "Wood forest production value",
    fill="**GDP**<br><span style='font-size:60px; color:grey80;'>euros per hectare</span>"
  )+
  theme_void()+
  custom_theme

fish<-ggplot()+
  geom_sf(
    fsh,
    mapping=aes(fill=fsh_ha),
    color=alpha("white",0),
    size=0.01
  )+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal_fsh,
    na.value = "white",
    breaks = seq(200,1600,400),
    labels = glue::glue("{seq(200,1600,400)} €"),
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "vertical",
      barheight = unit(50, units = "mm") , 
      barwidth = unit(6, units = "mm"),
      title.position = 'top',
      label.position = "right"
    )
  )+
  coord_sf(crs = sf::st_crs(2154), datum= NA)+
  labs(
    title = "Fishery production value",
    fill="**GDP**<br><span style='font-size:60px; color:grey80;'>euros per hectare</span>"
  )+
  theme_void()+
  custom_theme

fish

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 42, 
  height = 42, 
  units = "cm", 
  dpi = 300 
)

crop+livestock+forest+fish&
  theme(
    plot.background = element_rect(fill=NA,color=NA)
  )

all<-ggplot()+
  geom_sf(
    #all,
    ctg,
    #fsh,
    mapping=aes(fill=ctg),
    color=alpha("white",0),
    size=0.01
  )+
  scale_fill_manual(values=pal_ctg)+
  coord_sf(crs = sf::st_crs(2154), datum= NA)+
  labs(
    title = "Crop production value",
    fill="**Main agricultural production**<br><span style='font-size:80px; color:grey80;'>based on GDP estimates</span>"
  )+
  guides(fill = guide_legend(title.position = "top"))+
  theme_void()+
  theme(
    #plot.background = element_rect(fill="#1D201F",color=NA),
    plot.margin=margin(1,1,1,1,"cm"),
    #plot.title = element_text(family="ral",size=110,face="bold",hjust=0.5,color="white"),
    plot.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_markdown(
      size=70,family="bit",lineheight = 0.45,hjust=0.5,
      margin=margin(0,0,-0.4,0,"cm"),color="white"
    ),
    legend.text = element_text(
      size=70,family="jo",angle=0,hjust=1,
      margin=margin(0,0,0,-0.8,"cm"),color="white"
    ),
    legend.key.size = unit(1.5, 'cm')
  )

all

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 63, 
  height = 42, 
  units = "cm", 
  dpi = 300 
)


all+crop/livestock+forest/fish
