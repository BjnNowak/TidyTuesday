library(tidyverse)
library(showtext)
library(ggtext)
library(camcorder)
library(sf)
library(patchwork)
library(bertin)


# Set fonts
font_add_google("Bangers","bang")
font_add_google("Source Sans Pro","source")
font_add_google("IBM Plex Sans Condensed","ibm")

font_add_google("Fira Sans","fira")
font_add_google("Bitter","bit")

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
######
herd <- read_delim("Data/Troupeaux/FDS_SAANR_6_2020.csv",delim=";")%>%
  filter(FRDOM=="METRO")
crop <- read_delim("Data/Troupeaux/FDS_SAANR_1_2020.csv",delim=";")
sau <- read_delim("Data/Troupeaux/SAU_2020.csv",delim=";")

# Map
#####
dep<-read_sf('Data/Troupeaux/Maps/population-francaise-par-departement-2018.shp')%>%
  st_transform(crs="epsg:2154")

surf<-as.numeric(st_area(dep))/1000000

dep<-dep%>%
  mutate(surf=surf)

fun_anim <- function(label){

cow<-herd%>%
  filter(SAANR_6_LIB_DIM3==label)%>%
  filter(DEP!="...")%>%
  left_join(dep,by=c("DEP"="code_depart"))%>%
  mutate(density=VALEUR/surf)
 
return(cow)
 
}

col_bck<-"#FEF5EB"
col_cow<-"#8ac926"
col_pig<-"#FF595E"
col_shp<-"#F18F01"
col_goa<-"#1982C4"

cw<-ggplot()+
  geom_sf(
    cow,mapping=aes(geometry=geometry),
    fill=col_bck,color=alpha("black",0.1)
  )+
  geom_sf(
    make_points(st_as_sf(fun_anim("Ensemble espèce bovine")),n=55,TRUE),
    mapping=aes(size=density,geometry=geometry),
    color=col_cow,alpha=0.8
  )+
  scale_size_binned(
    range=c(0,3.5),
    breaks=c(1,25,50,75))+
  guides(size=guide_bins(
    keyheight=unit(1,"cm")
  ))+
  theme_void()+
  labs(
    size="Number of<br>**<span style='color:#8ac926'>cows</span>** per km<sup>2</sup>")+
  theme(
    plot.margin=margin(1,1,1,1,"cm"),
    legend.title = element_markdown(
      family="bit",size=50,lineheight=0.35),
    legend.text = element_text(family="ibm",size=40)
  )

pg<-ggplot()+
  geom_sf(
    cow,mapping=aes(geometry=geometry),
    fill=col_bck,color=alpha("black",0.1)
  )+
  geom_sf(
    make_points(st_as_sf(fun_anim("Ensemble espèce porcine")),n=55,TRUE),
    mapping=aes(size=density,geometry=geometry),
    color=col_pig,alpha=0.8
  )+
  scale_size_binned(
    range=c(0,4),
    breaks=c(10,50,150,300)
  )+
  guides(size=guide_bins(
    keyheight=unit(1,"cm")
  ))+
  theme_void()+
  labs(
    size="Number of<br>**<span style='color:#FF595E'>pigs</span>** per km<sup>2</sup>")+
  theme(
    plot.margin=margin(1,1,1,1,"cm"),
    legend.title = element_markdown(
        family="bit",size=50,lineheight=0.35),
    legend.text = element_text(family="ibm",size=40)
  )

sh<-ggplot()+
  geom_sf(
    cow,mapping=aes(geometry=geometry),
    fill=col_bck,color=alpha("black",0.1)
  )+
  geom_sf(
    make_points(st_as_sf(fun_anim("Ensemble espèce ovine")),n=55,TRUE),
    mapping=aes(size=density,geometry=geometry),
    color=col_shp,alpha=0.8
  )+
  scale_size_binned(
    range=c(0,3.5),
    breaks=c(1,10,40,60))+
  guides(size=guide_bins(
    keyheight=unit(1,"cm")
  ))+
  theme_void()+
  labs(
    size="Number of<br>**<span style='color:#F18F01'>sheeps</span>** per km<sup>2</sup>")+
  theme(
    plot.margin=margin(1,1,1,1,"cm"),
    legend.title = element_markdown(
      family="bit",size=50,lineheight=0.35),
    legend.text = element_text(family="ibm",size=40)
  )

go<-ggplot()+
  geom_sf(
    cow,mapping=aes(geometry=geometry),
    fill=col_bck,color=alpha("black",0.1)
  )+
  geom_sf(
    make_points(st_as_sf(fun_anim("Ensemble espèce caprine")),n=55,TRUE),
    mapping=aes(size=density,geometry=geometry),
    color=col_goa,alpha=0.8
  )+
  scale_size_binned(
    range=c(0,3.5),
    breaks=c(1,10,20,30))+
  guides(size=guide_bins(
    keyheight=unit(1,"cm")
  ))+
  theme_void()+
  labs(
    size="Number of<br>**<span style='color:#1982C4'>goats</span>** per km<sup>2</sup>")+
  theme(
    plot.margin=margin(1,1,1,1,"cm"),
    legend.title = element_markdown(
      family="bit",size=50,lineheight=0.35),
    legend.text = element_text(family="ibm",size=40)
  )

tst<-st_as_sf(fun_anim("Ensemble espèce ovine"))
sum(tst$VALEUR)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 41, 
  height = 41, 
  units = "cm", 
  dpi = 300 
)

cw+pg+sh+go
