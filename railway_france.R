library(tidyverse)
library(sf)
library(showtext)
library(camcorder)
library(ggtext)
library(MapColoring)
library(rnaturalearth)
library(wesanderson)
library(patchwork)

# Set fonts
font_add_google("Oswald","os")
font_add_google("Bebas Neue","beb")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 900 
)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load data
###########

fr<-ne_states(country = "France", returnclass = "sf")%>%
  filter(type_en=="Metropolitan department")%>%
  group_by(type_en)%>%
  summarize()%>%
  st_transform(crs='EPSG:2154')

st_crs(fr)

dep<-ne_states(country = "France", returnclass = "sf")%>%
  filter(type_en=="Metropolitan department")%>%
  st_transform(crs='EPSG:2154')

#fr_col <- fr%>%
#  mutate(col=getColoring(as(fr,"Spatial")))

# Dataset : https://www.data.gouv.fr/fr/datasets/fichier-de-formes-des-lignes-du-reseau-ferre-national/
rail<-read_sf('https://github.com/BjnNowak/TidyTuesday/raw/main/data/rail_france.gpkg')%>%
  st_transform(crs='EPSG:2154')

st_crs(rail)




levels(as.factor(rail$libelle))

use <- "Exploitée"
service <- "Transférée en voie de service"
tiers <- "Fermée et mise à disposition de tiers" 
defense <- "Neutralisée et conservée pour les besoins de la Défense"
closed <- c(
  "Déclassée non vendue","Déclassée vendue","Fermée","Fermée avec maintien en place de la voie",
  "Fermée et déposée (Plus utilisable)", "Fermée non déposée (Plus utilisable)", "Neutralisée",
  "Retranchée (Plus utilisable)"  
)

# Looking at lengths
sum(rail%>%filter(libelle%in%use)%>%st_length())/1000
sum(rail%>%filter(libelle%in%closed)%>%st_length())/1000
sum(rail%>%filter(libelle%in%service)%>%st_length())/1000
sum(rail%>%filter(libelle%in%defense)%>%st_length())/1000


# Get dep for each rail 
rail_per_dep_sf<-rail%>%
  filter(libelle%in%use)%>%
  st_intersection(dep)
# Get rail length
lg<-rail_per_dep_sf%>%st_length()

rail_per_dep<-rail_per_dep_sf%>%
  bind_cols(lg=as.numeric(lg))%>%
  st_drop_geometry()%>%
  group_by(name)%>%
  summarize(sm_length=sum(lg))

dep_clean <- dep%>%
  mutate(ar=as.numeric(st_area(dep)))%>%
  left_join(rail_per_dep)%>%
  mutate(density=sm_length/(ar/1000000))%>%
  mutate(density_cl=case_when(
    density<20~1,
    density<40~2,
    density<60~3,
    density<80~4,
    density<100~5,
    TRUE~6
  ))%>%
  select(name,density,density_cl)


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 900 
)

col_fill <- "#1F2733"
col_all <- "grey80"
col_closed <- "#F94144"
col_military <- '#BFBCCB'
col_use <- "#90BE6D"
col_service <- "#f9c74f" 

lwd_all<-0.05
lwd_spec<-0.25

theme_custom <- theme_void()+
  theme(
    plot.margin=margin(0.5,0.5,0.5,0.5,'cm'),
    plot.title=element_text(family="os",size=105,face='bold',hjust=0.5),
    plot.subtitle=element_text(family="ral",size=90,hjust=0.5),
    plot.background=element_rect(fill="#080A0D")
  )

m1<-ggplot()+
  geom_sf(fr,mapping=aes(geometry=geometry),fill=col_fill,color=NA)+
  geom_sf(
    rail,mapping=aes(geometry=geom),
    lwd=lwd_all
  )+
  geom_sf(
    rail%>%filter(libelle=='Exploitée'),mapping=aes(geometry=geom),
    col=col_use,lwd=lwd_spec
  )+
  #geom_sf(rail%>%filter(libelle%in%defense),mapping=aes(geometry=geom),col="red")+
  #geom_sf(rail%>%filter(libelle%in%tiers),mapping=aes(geometry=geom),col="red")+
  guides(fill='none')+
  labs(
    title="In operation",
    subtitle="27,595 km"
  )+
  theme_custom+
  theme(
    plot.title=element_text(color=col_use),
    plot.subtitle=element_text(color=col_use)
  )
m1

m2<-ggplot()+
  geom_sf(fr,mapping=aes(geometry=geometry),fill=col_fill,color=NA)+
  geom_sf(
    rail,mapping=aes(geometry=geom),
    lwd=lwd_all
  )+
  geom_sf(
    rail%>%filter(libelle%in%closed),mapping=aes(geometry=geom),
    col=col_closed,lwd=lwd_spec
  )+
  #geom_sf(rail%>%filter(libelle%in%defense),mapping=aes(geometry=geom),col="red")+
  #geom_sf(rail%>%filter(libelle%in%tiers),mapping=aes(geometry=geom),col="red")+
  guides(fill='none')+
  labs(
    title="Closed",
    subtitle="7,977 km"
  )+
  theme_custom+
  theme(
    plot.title=element_text(color=col_closed),
    plot.subtitle=element_text(color=col_closed)
  )

m3<-ggplot()+
  geom_sf(fr,mapping=aes(geometry=geometry),fill=col_fill,color=NA)+
  geom_sf(
    rail,mapping=aes(geometry=geom),
    lwd=lwd_all
  )+
  geom_sf(
    rail%>%filter(libelle%in%service),mapping=aes(geometry=geom),
    col=col_service,lwd=lwd_spec+0.5
  )+
  #geom_sf(rail%>%filter(libelle%in%defense),mapping=aes(geometry=geom),col="red")+
  #geom_sf(rail%>%filter(libelle%in%tiers),mapping=aes(geometry=geom),col="red")+
  guides(fill='none')+
  labs(
    title="Service track",
    subtitle="330 km"
  )+
  theme_custom+
  theme(
    plot.title=element_text(color=col_service),
    plot.subtitle=element_text(color=col_service)
  )

m4<-ggplot()+
  geom_sf(fr,mapping=aes(geometry=geometry),fill=col_fill,color=NA)+
  geom_sf(
    rail,mapping=aes(geometry=geom),
    lwd=lwd_all
  )+
  geom_sf(
    rail%>%filter(libelle%in%defense),mapping=aes(geometry=geom),
    col=col_military,lwd=lwd_spec+0.5
  )+
  #geom_sf(rail%>%filter(libelle%in%defense),mapping=aes(geometry=geom),col="red")+
  #geom_sf(rail%>%filter(libelle%in%tiers),mapping=aes(geometry=geom),col="red")+
  guides(fill='none')+
  labs(
    title="Military track",
    subtitle="64 km"
  )+
  theme_custom+
  theme(
    plot.title=element_text(color=col_military),
    plot.subtitle=element_text(color=col_military)
  )


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 900 
)

m1+m2+m3+m4&
  theme(plot.background = element_rect(fill="#080A0D",color=NA))


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Set color palette
pal <- c("#bb3e03","#ee9b00","#e9d8a6","#94d2bd","#0a9396","#005f73")
pal <- c("#f9f5e0","#f7bf9e","#e69699","#d46c94","#CC65A2","#b34ecc")
pal <- c("#f9f5e0","#f6ef71","#dbe64b","#c0dd24","#7fce51","#51c26f")
# Set color background
bck <- "#080A0D"

# Set theme 
theme_custom <- theme_void()+
  theme(
    plot.margin=margin(1,0,1,0,"cm"),
    plot.background = element_rect(fill=bck,color=NA),
    legend.position="bottom",
    legend.spacing.x = unit(-0.1, 'cm'),
    legend.title = element_markdown(
      size=40,family="os",hjust=0.5,lineheight=0.45,
      color="white",
      margin=margin(-0.5,0,-1,0,"cm")
    ),
    legend.text = element_text(
      size=35,family="ral",color="white",
      margin=margin(-0.5,0,0,0,"cm")
    ),
    plot.title = element_markdown(
      size=70,family="os",color="white",hjust=0.5,face="bold",
      margin=margin(0.5,0,0.5,0,"cm")
    ),
    plot.caption = element_markdown(
      size=25,family="fira",color="white",hjust=0.5,
      margin=margin(0.75,0,0,0,"cm")
    )
  )

ggplot(dep_clean,aes(fill=as.character(density_cl)))+
  geom_sf(color=alpha("white",0.5))+
  labs(fill="**In operation railway density**<br><span style='color:#080A0D;'>meter of track per km<sup>2</sup> of surface area</span>")+
  guides(
    fill=guide_legend(
      nrow=1,
      title.position="top",
      label.position="bottom"
    )
  )+
  scale_fill_manual(
    values=pal,
    label=c("< 20","< 40","< 60","< 80","< 100", "≥ 100")
  )+
  guides(
    color='none',
    fill=guide_legend(
      keywidth=4,keyheight=1,nrow=1,
      title.position="top",label.position="bottom"
    )
  )+
  theme_custom


capitale <- c('Paris','Hauts-de-Seine','Seine-Saint-Denis','Val-de-Marne') 


col_track<-"#801A86"
ggplot()+
  geom_sf(
    dep%>%filter(name%in%capitale),
    mapping=aes(geometry=geometry),fill=pal[6],
    color=alpha("white",0.5)
  )+
  geom_sf(rail_per_dep_sf%>%filter(name%in%capitale),mapping=aes(geometry=geom),color=col_track,lwd=1)+
  theme_custom+
  theme(plot.background = element_rect(fill=NA,color=NA))

ggplot()+
  geom_sf(
    dep%>%filter(name=="Gers"),
    mapping=aes(geometry=geometry),fill=pal[1],
    color=alpha("white",0.5)
  )+
  geom_sf(rail_per_dep_sf%>%filter(name=="Gers"),mapping=aes(geometry=geom),color=col_track,lwd=1)+
  theme_custom+
  theme(plot.background = element_rect(fill=NA,color=NA))



