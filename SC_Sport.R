library(tidyverse)
library(sf)
library(showtext)
library(camcorder)
library(ggtext)

# Set fonts
font_add_google("Luckiest Guy","ramp")
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
  dpi = 300 
)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load data
###########

map <- read_sf('Map/population-francaise-par-departement-2018.shp')

# Data : https://www.observatoire-des-territoires.gouv.fr/nombre-de-licencies-sportifs
data <- read_delim('Data/licsport.csv',delim=';')%>%
  group_by(codgeo)%>%
  filter(an==max(an))%>%
  ungroup()%>%
  select(code_depart=codgeo,nb_licsport)

# Keep only metropolitant France
dom<-c("971","972","973","974","975","976")

clean<-map%>%
  st_transform(crs='EPSG:2154')%>%
  mutate(
    surf=as.numeric(st_area(map))/1000000,
    dens=population/surf
  )%>%
  filter(code_depart%!in%dom)

# Make grid and centroids
grd<-st_make_grid(clean, n = c(60,60))%>%
  st_sf()%>%
  mutate(id=row_number())
  
cent<-grd%>%
  st_centroid()%>%
  st_intersection(clean)%>%
  left_join(data)%>%
  mutate(per_lic=nb_licsport/population*10*100)%>%
  mutate(cl_lic=case_when(
    per_lic<18~"1",
    per_lic<20~"2",
    per_lic<22~"3",
    per_lic<24~"4",
    per_lic<26~"5",
    TRUE~"6"
  ))

cent_no_geom<-cent%>%st_drop_geometry()

sel<-cent_no_geom%>%pull(id)

grd_fr<-grd%>%filter(id%in%sel)%>%left_join(cent_no_geom)

# Make plot
###########

# Set colors
bck <- "#001219"
pal <- rev(c("#005f73","#0a9396","#94d2bd","#e9d8a6","#ee9b00","#bb3e03"))


ggplot()+
  #geom_sf(grd,mapping=aes(geometry=geometry),color=alpha("black",0.25),fill=bck)+
  #geom_sf(grd%>%st_centroid(),mapping=aes(geometry=geometry),color=alpha("black",0.25),size=1)+
  geom_sf(grd_fr,mapping=aes(fill=cl_lic),color=alpha("black",0.50))+
  geom_sf(cent,mapping=aes(color=cl_lic),size=1)+
  geom_sf(
    cent,mapping=aes(geometry=geometry),
    pch=21,size=1,fill=NA,color="black",alpha=0.50
  )+
  scale_fill_manual(values=pal,label=c("< 18 %","< 20 %","< 22%","< 24 %","< 26 %", "≥ 26 %"))+
  scale_color_manual(values=pal)+
  
  guides(
    color='none',
    fill=guide_legend(
      keywidth=4,keyheight=1,nrow=1,
      title.position="top",label.position="bottom"
    )
  )+
  labs(
    title="Sport in France",
    fill="<b>Members of a sports association</b><br>percentage of the total population",
    caption="**Data** Observatoire des Territoires **| Plot** Benjamin Nowak"
  )+
  theme_void()+
  theme(
    plot.margin=margin(1,0,1,0,"cm"),
    plot.background = element_rect(fill=bck,color=NA),
    legend.position="bottom",
    legend.spacing.x = unit(0, 'cm'),
    legend.title = element_markdown(
      size=38,family="ral",hjust=0.5,lineheight=0.45,
      color="white",
      margin=margin(0,0,-0.5,0,"cm")
    ),
    legend.text = element_text(
      size=35,family="fira",color="white",
      margin=margin(-0.5,0,0,0,"cm")
    ),
    plot.title = element_markdown(
      size=70,family="ramp",color="white",hjust=0.5,face="bold",
      margin=margin(0.5,0,0.5,0,"cm")
    ),
    plot.caption = element_markdown(
      size=25,family="fira",color="white",hjust=0.5,
      margin=margin(0.75,0,0,0,"cm")
    )
  )
