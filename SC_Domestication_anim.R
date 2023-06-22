library(tidyverse)
library(sf)
library(rnaturalearth)
library(bertin)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(scico)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Bubbler One","bub")
font_add_google("Adamina","adam")
font_add_google("Staatliches","sta")
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

map<-read_sf('Data/Domestication/Map/foyer_dom.shp')

soybean<-read_delim('Data/Domestication/Data/soyabeans_2020.csv',delim=",")%>%
  select(Area,iso_a3_eh='Area Code (ISO3)',soybean_area=Value)

wheat<-read_delim('Data/Domestication/Data/wheat_area_2020.csv',delim=",")%>%
  select(Area,iso_a3_eh='Area Code (ISO3)',wheat_area=Value)

maize<-read_delim('Data/Domestication/Data/maize_2020.csv',delim=",")%>%
  select(Area,iso_a3_eh='Area Code (ISO3)',maize_area=Value)

rice<-read_delim('Data/Domestication/Data/rice_2020.csv',delim=",")%>%
  select(Area,iso_a3_eh='Area Code (ISO3)',rice_area=Value)

sau<-read_delim('Data/Domestication/Data/sau_2020.csv',delim=",")%>%
  filter(Item=="Arable land")%>%
  mutate(arable_land=Value*1000)%>%
  select(Area,iso_a3_eh='Area Code (ISO3)',arable_land)

wrld<-ne_countries(type = "countries", scale = "small")%>%
  st_as_sf()%>%
  st_transform(crs='EPSG:4326')

############################################################"

map_cent<-map%>%st_centroid()

data<-sau%>%
  left_join(wheat)%>%
  left_join(maize)%>%
  left_join(soybean)%>%
  left_join(rice)%>%
  mutate(
    per_wheat=wheat_area/arable_land,
    per_soy=soybean_area/arable_land,
    per_corn=maize_area/arable_land,
    per_rice=rice_area/arable_land,
  )

wrld_dat<-wrld%>%
  left_join(data,by=c('adm0_a3'='iso_a3_eh'))

sf_use_s2(FALSE)
wrld_dat2<-st_make_valid(wrld_dat)
wrld_pts<-make_points(
  polygon=wrld_dat2, # Input file (sf object)
  n=50, # Number of points per side
  square=TRUE # Points shaped as squares (hexagons otherwise)
)
sf_use_s2(TRUE)

sf_use_s2(FALSE)
wrld_cent<-wrld_dat%>%st_centroid()
sf_use_s2(TRUE)



pal<-scico(7, palette = 'buda',direction=-1)



wrld_dat<-wrld_dat%>%
  mutate(soy_cl=case_when(
    per_soy<0.05~"1",
    per_soy<0.1~"2",
    per_soy<0.15~"3",
    per_soy<0.2~"4",
    per_soy<0.25~"5",
    per_soy<0.3~"6",
    per_soy>=0.35~"7",
    name_en=="Antarctica"~NA,
    name_en=="Greenland"~NA,
    TRUE~"1"
  ))%>%
  mutate(whe_cl=case_when(
    per_wheat<0.05~"1",
    per_wheat<0.1~"2",
    per_wheat<0.15~"3",
    per_wheat<0.2~"4",
    per_wheat<0.25~"5",
    per_wheat<0.3~"6",
    per_wheat>=0.35~"7",
    name_en=="Antarctica"~NA,
    name_en=="Greenland"~NA,
    TRUE~"1"
  ))%>%
  mutate(cor_cl=case_when(
    per_corn<0.05~"1",
    per_corn<0.1~"2",
    per_corn<0.15~"3",
    per_corn<0.2~"4",
    per_corn<0.25~"5",
    per_corn<0.3~"6",
    per_corn>=0.35~"7",
    name_en=="Antarctica"~NA,
    name_en=="Greenland"~NA,
    TRUE~"1"
  ))%>%
  mutate(ric_cl=case_when(
    per_rice<0.05~"1",
    per_rice<0.1~"2",
    per_rice<0.15~"3",
    per_rice<0.2~"4",
    per_rice<0.25~"5",
    per_rice<0.3~"6",
    per_rice>=0.35~"7",
    name_en=="Antarctica"~NA,
    name_en=="Greenland"~NA,
    TRUE~"1"
  ))

wrld_dat$soy_cl <- factor(wrld_dat$soy_cl, levels = c("1", "2", "3","4", "5", "6","7"))

pal_cl<-c(
  "1"=pal[1],
  "2"=pal[2],
  "3"=pal[3],
  "4"=pal[4],
  "5"=pal[5],
  "6"=pal[6],
  "7"=pal[7]
)
#########################################

prj<-tibble(x=seq(-180,180,2.5))%>%
  mutate(proj=glue::glue("+proj=aeqd +lon_0={x} +lat_0=90"))

grat <- sf::st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9))

col_foy <- "#06D6A0"
col_crop <- "#faae7b"
col_back <- "#151515"
col_line <- "white"

key_width <- 2

theme_cust<-theme(
  plot.background=element_rect(fill=col_back,color=NA),
  legend.position="bottom",
  legend.spacing.x = unit(0, 'cm'),
  legend.title=element_text(color=col_line,family="adam",size=60,margin=margin(0,0,-0.5,0,"cm")),
  legend.text=element_text(color=col_line,family="bub",size=60,margin=margin(-0.5,0,0,0,"cm")),
  plot.title = element_text(size=100,family="sta",color=col_line,hjust=0.5,margin=margin(1,0,0.25,0,"cm"))
)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 40, 
  height = 40, 
  units = "cm", 
  dpi = 300
)

fun_proj = function(proj){

sy<-ggplot()+
  geom_sf(
    wrld_dat%>%filter(name_en!="Antarctica"),
    mapping=aes(fill=soy_cl,geometry=geometry),
    color=alpha("white",1),lwd=0.05
  )+
  geom_sf(
    map_cent%>%filter(id==1)%>%st_buffer(exp(14)),
    mapping=aes(geometry=geometry),
    alpha=0.75,fill=col_foy,
    inherit.aes = FALSE
  )+
  geom_sf(
    grat%>%filter(type!="E")%>%filter(degree>-80),
    mapping=aes(geometry=geometry),
    color=alpha(col_line,0.5),lwd=0.25,
    inherit.aes=FALSE)+
  labs(title="Soybean",fill="share of each crop in arable land in 2020")+
  scale_fill_manual(
    values=pal_cl,
    na.value=alpha("grey80",1),
    label=c("5 %","10 %",'15 %', '20 %', '25 %', '30%','35 %'),
    drop=FALSE
  )+
  coord_sf(crs=proj)+
  guides(
    fill = guide_legend(
      nrow = 1,title.position="top",title.hjust=0.5,
      label.position = "bottom",keywidth = unit(key_width,"cm")
    )
  )+
  theme_void()+
  theme_cust

wh<-ggplot()+
  geom_sf(
    wrld_dat%>%filter(name_en!="Antarctica"),
    mapping=aes(fill=whe_cl,geometry=geometry),
    color=alpha("white",1),lwd=0.05
  )+
  geom_sf(
    map_cent%>%filter(id==2)%>%st_buffer(exp(14)),
    mapping=aes(geometry=geometry),
    alpha=0.75,fill=col_foy,
    inherit.aes = FALSE
  )+
  geom_sf(
    grat%>%filter(type!="E")%>%filter(degree>-80),
    mapping=aes(geometry=geometry),
    color=alpha(col_line,0.5),lwd=0.25,
    inherit.aes=FALSE)+
  
  scale_fill_manual(
    values=pal_cl,
    na.value=alpha("grey80",1),
    drop=FALSE
  )+
  coord_sf(crs=proj)+
  labs(title="Wheat")+
  guides(
    fill = 'none'
  )+
  theme_void()+
  theme_cust

co<-ggplot()+
  geom_sf(
    wrld_dat%>%filter(name_en!="Antarctica"),
    mapping=aes(fill=cor_cl,geometry=geometry),
    color=alpha("white",1),lwd=0.05
  )+
  geom_sf(
    map_cent%>%filter(id==3)%>%st_buffer(exp(14)),
    mapping=aes(geometry=geometry),
    alpha=0.75,fill=col_foy,
    inherit.aes = FALSE
  )+
  geom_sf(
    grat%>%filter(type!="E")%>%filter(degree>-80),
    mapping=aes(geometry=geometry),
    color=alpha(col_line,0.5),lwd=0.25,
    inherit.aes=FALSE)+
  
  scale_fill_manual(
    values=pal_cl,
    na.value=alpha("grey80",1),
    drop=FALSE
  )+
  coord_sf(crs=proj)+
  labs(title="Corn")+
  guides(
    fill = 'none'
  )+
  theme_void()+
  theme_cust

ri<-ggplot()+
  geom_sf(
    wrld_dat%>%filter(name_en!="Antarctica"),
    mapping=aes(fill=ric_cl,geometry=geometry),
    color=alpha("white",1),lwd=0.05
  )+
  geom_sf(
    map_cent%>%filter(id==1)%>%st_buffer(exp(14)),
    mapping=aes(geometry=geometry),
    alpha=0.75,fill=col_foy,
    inherit.aes = FALSE
  )+
  geom_sf(
    grat%>%filter(type!="E")%>%filter(degree>-80),
    mapping=aes(geometry=geometry),
    color=alpha(col_line,0.5),lwd=0.25,
    inherit.aes=FALSE)+
  
  scale_fill_manual(
    values=pal_cl,
    na.value=alpha("grey80",1),
    drop=FALSE
  )+
  coord_sf(crs=proj)+
  labs(title="Rice")+
  guides(
    fill = 'none'
  )+
  theme_void()+
  theme_cust

#################################



ptch<-wh+co+ri+sy+
  plot_layout(guides = "collect") & 
  theme(
    plot.margin=margin(0.5,0,0.5,0,"cm"),
    legend.position = 'bottom',
    plot.background=element_rect(fill=col_back,color=NA)
  )

return(ptch)

}

fun_proj(prj$proj[prj$x==0])

for (i in 1:dim(prj)[1]){
  print(fun_proj(prj$proj[dim(prj)[1]-i]))
}

# Make animation
gg_playback(
  name = file.path(tempdir(), "recording", "domestication.gif"),
  first_image_duration = 1,
  last_image_duration = 1,
  frame_duration = 0.25
  #image_resize = 1200
)

