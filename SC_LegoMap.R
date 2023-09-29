library(tidyverse)
library(sf)
library(showtext)
library(camcorder)
library(ggtext)
library(MapColoring)

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


# Code based on a previous map about sport members association, 
# I did not suppress the lines related to this map

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

my.palette = RColorBrewer::brewer.pal(5, 'Set1')
clean$color = my.palette[getColoring(as(clean, 'Spatial'))]

ggplot(clean,aes(fill=color))+
  geom_sf()+
  scale_fill_manual(values=my.palette)




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


# Set offset
off <- 2000

cent_off <- cent%>%
  # Extract lon and lat and add offset to lon
  mutate(
    lon=st_coordinates(.)[,1]+off,
    lat=st_coordinates(.)[,2],
  )%>%
  # Drop old geometry
  st_drop_geometry()%>%
  # Make new geometry based on new {lon;lat}
  st_as_sf(coords=c('lon','lat'))%>%
  st_set_crs(st_crs(cent))


cent_no_geom<-cent%>%st_drop_geometry()

sel<-cent_no_geom%>%pull(id)

grd_fr<-grd%>%filter(id%in%sel)%>%left_join(cent_no_geom)

# Make plot
###########

# Set colors
bck <- "#073B4C"
bck <- "#001514"
bck <- "#C6DBF0"

txt <- "#212738"
txt <- "white"


pal<-c("#ef476f","#ffd166","#06d6a0","#118ab2","#3A5683")

pal<-c("#ef476f","#ffd166","#06d6a0","#118ab2","#083D77")

pal<-c("#d01012","#f6ec36","#000000","#009ffd","#1effbc")
pal<-c("#d01012","#f6ec36","#4c61db","#ffa70b","#3CB371")
pal<-c("#fad41a","#e71d25","#0496dd","#03a74c","#9aca56")

pal<-c("#ffbe0b","#fb5607","#ff006e","#8338ec","#3a86ff")

# Departement boundaries
ggplot()+
  geom_sf(grd_fr,mapping=aes(fill=color),color=alpha("black",0.50))+
  geom_sf(cent_off,mapping=aes(geometry=geometry),size=0.8,color="black",alpha=0.25)+
  geom_sf(cent,mapping=aes(color=color),size=0.8)+
  geom_sf(
    cent,mapping=aes(geometry=geometry),
    pch=21,size=0.8,fill=NA,color="black",alpha=0.50
  )+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=pal)+
  
  guides(color='none',fill='none')+
  labs()+
  theme_void()+
  theme(
    plot.margin=margin(1,0,1,0,"cm"),
    plot.background = element_rect(fill=NA,color=NA),
    legend.position="bottom",
    legend.spacing.x = unit(0, 'cm'),
    legend.title = element_markdown(
      size=38,family="ral",hjust=0.5,lineheight=0.45,
      color="white",
      margin=margin(0,0,-0.5,0,"cm")
    ),
    legend.text = element_text(
      size=35,family="fira",color=txt,
      margin=margin(-0.5,0,0,0,"cm")
    ),
    plot.title = element_markdown(
      size=70,family="ramp",color=txt,hjust=0.5,face="bold",
      margin=margin(0.5,0,0.5,0,"cm")
    ),
    plot.caption = element_markdown(
      size=25,family="fira",color=txt,hjust=0.5,
      margin=margin(0.75,0,0,0,"cm")
    )
  )

