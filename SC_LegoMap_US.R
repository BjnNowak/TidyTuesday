library(tidyverse)
library(sf)
library(showtext)
library(camcorder)
library(ggtext)
library(MapColoring)
library(rnaturalearth)

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
  dpi = 900 
)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load data
###########

us<-ne_states(country = "United States of America", returnclass = "sf")%>%
  filter(postal!="AK")%>%
  filter(postal!="HI")

ggplot(us)+
  geom_sf()

#my.palette = RColorBrewer::brewer.pal(5, 'Set1')
#clean$color = my.palette[getColoring(as(us, 'Spatial'))]

clean<-us%>%
  mutate(col=getColoring(as(us,"Spatial")))

ggplot(clean,aes(fill=as.character(col)))+
  geom_sf()

max(test)

# Make grid and centroids
grd<-st_make_grid(clean, n = c(60,40))%>%
  st_sf()%>%
  mutate(id=row_number())

cent<-grd%>%
  st_centroid()%>%
  st_intersection(clean)



# Set offset
off <- 0.065

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

grd_us<-grd%>%filter(id%in%sel)%>%left_join(cent_no_geom)


# Make plot
###########


pal<-c("#fad41a","#e71d25","#0496dd","#03a74c")


# Departement boundaries
ggplot()+
  geom_sf(grd_us,mapping=aes(fill=as.character(col)),color=alpha("black",0.50))+
  geom_sf(
    cent_off,mapping=aes(geometry=geometry),size=0.8,
    color=alpha("black",0.25),fill=alpha('black',0),pch=21
  )+
  geom_sf(cent,mapping=aes(color=as.character(col)),size=0.8)+
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


