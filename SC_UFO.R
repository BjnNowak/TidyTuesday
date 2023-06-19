library(tidyverse)
library(sf)
library(rnaturalearth)
library(camcorder)
library(showtext)
library(ggtext)

# Set fonts
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Fira Sans","fira")
font_add_google("Jost","jost")
font_add_google("Russo One","rus")
font_add_google("Open Sans","open")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')

places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')

wrld<-ne_countries(type = "countries", scale = "small")%>%
  st_as_sf()%>%
  st_transform(crs='EPSG:4087')

st_crs(wrld)

data<-ufo_sightings%>%
  left_join(places)%>%
  mutate(n=1)%>%
  group_by(shape,latitude,longitude)%>%
  summarize(sm=sum(n))%>%
  st_as_sf(coords=c("longitude","latitude"),crs='EPSG:4326')%>%
  st_transform(crs='EPSG:4087')

# Checking type of shapes
res<-data%>%
  st_drop_geometry()%>%
  group_by(shape)%>%
  summarize(sm=sum(sm))

clean<-data%>%
  mutate(shape=case_when(
    shape%in%c("light","fireball","flash")~"light",
    shape%in%c("circle","disk","orb","sphere","egg","teardrop","oval")~"round",
    #shape%in%c("cigar","cylinder")~"cylinder",
    shape%in%c("triangle")~"triangle",
    shape%in%c("rectangle")~"rectangle",
    shape%in%c("diamond")~"diamond",
    TRUE~"other"
  ))

ggplot(data)+
  geom_sf()

grd<-st_make_grid(data,n=c(80,80),square=FALSE,flat_topped=TRUE)%>%
  st_as_sf()%>%
  mutate(id=row_number())

ggplot(grd)+
  geom_sf()+
  geom_sf(data,mapping=aes(geometry=geometry))


ufo_type <- st_intersection(clean, grd)%>%
  group_by(id)%>%
  filter(sm==max(sm))%>%
  st_drop_geometry()%>%
  distinct(id,.keep_all = TRUE)

grd_ufo<-grd%>%
  left_join(ufo_type)

grat <- sf::st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) 

pal_shp <- c(
  'diamond'=18,
  'light'=8,
  'round'=16,
  'rectangle'=15,
  'triangle'=17,
  'other'=3
)

pal_fill <- c(
  'diamond'="blue",
  'light'=NA,
  'round'="red",
  'rectangle'="green",
  'triangle'="yellow",
  'other'=NA
)

pal_color <- c(
  'diamond'="#ff0a54",
  'light'="#ffdd13",
  'round'='#067BC2',
  'rectangle'='#DB995A',
  'triangle'='#E5D4ED',
  'other'='#540D6E'
)

grd_ufo$shape<-fct_relevel(grd_ufo$shape,"other",after=Inf)

ggplot(grd_ufo%>%drop_na()%>%st_centroid(),aes(shape=shape,color=shape))+
  geom_sf(
    wrld,
    mapping=aes(geometry=geometry),
    fill="#3e505b",color=alpha("black",0.05),
    inherit.aes=FALSE
  )+
  geom_sf(
    size=0.8
  )+
  geom_sf(
    grat,mapping=aes(geometry=geometry),
    color=alpha("#06d6a0",0.25),
    inherit.aes=FALSE)+
  scale_shape_manual(values=pal_shp)+
  scale_fill_manual(values=pal_fill)+
  scale_color_manual(values=pal_color)+
  coord_sf(crs="+proj=eck4")+
  labs(
    title="Who see what?",
    subtitle="The map below shows the most frequently mentioned UFO shapes in different parts of the world<br>
The luminous shape is far ahead in the United States, but the round shape is also common in other parts of the world",
    shape="Most cited UFO shape",
    color="Most cited UFO shape",
    caption = "**Data**  National UFO Reporting Center | **Plot** @BjnNowak"
  )+ 
  guides(
    colour = guide_legend(nrow = 1,title.position="top",title.hjust=0.5),
    shape = guide_legend(nrow = 1,title.position="top",title.hjust=0.5)
  )+
  theme_void()+
  theme(
    plot.background=element_rect(fill="#151515",color=NA),
    plot.title=element_text(size=40,hjust=0.5,color="#06d6a0",family='rus'),
    plot.subtitle=element_markdown(size=15,hjust=0.5,color="#06d6a0",family='open',lineheight=0.45),
    legend.title=element_text(color="#06d6a0",family="open",size=20,face="bold",margin=margin(0,0,-0.25,0,'cm')),
    legend.text=element_text(color="#06d6a0",family="open",size=15,margin=margin(0,0,0,-0.5,'cm')),
    plot.caption=element_markdown(color="#06d6a0",family="open",size=15,hjust=0.5),
    legend.position = "bottom",
    
    plot.margin=margin(0.5,0.5,0.5,0.5,"cm")
  )
  #coord_sf(crs="EPSG:4328")


# Checking type of shapes
res<-grd_ufo%>%
  st_drop_geometry()%>%
  group_by(shape)%>%
  summarize(sm=sum(sm))
