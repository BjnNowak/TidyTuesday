library(tidyverse)
library(sf)
library(rnaturalearth)
library(camcorder)
library(showtext)
library(ggtext)
library(patchwork)

# Set fonts
font_add_google("Russo One","rus")
font_add_google("Open Sans","open")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 25, 
  height = 15, 
  units = "cm", 
  dpi = 300 
)

ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')

# Pop data downloable here: https://www.sciencebase.gov/catalog/item/57753ebee4b07dd077c70868
pop<-terra::rast('Map/pden2010_60m.tif')%>%
  terra::aggregate(fact=10,fun="mean")%>%
  terra::project('EPSG:4087')

us<-ne_countries(type = "countries", scale = "small")%>%
  st_as_sf()%>%
  st_transform(crs='EPSG:4087')%>%
  filter(admin=="United States of America")%>%
  st_cast("POLYGON")%>%
  mutate(id=row_number())%>%
  filter(id==1)

grd<-st_make_grid(us,n=c(100,100),square=FALSE,flat_topped=TRUE)%>%
  st_as_sf()%>%
  mutate(id=row_number())

data<-ufo_sightings%>%
  left_join(places)%>%
  filter(country_code=="US")%>%
  filter(state!="AK")%>%
  filter(state!="HI")%>%
  st_as_sf(coords=c("longitude","latitude"),crs='EPSG:4326')%>%
  st_transform(crs='EPSG:4087')%>%
  mutate(n=1)

sights <- st_intersection(data, grd)%>%
  group_by(id)%>%
  summarize(sig=sum(n))%>%
  ungroup()%>%
  st_drop_geometry()



pop_inter<- terra::extract(pop, grd)

clean_pop<-pop_inter%>%
  group_by(ID)%>%
  summarize(
    density=mean(na.omit(pden2010_60m))
  )%>%
  ungroup()%>%
  select(id=ID,density)%>%
  mutate(pop_cl=case_when(
    density<10~1,
    density<100~2,
    density<200~3,
    density<300~4,
    density<400~5,
    density>=400~6
  ))

clean<-grd%>%
  left_join(sights)%>%
  mutate(sig_cl=case_when(
    sig<10~1,
    sig<100~2,
    sig<200~3,
    sig<300~4,
    sig<400~5,
    sig>=400~6,
    TRUE~1
  ))%>%
  st_intersection(us)

clean<-clean%>%
  left_join(clean_pop)

cust_theme<-theme(
  legend.position="bottom",
  legend.spacing.x = unit(0, 'cm'),
  plot.title=element_text(size=40,hjust=0.5,color="#06d6a0",family='rus'),
  plot.subtitle=element_markdown(size=15,hjust=0.5,color="#06d6a0",family='open',lineheight=0.45),
  legend.title=element_markdown(color="black",family="open",size=25,face="bold",margin=margin(0,0,-0.25,0,'cm')),
  legend.text=element_text(color="black",family="open",size=22,margin=margin(-0.25,0,0,0,'cm')),
  plot.caption=element_markdown(color="#06d6a0",family="open",size=15,hjust=0.5),
  plot.margin=margin(0.5,0.5,0.5,0.5,"cm")
)

pal<-viridisLite::viridis(6, alpha = 1, begin = 0, end = 1, direction = 1)

pal_fill<-c(
  "1"=pal[1],
  "2"=pal[2],
  "3"=pal[3],
  "4"=pal[4],
  "5"=pal[5],
  "6"=pal[6]
)

p1<-ggplot(clean,aes(fill=as.factor(sig_cl)))+
  geom_sf(color=alpha("grey80",0.1),lwd=0.05)+
  scale_fill_manual(
    values=pal_fill,
    label=c("10<","100<","200<","300<","400<","≥400")
  )+
  coord_sf(crs="EPSG:5070")+
  guides(fill = guide_legend(
    nrow = 1,title.position="top",title.hjust=0.5,
    label.position = "bottom",keywidth = unit(1.5,"cm"),keyheight = unit(0.25,"cm")
  ))+
  labs(fill="Number of UFO sightings reported since 1998")+
  theme_void()+
  cust_theme

p2<-ggplot(clean,aes(fill=as.factor(pop_cl)))+
  geom_sf(color=alpha("grey80",0.1),lwd=0.05)+
  scale_fill_manual(
    values=pal_fill,
    label=c("10<","100<","200<","300<","400<","≥400")
  )+
  coord_sf(crs="EPSG:5070")+
  guides(fill = guide_legend(
    nrow = 1,title.position="top",title.hjust=0.5,
    label.position = "bottom",keywidth = unit(1.5,"cm"),keyheight = unit(0.25,"cm")
  ))+
  labs(fill="Population density in 2010 (inhabitants per km<sup>2</sup>)")+
  theme_void()+
  cust_theme


p1+p2
