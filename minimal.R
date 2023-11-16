#install.packages('osmdata')
library(osmdata)
library(sf)
library(tidyverse)
library(camcorder)
library(scico)
library(patchwork)
library(showtext)

# Set fonts
font_add_google("Abril Fatface","abril")
font_add_google("Concert One","concert")

showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

fun_min <- function(city){

  city_cycle <- getbb(place_name = city) %>%
    opq(timeout = 300) %>%
    add_osm_feature(key = "highway", value = c("cycleway"))%>%
    osmdata_sf()

  city_grid<-st_make_grid(
      city_cycle$osm_lines,
      #square=FALSE,
      n=c(10,10)
    )%>%
    st_as_sf()%>%
    mutate(id=row_number())

  area_grid<-city_grid%>%
    mutate(area=st_area(city_grid))%>%
    st_drop_geometry()

  tile_grid<-tibble(id=seq(1,100,1))%>%
    mutate(y=case_when(
      id<11~1,
      id<21~2,
      id<31~3,
      id<41~4,
      id<51~5,
      id<61~6,
      id<71~7,
      id<81~8,
      id<91~9,
      TRUE~10
    ))%>%
    mutate(x=id%%10)%>%
    mutate(x=case_when(
      x==0~10,
      TRUE~x
    ))

  inter_cycle<-st_intersection(city_grid,city_cycle$osm_line)
  
  inter_cycle<-inter_cycle%>%
    mutate(cycle_length=st_length(inter_cycle))%>%
    group_by(id)%>%
    summarize(lgt_cycle=sum(cycle_length))%>%
    ungroup()%>%
    st_drop_geometry()

  clean_grid<-inter_cycle%>%
    left_join(tile_grid)%>%
    left_join(area_grid)%>%
    mutate(density=as.numeric(lgt_cycle)/(as.numeric(area)/1000000))
  
  return(clean_grid)
  
}

paris<-fun_min("Paris")
amsterdam<-fun_min("Amsterdam")
roma<-fun_min("Roma")
madrid<-fun_min("Madrid")
brussels<-fun_min("Brussels")
helsinki<-fun_min("Helsinki")
oslo<-fun_min("Oslo")
stockholm<-fun_min("Stockholm")
berlin<-fun_min("Berlin")
london<-fun_min("London")
lisbon<-fun_min("Lisbon")
prague<-fun_min("Prague")
budapest<-fun_min("Budapest")
bucarest<-fun_min("Bucarest")
vienna<-fun_min("Vienna")

dublin<-fun_min("Dublin")
bern<-fun_min("Bern")
#sofia<-fun_min("Sofia")
athens<-fun_min("Athens")
bratislava<-fun_min("Bratislava")
chisinau<-fun_min("Chisinau")
copenhagen<-fun_min("Copenhagen")
kiev<-fun_min("Kiev")
Reykjavik<-fun_min("Reykjavik")
warsaw<-fun_min("Warsaw")
tallinn<-fun_min("Tallinn")



new_york<-fun_min("New York")
sidney<-fun_min("Sidney")
ottawa<-fun_min("Ottawa")
auckland<-fun_min("Auckland")
pretoria<-fun_min("Pretoria")
tokyo<-fun_min("Tokyo")
pekin<-fun_min("Pekin")
alger<-fun_min("Alger")


pal<-c(
  '1'='#42047E',
  '2'='#344086',
  '3'='#2A6D8C',
  '4'='#1CA293',
  '5'='#16B896',
  '6'='#07F49E'
)

make_plot<-function(data,name){

  data<-data%>%
    mutate(cl=case_when(
      density<1000~'1',
      density<2000~'2',
      density<3000~'3',
      density<4000~'4',
      density<5000~'5',
      TRUE~'6'
    ))
  
  pl<-ggplot(data,aes(x=x,y=y,fill=cl))+
    geom_tile(color=alpha("white",0.5))+
    coord_fixed()+
    scale_fill_manual(values=pal)+
    guides(fill='none')+
    labs(title=name)+
    theme_void()+
    theme(
      #plot.background = element_rect(fill="#140128",color="#140128"),
      plot.margin = margin(0.5,0.5,0.5,0.5,'cm'),
      plot.title=element_text(hjust=0.5,family='concert',size=100,color="white",margin=margin(0,0,0.2,0,'cm'))
    )
  
  return(pl)
  
}

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 60, 
  height = 60*(5384/18387), 
  units = "cm", 
  dpi = 600 
)

make_plot(amsterdam,'Amsterdam')+
  make_plot(athens,'Athens')+
  make_plot(berlin,'Berlin')+
  make_plot(bern,'Bern')+
  make_plot(bratislava,'Bratislava')+
  make_plot(brussels,'Brussels')+
  make_plot(bucarest,'Bucarest')+
  make_plot(budapest,'Budapest')+
  #make_plot(chisinau,'Chisinau')+
  make_plot(copenhagen,'Copenhagen')+
  make_plot(dublin,'Dublin')+
  make_plot(helsinki,'Helsinki')+
  make_plot(kiev,'Kiev')+
  make_plot(lisbon,'Lisbon')+
  make_plot(london,'London')+
  make_plot(sidney,'Madrid')+
  make_plot(oslo,'Oslo')+
  make_plot(paris,'Paris') +
  make_plot(prague,'Prague')+
  make_plot(Reykjavik,'Reykjavik')+
  make_plot(new_york,'Roma')+
  make_plot(stockholm,'Stockholm')+
  make_plot(tallinn,'Tallinn')+
  make_plot(vienna,'Vienna')+
  make_plot(warsaw,'Warsaw')+
  plot_layout(nrow = 3)&
  theme(
    plot.background = element_rect(fill="#0A0114",color='#0A0114')
  )

