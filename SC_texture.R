library(ggtern)
library(tidyverse)
library(sf)
library(patchwork)
library(ggtext)
library(showtext)
library(camcorder)


# Load hexagon map
hex <- read_sf("maps/fr_more_poly_wgs84.shp")%>%
  mutate(id=as.numeric(FID))%>%
  st_transform(2154)

# Load soil features
#soil <- read_delim('data/soil_per_hex.csv',delim=",")%>%
soil <- read_delim('data/persist_soil_extend.csv',delim=",")%>%
  mutate(
    clay=clay_5_15cm/10,
    silt=silt_5_15cm/10,
    sand=sand_5_15cm/10,
    x=clay,
    y=silt,
    z=sand
  )%>%
  select(id=id_str,clay,silt,sand,x,y,z)%>%
  mutate(
    clay2=10+clay*0.8,
    silt2=10+silt*0.8,
    sand2=sand*0.8
  )%>%
  mutate(clay_cl=case_when(
    clay<(100*1/3)*0.8+10~"A",
    clay<(100*2/3)*0.8+10~"B",
    TRUE~"C"
  ))%>%
  mutate(silt_cl=case_when(
    silt<(100*1/3)*0.8+10.~"A",
    silt<(100*2/3)*0.8+10~"B",
    TRUE~"C"
  ))%>%
  mutate(sand_cl=case_when(
    sand<(100*1/3)*0.8~"A",
    sand<(100*2/3)*0.8~"B",
    TRUE~"C"
  ))%>%
  mutate(cl=glue::glue("{clay_cl}{silt_cl}{sand_cl}"))%>%
  mutate(cl2=case_when(
    cl=="CAA"~"p1",
    cl=="BAA"~"p2",
    cl=="BAB"~"p3",
    cl=="AAB"~"p4",
    cl=="AAC"~"p5",
    cl=="BBA"~"p6",
    cl=="ABA"~"p7",
    cl=="ABB"~"p8",
    cl=="ACA"~"p9"
  ))
 
mp<-hex%>%left_join(soil)


pal_fill2<-c(
  "p1"="#01a0c6",
  "p2"="#3EBAD4",
  "p3"="#8ACCC5",
  "p4"="#feffab",
  "p5"="#feff00",
  "p6"="#6DACCF",
  "p7"="#f882be",
  "p8"="#ffb4a9",
  "p9"="#f21c8d"
)

ggplot(mp,aes(fill=cl2))+
  geom_sf(color=alpha("white",0))+
  scale_fill_manual(values=pal_fill2)+
  guides(fill="none")+
  coord_sf(crs=2154)+
  theme_void()+
  theme(plot.background = element_rect(fill=NA,color=NA))

ggsave(
  "soil_map.png",
  width=30,
  height=30,
  units="cm"
  
)

tern1<-ggtern(mp,aes(x=clay,y=silt,z=sand,color=cl2))+
  geom_point()+
  scale_color_manual(values=pal_fill2)+
  scale_T_continuous(limits=c(0.1,0.9))+ 
  scale_L_continuous(limits=c(0.10,0.90))+ 
  scale_R_continuous(limits=c(0.0,0.8))+
  guides(color="none")+
  theme_void()+
  theme_nolabels()+
  theme_custom(
    tern.plot.background = NA,
    tern.panel.background ="#EAEAEA",
    col.grid.minor = NA,
    col.L=NA,
    col.T=NA,
    col.R=NA
  )
tern1  
ggsave(
  "t1.png",
  width=15,
  height=15,
  units="cm"

)
  


# Make the legend

pol1 <- tibble(
  x=c(100*2/3,100*2/3,100),
  y=c(100*1/3,0,0),
  z=c(0,1/3*100,0),
  col="p1"
)

pol2 <- tibble(
  x=c(100*2/3,100*1/3,2/3*100),
  y=c(100*1/3,100*1/3,0),
  z=c(0,100*1/3,1/3*100),
  col="p2"
)

pol3 <- tibble(
  x=c(100*2/3,100*1/3,1/3*100),
  y=c(0,100*1/3,0),
  z=c(100*1/3,100*1/3,100*2/3),
  col="p3"
)

pol4 <- tibble(
  x=c(100*1/3,100*1/3,0),
  y=c(0,100*1/3,1/3*100),
  z=c(100*2/3,100*1/3,100*2/3),
  col="p4"
)

pol5<-tibble(
  x=c(0,1/3*100,0),
  y=c(0,0,1/3*100),
  z=c(100,2/3*100,2/3*100),
  col="p5"
)

pol6 <- tibble(
  x=c(100*2/3,100*1/3,100*1/3),
  y=c(100*1/3,2/3*100,100*1/3),
  z=c(0,0,100*1/3),
  col="p6"
)

pol7 <- tibble(
  x=c(100*1/3,0,100*1/3),
  y=c(2/3*100,2/3*100,100*1/3),
  z=c(0,100*1/3,100*1/3),
  col="p7"
)

pol8 <- tibble(
  x=c(0,0,100*1/3),
  y=c(2/3*100,1/3*100,100*1/3),
  z=c(1/3*100,100*2/3,100*1/3),
  col="p8"
)


pol9 <- tibble(
  x=c(0,1/3*100,0),
  y=c(2/3*100,2/3*100,100),
  z=c(1/3*100,0,0),
  col="p9"
)



multipol<-pol1%>%
  bind_rows(pol2)%>%
  bind_rows(pol3)%>%
  bind_rows(pol4)%>%
  bind_rows(pol5)%>%
  bind_rows(pol6)%>%
  bind_rows(pol7)%>%
  bind_rows(pol8)%>%
  bind_rows(pol9)%>%
  mutate(
    x=10+x*0.8,
    y=10+y*0.8,
    z=z*0.8
  )


t2<-ggtern(multipol,aes(x=x,y=y,z=z,fill=col))+
  geom_polygon(color=NA)+
  #geom_point(mp,mapping=aes(x=clay,y=silt,z=sand,color=cl))+
  scale_fill_manual(values=pal_fill2)+
  scale_T_continuous(limits=c(.1,.9))+ 
  scale_L_continuous(limits=c(.1,.9))+ 
  scale_R_continuous(limits=c(0,.8))+
  guides(fill="none")
  theme_void()+
  theme_nolabels()+
  theme_custom(
    tern.plot.background = NA,
    tern.panel.background = NA,
    col.grid.minor = NA,
    col.L=NA,
    col.T=NA,
    col.R=NA
  )

ggsave(
  "t2.png",
  width=15,
  height=15,
  units="cm"
  
)


