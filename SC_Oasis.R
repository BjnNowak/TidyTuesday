library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)


# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
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


data <- tibble(
  id = seq(1,7,1),
  year = c(1994,1995,1997,2000,2002,2005,2008),
  album = c(
    "Definitely Maybe","(What's the Story)<br>Morning Glory?","Be Here Now",
    "Standing on the<br>Shoulder of Giants","Heathen Chemistry","Don't Believe the Truth",
    "Dig Out your Soul"
  ),
  max_pos = rep(1,7),
  weeks_no1 = c(1,10,5,1,1,1,1),
  sales = c(5000000,22000000,8000000,1294812,1315950,1245843,700000)
)%>%
  mutate(label=glue("{year}<br>**{album}**"))%>%
  mutate(class_sold=case_when(
    sales>10000000~"A",
    sales>1000000~"B",
    sales>100000~"C",
    TRUE~"D"
    
  ))

noel <- tibble(
  id = c(7,1,2),
  year = c(2011,2015,2017),
  album = c(
    "Noel Gallagher's<br>High Flying Birds",
    'Chasing Yesterday',"Who Built The Moon"
  ),
  max_pos = rep(1,3),
  weeks_no1 = c(1,1,1),
  sales = c(867039,302294,300863)
)%>%
  mutate(label=glue("{year}<br>**{album}**"))%>%
  mutate(class_sold=case_when(
    sales>10000000~"A",
    sales>1000000~"B",
    sales>100000~"C",
    TRUE~"D"
    
  ))

liam <- tibble(
  id = c(7.5,1.5),
  year = c(2011,2013),
  album = c(
    "Different Gear<br>Still Speeding",
    'Be'
  ),
  max_pos = c(3,2),
  weeks_no1 = c(0,0),
  sales = c(102306,60000)
)%>%
  mutate(label=glue("{year}<br>**{album}**"))%>%
  mutate(class_sold=case_when(
    sales>10000000~"A",
    sales>1000000~"B",
    sales>100000~"C",
    TRUE~"D"
    
  ))


pal<-c(
  "A"="#0191B4",
  "B"="#97CA6D",
  "C"="#F8D90F",
  "D"="#FE7A15"  
)

ggplot(data)+
  geom_point(aes(x=id,y=1),size=26*2,pch=21,color="black",fill="black")+
  geom_point(aes(x=id,y=1),size=24.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=23*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=21.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=20*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=18.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=17*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=15.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=14*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=12.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=11*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1,fill=class_sold),size=7.5,pch=21,color="black")+
  
  geom_point(aes(x=1,y=1),size=11*2,pch=21,color="white",fill=NA,alpha=1)+
  geom_point(aes(x=2,y=1),size=24.5*2,pch=21,color="white",fill=NA,alpha=1)+
  geom_point(aes(x=3,y=1),size=17*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(data%>%filter(id>3),mapping=aes(x=id,y=1),size=11*2,pch=21,color="white",fill=NA,alpha=1)+
  
  geom_richtext(
    aes(x=id,y=1,label=label),
    size=10,family="ral",lineheight=0.45,
    color="white",
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  
  guides(fill="none")+
  scale_fill_manual(
    values=pal
  )+
  scale_y_continuous(limits=c(-5,5))+
  scale_x_continuous(limits=c(1,8))+
  coord_polar()+
  theme_void()



ggplot(noel)+
  geom_point(aes(x=id,y=1),size=26*2,pch=21,color="black",fill="black")+
  geom_point(aes(x=id,y=1),size=24.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=23*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=21.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=20*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=18.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=17*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=15.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=14*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=12.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=11*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1,fill=class_sold),size=7.5,pch=21,color="black")+
  
  geom_point(aes(x=id,y=1),size=11*2,pch=21,color="white",fill=NA,alpha=1)+
  
  geom_richtext(
    aes(x=id,y=1,label=label),
    size=10,family="ral",lineheight=0.45,
    color="white",
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  
  guides(fill="none")+
  scale_fill_manual(
    values=pal
  )+
  scale_y_continuous(limits=c(-5,5))+
  scale_x_continuous(limits=c(1,8))+
  coord_polar()+
  theme_void()

ggplot(liam)+
  geom_point(aes(x=id,y=1),size=26*2,pch=21,color="black",fill="black")+
  geom_point(aes(x=id,y=1),size=24.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=23*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=21.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=20*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=18.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=17*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=15.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=14*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=12.5*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1),size=11*2,pch=21,color="white",fill=NA,alpha=0.2)+
  geom_point(aes(x=id,y=1,fill=class_sold),size=7.5,pch=21,color="black")+
  

  
  geom_richtext(
    aes(x=id,y=1,label=label),
    size=10,family="ral",lineheight=0.45,
    color="white",
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  
  guides(fill="none")+
  scale_fill_manual(
    values=pal
  )+
  scale_y_continuous(limits=c(-5,5))+
  scale_x_continuous(limits=c(1,8))+
  coord_polar()+
  theme_void()
