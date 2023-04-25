library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(zoo)
library(ggshadow)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

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
  width = 15, 
  height = 15, 
  units = "cm", 
  dpi = 300 
)

# Load data
data <- read_delim('Data/Yield/Data/FAO_wheat.csv',delim=',')
org <- read_delim('Data/Yield/Data/wpd_datasets.csv',delim=',')%>%
  mutate(Year=round(Year))%>%
  mutate(Per=Area/26800*100)

# Vector to highlight few countrie
sel<-c(
  "France","China, mainland",
  "United Kingdom of Great Britain and Northern Ireland",
  "India","Mauritania","United States of America")

clean<-data%>%
  # Only keep countries with
  # data from 1961 to 2021
  mutate(ct=1)%>%
  group_by(Area)%>%
  mutate(sm=sum(ct))%>%
  ungroup()%>%
  filter(sm==max(sm))%>%
  # Highlighting few countries
  mutate(cl=case_when(
    Area%in%sel~Area,
    TRUE~"Other"
  ))%>%
  mutate(al=case_when(
    Area%in%sel~1,
    TRUE~0
  ))%>%
  group_by(Area)%>%
  mutate(rM=rollmean(Value,5, fill=NA, align="center"))

pal <- c(
  "United Kingdom of Great Britain and Northern Ireland"="#FFD2FC",
  "China, mainland"="#ef476f",
  "France"="#496DDB",
  "India"="#06d6a0",
  "Mauritania"="#FFD166",
  "United States of America"="#96CDFF",
  "Other"="grey65"
)


xax <- tibble(
  x = seq(1970,2010,20)
)

yax <- tibble(
  y = seq(2,8,2),
  lab = glue::glue("{y} t.ha<sup>-1</sup>")
)

yax_org <- tibble(
  y = seq(2,8,2),
  lab = case_when(
    y == 8 ~glue::glue("{y} % of agricultural area under organic farming"),
    TRUE~glue::glue("{y} %")
))

custom_theme <- theme(
  plot.margin = margin(0,1,0,1,"cm"),
  plot.background = element_rect(fill="black",color="white",size=2)
)

p0<-ggplot()+
  annotate(
    "text",x=1961,y=12,
    label="Wheat yield\nstagnation in France",color="white",
    hjust=0,family="ral",size=14,
    lineheight=0.35,vjust=1,
    fontface="bold"
  )+
  scale_x_continuous(limits=c(1961,2021))+
  scale_y_continuous(limits=c(-0.75,12.5))+
  guides(color="none")+
  theme_void()+
  custom_theme
p0

p1<-ggplot()+
  annotate(
    "text",x=1961,y=12,
    label="- Look, wheat yield is stagnating in France since the 1990s",color="white",
    hjust=0,family="ral",size=14,
    fontface="italic",vjust=1
  )+
  geom_segment(
    yax,
    mapping=aes(x=1961,xend=2021,y=y,yend=y),
    color="white",
    lwd=0.15
  )+
  geom_richtext(
    yax,
    mapping=aes(x=1961,y=y+0.25,label=lab),
    hjust=0,color="white",family="fira",size=12,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_richtext(
    xax,
    mapping=aes(x=x,y=-0.5,label=x),
    hjust=0.5,color="white",family="fira",size=12,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_line(
    data=clean%>%filter(Area=="France"),
    aes(x=Year,y=Value/10000,group=Area),
    lwd=0.5,alpha=0.5,color="white"
  )+
  geom_smooth(
    data=clean%>%filter(Area=="France"),
    aes(x=Year,y=rM/10000,group=Area),
    lwd=1,color="white",se=FALSE
  )+
  scale_y_continuous(limits=c(-0.75,12.5))+
  guides(color="none")+
  theme_void()+
  custom_theme

p1

p2<-ggplot()+
  annotate(
    "text",x=1961,y=12,
    label="- Did we reach the maximum yield for wheat?\n- Perhaps, but yields may be higher in nearby countries",color="white",
    lineheight=0.35,
    hjust=0,family="ral",size=14,
    fontface="italic",vjust=1
  )+
  geom_segment(
    yax,
    mapping=aes(x=1961,xend=2021,y=y,yend=y),
    color="white",
    lwd=0.15
  )+
  geom_richtext(
    yax,
    mapping=aes(x=1961,y=y+0.25,label=lab),
    hjust=0,color="white",family="fira",size=12,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_richtext(
    xax,
    mapping=aes(x=x,y=-0.5,label=x),
    hjust=0.5,color="white",family="fira",size=12,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_smooth(
    data=clean%>%filter(Area=="France"),
    aes(x=Year,y=rM/10000,group=Area),
    lwd=1,color="white",se=FALSE
  )+
  geom_smooth(
    data=clean%>%filter(Area=="United Kingdom of Great Britain and Northern Ireland"),
    aes(x=Year,y=rM/10000,group=Area),
    lwd=1,color="white",se=FALSE,lty="dashed"
  )+
  scale_y_continuous(limits=c(-0.75,12.5))+
  guides(color="none")+
  theme_void()+
  custom_theme

p2

p3<-ggplot()+
  annotate(
    "text",x=1961,y=12,
    label="- So, is it because of lower yields in organic farming?\n- It is unlikely, the increase in organic areas comes later",color="white",
    hjust=0,family="ral",size=14,
    lineheight=0.35,vjust=1,
    fontface="italic"
  )+
  geom_segment(
    yax_org,
    mapping=aes(x=1961,xend=2021,y=y,yend=y),
    color="white",
    lwd=0.15
  )+
  geom_richtext(
    yax_org,
    mapping=aes(x=1961,y=y+0.25,label=lab),
    hjust=0,color="white",family="fira",size=12,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_richtext(
    xax,
    mapping=aes(x=x,y=-0.5,label=x),
    hjust=0.5,color="white",family="fira",size=12,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_segment(
    data=org,
    aes(x=Year,xend=Year,y=0,yend=Per),
    lwd=1,alpha=1,color="white"
  )+
  scale_x_continuous(limits=c(1961,2021))+
  scale_y_continuous(limits=c(-0.75,12.5))+
  guides(color="none")+
  theme_void()+
  custom_theme
p3

p4 <- ggplot()+
  annotate(
    "text",x=1961,y=12,
    label="- But do we know the reasons for this stagnation?\n- Not exactly, but political decisions may have played a role",color="white",
    hjust=0,family="ral",size=14,
    lineheight=0.35,vjust=1,
    fontface="italic"
  )+
  geom_segment(
    yax,
    mapping=aes(x=1961,xend=2021,y=y,yend=y),
    color="white",
    lwd=0.15
  )+
  geom_richtext(
    yax,
    mapping=aes(x=1961,y=y+0.25,label=lab),
    hjust=0,color="white",family="fira",size=12,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    xax,
    mapping=aes(x=x,y=-0.5,label=x),
    hjust=0.5,color="white",family="fira",size=12,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_line(
    data=clean%>%filter(Area=="France"),
    aes(x=Year,y=Value/10000,group=Area),
    lwd=0.5,alpha=0.5,color="white"
  )+
  geom_smooth(
    data=clean%>%filter(Area=="France"),
    aes(x=Year,y=rM/10000,group=Area),
    lwd=1,color="white",se=FALSE
  )+
  annotate(
    geom="point",
    x=1992,y=65393.4/10000,
    color="white",size=6,pch=18
  )+
  scale_y_continuous(limits=c(-0.75,12.5))+
  guides(color="none")+
  theme_void()+
  custom_theme

p4

p5 <- ggplot()+
  annotate(
    "text",x=1961,y=12,
    label="- Certain extreme climatic events also explain this situation",color="white",
    hjust=0,family="ral",size=14,
    lineheight=0.35,vjust=1,
    fontface="italic"
  )+
  geom_segment(
    yax,
    mapping=aes(x=1961,xend=2021,y=y,yend=y),
    color="white",
    lwd=0.15
  )+
  geom_richtext(
    yax,
    mapping=aes(x=1961,y=y+0.25,label=lab),
    hjust=0,color="white",family="fira",size=12,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    xax,
    mapping=aes(x=x,y=-0.5,label=x),
    hjust=0.5,color="white",family="fira",size=12,
    fill = NA, label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_line(
    data=clean%>%filter(Area=="France"),
    aes(x=Year,y=Value/10000,group=Area),
    lwd=0.5,alpha=0.5,color="white"
  )+
  geom_smooth(
    data=clean%>%filter(Area=="France"),
    aes(x=Year,y=rM/10000,group=Area),
    lwd=1,color="white",se=FALSE
  )+
  annotate(
    geom="point",
    x=2003,y=62500/10000,
    color="white",size=6,pch=18
  )+
  annotate(
    geom="point",
    x=2016,y=	52896/10000,
    color="white",size=6,pch=18
  )+
  scale_y_continuous(limits=c(-0.75,12.5))+
  guides(color="none")+
  theme_void()+
  custom_theme

p5

p6 <- ggplot()+
  annotate(
    "text",x=1961,y=12,
    label="- Should we try to increase the yields again?\n- This could be rather inefficient",color="white",
    hjust=0,family="ral",size=14,
    lineheight=0.35,vjust=1,
    fontface="italic"
  )+
  geom_segment(
    yax,
    mapping=aes(x=1961,xend=2021,y=y,yend=y),
    color="white",
    lwd=0.15
  )+
  geom_richtext(
    yax,
    mapping=aes(x=1961,y=y+0.25,label=lab),
    hjust=0,color="white",family="fira",size=12,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    xax,
    mapping=aes(x=x,y=-0.5,label=x),
    hjust=0.5,color="white",family="fira",size=12,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_line(
    data=clean%>%filter(Area=="France"),
    aes(x=Year,y=Value/10000,group=Area),
    lwd=0.5,alpha=0.5,color="white"
  )+
  geom_smooth(
    data=clean%>%filter(Area=="France"),
    aes(x=Year,y=rM/10000,group=Area),
    lwd=1,color="white",se=FALSE
  )+
  annotate(
    geom="point",
    x=2003,y=62500/10000,
    color="white",size=6,pch=18
  )+
  annotate(
    geom="point",
    x=2016,y=	52896/10000,
    color="white",size=6,pch=18
  )+
  scale_y_continuous(limits=c(-0.75,12.5))+
  guides(color="none")+
  theme_void()+
  custom_theme

p7 <- ggplot()+
  annotate(
    "text",x=1961,y=12,
    label="- Thus increasing yields in countries with lower productivity\nwould be much more efficient to help feed the world",color="white",
    lineheight=0.35,
    hjust=0,family="ral",size=14,
    fontface="italic",vjust=1
  )+
  geom_segment(
    yax,
    mapping=aes(x=1961,xend=2021,y=y,yend=y),
    color="white",
    lwd=0.15
  )+
  geom_richtext(
    yax,
    mapping=aes(x=1961,y=y+0.25,label=lab),
    hjust=0,color="white",family="fira",size=12,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    xax,
    mapping=aes(x=x,y=-0.5,label=x),
    hjust=0.5,color="white",family="fira",size=12,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_smooth(
    data=clean%>%filter(Area=="France"),
    aes(x=Year,y=rM/10000,group=Area),
    lwd=1,color="white",se=FALSE
  )+
  geom_smooth(
    data=clean%>%filter(Area=="Mauritania"),
    aes(x=Year,y=rM/10000,group=Area),
    lwd=1,color="white",se=FALSE,lty="dashed"
  )+
  scale_y_continuous(limits=c(-0.75,12.5))+
  guides(color="none")+
  theme_void()+
  custom_theme

p7


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 30, 
  height = 60, 
  units = "cm", 
  dpi = 300 
)

p0+p1+p2+p3+p4+p5+p6+p7+
  plot_layout(nrow=4,ncol=2)
