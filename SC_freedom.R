
library(tidyverse)
library(camcorder)
library(showtext)
library(patchwork)
library(ggtext)

# Set plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10*1.618, 
  units = "cm", 
  dpi = 300 
)

# Load fonts 
font_add_google("Barlow Condensed","barlow")
font_add_google("Archivo Black","archivo")
font_add_google("Archivo Narrow","arch")
font_add_google("Kreon","kreon")
# Automatically use {showtext} for plots
showtext_auto()


# Colors
pal <- c(
  'Americas'='#FFB30F',
  'Europe'='#264653',
  'Oceania'='#2A9D8F',
  'Asia'="#A23E48",
  'Africa'='#E76F51'
)

# Load data
freedom<-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

# Prep data
reg<-freedom%>%
  group_by(Region_Name,year)%>%
  summarise(
    CL = mean(na.omit(CL)),
    PR = mean(na.omit(PR))
  )

# Make plot
cap <- tibble(
  x=2021.5,y=0.7,
  label="**Data:** Freedom House  **| Plot:** @BjnNowak"
)

# Civil liberties
cl <- ggplot(reg,aes(x=year,y=CL,color=Region_Name))+
  geom_jitter(data=freedom,alpha=0.2,size=0.8)+
  geom_line(size=1.5)+
  annotate(
    'text',label='civil liberties',x=1992,y=4,hjust=0.5,angle=90,
    family='archivo',size=25,alpha=0.5)+
  annotate(
    'text',label='high',x=1995.5,y=1,hjust=0,angle=0,
    family='arch',size=15,alpha=0.8,color='black')+
  annotate(
    'text',label='low',x=1995.5,y=7,hjust=0,angle=0,
    family='arch',size=15,alpha=0.8,color='black')+
  annotate(
    'text',label='dots show the score per country and per year\nlines indicate the mean per continent',
    x=2020,y=7,hjust=1,angle=0,lineheight=0.35,fontface='italic',
    family='arch',size=7,alpha=0.8,color='black')+
  geom_richtext(
    data=cap,
    aes(x=x, y=y, label = label),
    col="grey20",
    size=7,family="arch",angle=270,hjust=0,vjust=1,lineheight=0.40,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  scale_color_manual(values=pal)+
  scale_y_reverse()+
  scale_x_continuous(
    limits=c(1990,2022)
  )+
  guides(color='none')+
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin = margin(0,0,0,0),
    axis.title=element_blank(),
    axis.text=element_blank(),
  )

# Political rights
pr <- ggplot(reg,aes(x=year,y=PR,color=Region_Name))+
  geom_jitter(data=freedom,alpha=0.2,size=0.8)+
  geom_line(size=1.5)+
  annotate(
    'text',label='political rights',x=1992,y=4,hjust=0.5,angle=90,
    family='archivo',size=25,alpha=0.5)+
  annotate(
    'text',label='Europe',x=2008,y=1.4,hjust=0.5,angle=0,
    family='archivo',size=10,alpha=0.9,color='#264653')+
  annotate(
    'text',label='Oceania',x=2017,y=1.4,hjust=0.5,angle=0,
    family='archivo',size=10,alpha=0.9,color='#2A9D8F')+
  annotate(
    'text',label='Americas',x=2002,y=2.6,hjust=0.5,angle=0,
    family='archivo',size=10,alpha=0.9,color='#FFB30F')+
  annotate(
    'text',label='Africa',x=2016,y=4.3,hjust=0.5,angle=0,
    family='archivo',size=10,alpha=0.9,color='#E76F51')+
  annotate(
    'text',label='Asia',x=2006,y=5.2,hjust=0.5,angle=0,
    family='archivo',size=10,alpha=0.9,color='#A23E48')+
  scale_y_reverse()+
  scale_color_manual(values=pal)+
  scale_x_continuous(
    limits=c(1990,2022),breaks=c(2000,2010,2020)
  )+
  guides(color='none')+
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin = margin(0,0,8,0),
    axis.title=element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_text(family='arch',size=35)
  )
  


cl/pr


