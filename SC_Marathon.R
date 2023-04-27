library(tidyverse)
library(lubridate)
library(camcorder)
library(showtext)
library(ggtext)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Calistoga","cal")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 14.3, 
  height = 30.9, 
  units = "cm", 
  dpi = 300 
)

winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')

women<-winners%>%
  filter(Category%in%c("Women"))%>%
  select(year=Year,time_women=Time)

men<-winners%>%
  filter(Category%in%c("Men"))%>%
  select(year=Year,time_men=Time)

clean<-men%>%
  left_join(women)%>%
  mutate(
    tmen=as.numeric(time_men),
    twomen=as.numeric(time_women)
  )%>%
  select(year,tmen,twomen)%>%
  add_row(year=2023,tmen=2*3600+60+25,twomen=2*3600+18*60+33)%>%
  mutate(
    diff=twomen-tmen,
    off=tmen+(twomen-tmen)/2
  )



pal<-c(
  "#FFECD1",
  "#FFDA74",
  "#FFCA3A",
  "#FFAB2D",
  "#FF8B1F"
)

col_ax <- "grey80"

ggplot(clean)+
  annotate("segment",x=7200,xend=7200,y=-2023.5,yend=-1980,color=col_ax,size=0.1)+
  annotate("segment",x=7800,xend=7800,y=-2023.5,yend=-1980,color=col_ax,size=0.1)+
  annotate("segment",x=8400,xend=8400,y=-2023.5,yend=-1980,color=col_ax,size=0.1)+
  annotate("segment",x=9000,xend=9000,y=-2023.5,yend=-1980,color=col_ax,size=0.1)+
  geom_segment(
    aes(x=tmen,xend=twomen,y=-year,yend=-year,color=-diff),
    size=7
  )+
  geom_text(
    aes(x=off,y=-year,label=year),size=10,family="ral"
  )+
  scale_x_continuous(limits=c(7100,9500))+
  scale_y_continuous(limits=c(-2024,-1979))+
  binned_scale(
    aesthetics = "color",
    scale_name = "stepsn", 
    palette = function(x) pal,
    breaks = c(-1200,-960,-720,-480),
    labels = c("20","16","12","8"),
    na.value = NA,
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "horizontal",
      barheight = unit(4, units = "mm") , 
      barwidth = unit(25, units = "mm"),
      title.position = 'top',
      label.position = "bottom"
  ))+
  theme_void()+
  guides(
    title="Mind the gap",
    subtitle="Evolution of the time gap between women and men winners of the London Marathon from 1981 to 2022",
  )+
  theme(
    legend.position = c(0.875,0.825),
    legend.text = element_text(family="fira",color="white",size=26),
    legend.title=element_blank(),
    panel.background = element_rect(fill="#1D201F",color=NA)
  )
  


