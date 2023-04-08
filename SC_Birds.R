
library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Jost","jo")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 32, 
  height = 16.6, 
  units = "cm", 
  dpi = 300 
)

# Article sur le programme STOC :
# https://www.nationalgeographic.com/science/article/farmland-birds-declines-agriculture-environnment-science
# Rapport du prog STOC :
# https://www.vigienature.fr/sites/vigienature/files/atoms/files/syntheseoiseauxcommuns2020_final.pdf


data <- read_delim('Data/Birds/data/stoc.csv',delim=';')

clean<-data%>%
  mutate(year=round(year))%>%
  mutate(trend=case_when(
    year==1989~1,
    TRUE~trend
  ))%>%
  mutate(lw=case_when(
    type=="farm"~2,
    TRUE~1
  ))

ytbl<-tibble(
  y=seq(0.6,1.1,0.1),
  per=y*100-100
)%>%
  filter(y!=1)%>%
  mutate(lab=case_when(
    y==1.1~glue::glue("+{per} %"),
    TRUE~glue::glue("{per} %")
  ))

xtbl<-tibble(
  x=seq(1995,2015,10)
)

down<-1

pal<-c(
  "urban"=alpha("#FF595E",down),
  "farm"=alpha("#FFCA3A",1),
  "woodland"=alpha("#8AC926",down),
  "generalist"=alpha("#1982C4",down)
)

low<-1
high<-2

lw<-c(
  "urban"=low,
  "farm"=high,
  "woodland"=low,
  "generalist"=low
)

ax <- "white"

fun_pl <- function(nm){
  
  pl<-ggplot(clean%>%filter(type==nm),aes(x=year,y=trend,color=type,fill=type))+
    #geom_line()+
    #geom_point(pch=21,color="white",size=4)+
    geom_ribbon(aes(x=year,ymin=1,ymax=trend))+
    geom_segment(
      data=ytbl,aes(x=1989,xend=2017,y=y,yend=y),
      alpha=0.1,color=ax,
      inherit.aes=FALSE
    )+
    geom_text(
      data=xtbl,aes(x=x,y=1+0.015,label=x),
      family="jo",size=12,hjust=0.5,alpha=0.5,
      color=ax,
      inherit.aes=FALSE
    )+
    annotate(geom="segment",x=1989,xend=2017,y=1,yend=1,linewidth=0.8,color=ax)+
    #geom_smooth(color=NA)+
    scale_fill_manual(values=pal)+
    scale_color_manual(values=pal)+
    scale_linewidth(range=c(0.25,1.5))+
    scale_x_continuous(limits=c(1989,2017))+
    scale_y_continuous(limits=c(0.45,1.25))+
    guides(fill="none",color="none",linewidth="none")+
    theme_void()
  
  
  return(pl)
  
}

fun_pl_ylab <- function(nm){

pl<-ggplot(clean%>%filter(type==nm),aes(x=year,y=trend,color=type,fill=type))+
  geom_ribbon(aes(x=year,ymin=1,ymax=trend))+
  geom_segment(
    data=ytbl,aes(x=1989,xend=2017,y=y,yend=y),
    alpha=0.1,color=ax,
    inherit.aes=FALSE
  )+
  geom_text(
    data=xtbl,aes(x=x,y=1+0.015,label=x),
    family="jo",size=12,hjust=0.5,alpha=0.5,
    color=ax,
    inherit.aes=FALSE
  )+
  geom_text(
    data=ytbl,aes(x=1989,y=y+0.01,label=y),
    family="jo",size=12,hjust=0,alpha=0.5,
    color=ax,
    inherit.aes=FALSE
  )+
  annotate(geom="segment",x=1989,xend=2017,y=1,yend=1,linewidth=0.8,color=ax)+
  #geom_smooth(color=NA)+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=pal)+
  scale_linewidth(range=c(0.25,1.5))+
  scale_x_continuous(limits=c(1989,2017))+
  scale_y_continuous(limits=c(0.45,1.25))+
  guides(fill="none",color="none",linewidth="none")+
  theme_void()


return(pl)

}



fun_pl_ylab("generalist")+fun_pl(nm="woodland")+fun_pl(nm="urban")+fun_pl(nm="farm")+
  plot_layout(nrow=1)+
  plot_annotation(
    theme = theme(
      plot.background = element_rect(
      color  = NA, fill = NA 
      #fill = "#1D201F"
  )))


