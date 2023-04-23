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
  width = 22.5, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Load data
data <- read_delim('Data/Yield/Data/FAO_wheat.csv',delim=',')
total <- read_delim('Data/Yield/Data/FAO_wheat_world.csv',delim=',')%>%
  mutate(rM=rollmean(Value,5, fill=NA, align="center"))

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

fun_multi <- function(country,ttl){

  pl<-ggplot()+
    geom_segment(
      yax,
      mapping=aes(x=1961,xend=2021,y=y,yend=y),
      color="white",
      lwd=0.15
    )+
    annotate(
      "text",x=1961,y=11,
      label=ttl,color=pal[country],
      hjust=0,family="ral",size=16,
      fontface="bold"
    )+
    geom_line(
      data=clean%>%filter(Area==country),
      aes(
        x=Year,y=Value/10000,
        group=Area,color=cl),
      lwd=0.5,alpha=0.5
    )+
    geom_line(
      data=clean%>%filter(Area==country),
      aes(
        x=Year,y=rM/10000,
        group=Area,color=cl
        #shadowcolour="white"
      ),
      lwd=1
    )+
    scale_y_continuous(limits=c(0,11.5))+
    scale_color_manual(values=pal)+
    scale_fill_manual(values=pal)+
    guides(color="none")+
    theme_void()+
    theme(
      plot.margin = margin(0,1,0,1,"cm"),
      plot.background = element_rect(fill="#1D201F",color=NA)
    )
  
  return(pl)

}

fun_multi("United Kingdom of Great Britain and Northern Ireland","United Kingdom")+
  fun_multi("France","France")+
  fun_multi("China, mainland","China")+
  fun_multi("United States of America","USA")+
  fun_multi("India","India")+
  fun_multi("Mauritania","Mauritania")&
  theme(
    plot.background = element_rect(fill="#1D201F",color=NA)
  )

# World average

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)


ggplot()+
  annotate(
    "text",x=1961,y=11,
    label="World",color="#f77f00",
    hjust=0,family="ral",size=28,
    fontface="bold"
  )+
  geom_segment(
    yax,
    mapping=aes(x=1961,xend=2021,y=y,yend=y),
    color="white",
    lwd=0.5
  )+
  geom_richtext(
    yax,
    mapping=aes(x=1961,y=y+0.25,label=lab),
    hjust=0,color="white",family="fira",size=20,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_richtext(
    xax,
    mapping=aes(x=x,y=0,label=x),
    hjust=0.5,color="white",family="fira",size=20,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_line(
    data=clean,
    aes(
      x=Year,y=Value/10000,
      group=Area,color=cl),
    alpha=0.65,
    lwd=0.1
  )+
  geom_line(
    data=total,
    aes(
      x=Year,y=Value/10000,
      group=Area),
    lwd=0.75,alpha=0.5,
    color="#f77f00"
  )+
  geom_line(
    data=total,
    aes(
      x=Year,y=rM/10000,
      group=Area
    ),
    color="#f77f00",lwd=1.5
  )+
  scale_y_continuous(limits=c(0,11.5))+
  scale_color_manual(values=pal)+
  guides(color="none")+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#1D201F",color=NA)
  )

  