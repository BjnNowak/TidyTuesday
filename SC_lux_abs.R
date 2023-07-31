library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)


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
  width = 31, 
  height = 16.6, 
  units = "cm", 
  dpi = 300 
)

# Data from:
# https://www.sciencedirect.com/science/article/pii/S2352340919302264#appsec1
# (Appendix A.)

data<-read_tsv('Data/NDVI/data/lux_abs.txt')

sub<-data[,1:3]
colnames(sub)<-c('lambda','ca','cb')


xlab<-tibble(
  x=seq(400,700,100),
  lab=glue::glue("{x} nm")
)

col_pal <- tibble(
  xmin=c(
    380,420,460,500,
    520,540,565,
    590,620,650),
  xmax=c(
    420,460,500,520,
    540,565,590,
    620,650,770),
  col=c(
    "purple","indigo","blue","cyan",
    "spring green","green","yellow",
    "orange","pantone","red")
)

pal<-c(
  "purple"="#712CA1",
  "indigo"="#3C3DC1",
  "blue"="#064EE0",
  "cyan"="#00EFE4",
  "spring green"="#19EE8F",
  "green"="#31ED3A",
  "yellow"="#FFFF00",
  "orange"="#FFC100",
  "pantone"="#FF6100",
  "red"="#FF0000"
)

cust_arrow<-arrow(angle = 25, length = unit(0.5, "cm"),
                  ends = "last", type = "closed")

col_arrow="black"

cap <- tibble(x=320,y=0.04,lab="**data** Clementson and Wojtasiewicz (2019) **| plot** Benjamin Nowak")

ggplot(data=sub,aes(x=lambda))+
  # Add title and caption
  annotate(
    "text",x=320,y=0.045,label="Chlorophyll absorption spectrum",
    size=28,family="ral",hjust=0,fontface='bold'
  )+
  geom_richtext(
    data=cap,
    mapping=aes(x=x,y=y,label=lab),
    hjust=0,size=18,family="fira",color="grey20",
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  # add axes 
  annotate('segment',x=340,xend=810,y=-0.002,yend=-0.002,arrow=cust_arrow,col=col_arrow)+
  annotate('segment',x=340,xend=340,y=-0.002,yend=0.035,arrow=cust_arrow,col=col_arrow)+
  annotate(
    "text",x=330,y=0.0165,label="absorbance",
    size=20,family="fira",hjust=0.5,angle=90
  )+
  annotate(
    "text",x=575,y=-0.009,label="wavelength",
    size=20,family="fira",hjust=0.5
  )+
  geom_rect(
    data=col_pal,
    aes(xmin=xmin,xmax=xmax,ymax=-0.0025,ymin=-0.0045,fill=col),
    inherit.aes = F
  )+
  geom_text(
    data=xlab,
    aes(x=x,y=-0.006,label=lab),
    size=19,family='fira',alpha=0.5,
    inherit.aes=F
  )+
  # Label pigments names
  annotate(
    "text",x=470,y=0.03,label="chlorophyll b",
    size=16,family="fira",hjust=0,angle=0,fontface='italic'
  )+
  annotate(
    "text",x=676,y=0.015,label="chlorophyll a",
    size=16,family="fira",hjust=0,angle=0
  )+
  # Add lines
  geom_line(mapping=aes(y=ca))+
  geom_line(mapping=aes(y=cb),lty='dashed')+
  scale_fill_manual(values=pal)+
  scale_x_continuous(limits=c(320,810))+
  scale_y_continuous(limits=c(-0.009,0.05))+
  guides(fill='none')+
  theme_void()+
  theme(plot.background = element_rect(fill="white",color=NA))
