library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(ggforce)
library(glue)
library(cartogram)
library(sf)
library(units)

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
  width = 32, 
  height = 16.6, 
  units = "cm", 
  dpi = 300 
)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load data
###########
# Data about cherry blossom in Kyoto:
# http://atmenv.envi.osakafu-u.ac.jp/aono/kyophenotemp4/
# paper:
# https://iopscience.iop.org/article/10.1088/1748-9326/ac6bb4

# Analyse here :
# https://fosstodon.org/@deivudesu@mastodon.social/110111210854666032

data <- read_delim('Data/Cherry/KyotoFullFlower7.csv',delim=';')

# Define time period
clean <- data%>%
  mutate(per=case_when(
    AD<900~875,
    AD<950~925,
    AD<1000~975,
    AD<1050~1025,
    AD<1100~1075,
    AD<1150~1125,
    AD<1200~1175,
    AD<1250~1225,
    AD<1300~1275,
    AD<1350~1325,
    AD<1400~1375,
    AD<1450~1425,
    AD<1500~1475,
    AD<1550~1525,
    AD<1600~1575,
    AD<1650~1625,
    AD<1700~1675,
    AD<1750~1725,
    AD<1800~1775,
    AD<1850~1825,
    AD<1900~1875,
    AD<1950~1925,
    AD<2000~1975,
    AD<2020~2010
  ))

res<-clean%>%
  group_by(per)%>%
  summarize(
    mn = mean(na.omit(flow_date_doy)),
    sd = sd(na.omit(flow_date_doy))
  )

xax <- tibble(
  yr=seq(1000,2000,200)
)

yax <- tibble(
  doy = c(91,106,121),
  lab = c("April 1st","April 15th","April 30th")
)

col_bl <- "#e3dce8"
col_bl2 <- col_bl

ggplot()+
  geom_point(
    data,
    mapping=aes(x=AD,y=flow_date_doy),
    size=0.3,color="grey40")+
  geom_segment(
    data=yax,
    mapping=aes(x=812,xend=2015,y=doy,yend=doy),
    alpha=0.15
  )+
  geom_text(
    data=yax,
    mapping=aes(x=812,y=doy+1,label=lab),
    family="ral",size=13,hjust=0
  )+
  geom_line(
    res,
    mapping=aes(x=per,y=mn),
    linewidth=1,color="#ff006e")+
  geom_point(
    res,
    mapping=aes(x=per,y=mn+0.5),
    pch=21,fill=col_bl,size=3.75,color=col_bl2
  )+
  geom_point(
    res,
    mapping=aes(x=per+6,y=mn+0.1),
    pch=21,fill=col_bl,size=3.75,color=col_bl2
  )+
  geom_point(
    res,
    mapping=aes(x=per+4,y=mn-0.3),
    pch=21,fill=col_bl,size=3.75,color=col_bl2
  )+
  geom_point(
    res,
    mapping=aes(x=per-4,y=mn-0.3),
    pch=21,fill=col_bl,size=3.75,color=col_bl2
  )+
  geom_point(
    res,
    mapping=aes(x=per-6,y=mn+0.1),
    pch=21,fill=col_bl,size=3.75,color=col_bl2
  )+
  geom_point(
    res,
    mapping=aes(x=per,y=mn),
    size=2,color="#ff006e"
  )+
  geom_point(
    res,
    mapping=aes(x=per,y=mn),
    size=2.5,color="#ffb703",pch=8,alpha=0.45)+
  geom_segment(
    res,
    mapping=aes(x=per,xend=per,y=mn-sd,yend=mn+sd),
    lwd=0.5,color="#ffb703",alpha=0.5
  )+
  geom_text(
    xax,
    mapping=aes(x=yr,y=85,label=yr),
    family="ral",size=16
  )+
  scale_x_continuous(limits=c(800,2030))+
  scale_y_continuous(limits=c(80,124))+
  theme_void()
