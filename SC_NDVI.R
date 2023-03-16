library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(lubridate)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Jost","jost")
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

s1 <- read_delim('Data/NDVI/data/sentinel_1.csv',delim=',')
s2 <- read_delim('Data/NDVI/data/sentinel_2.csv',delim=',')%>%drop_na()%>%mutate(plot=1)
s2_2 <- read_delim('Data/NDVI/data/s2_2.csv',delim=',')%>%drop_na()%>%mutate(plot=2)
s2_3 <- read_delim('Data/NDVI/data/s2_3.csv',delim=',')%>%drop_na()%>%mutate(plot=3)
s2_4 <- read_delim('Data/NDVI/data/s2_4.csv',delim=',')%>%drop_na()%>%mutate(plot=4)
s2_5 <- read_delim('Data/NDVI/data/s2_5.csv',delim=',')%>%drop_na()%>%mutate(plot=5)
s2_6 <- read_delim('Data/NDVI/data/s2_6.csv',delim=',')%>%drop_na()%>%mutate(plot=6)
s2_7 <- read_delim('Data/NDVI/data/s2_7.csv',delim=',')%>%drop_na()%>%mutate(plot=7)
s2_8 <- read_delim('Data/NDVI/data/s2_8.csv',delim=',')%>%drop_na()%>%mutate(plot=8)
s2_9 <- read_delim('Data/NDVI/data/s2_9.csv',delim=',')%>%drop_na()%>%mutate(plot=9)
s2_10 <- read_delim('Data/NDVI/data/s2_10.csv',delim=',')%>%drop_na()%>%mutate(plot=10)

colnames(s1)<-c("date","value","plot")

colnames(s2)<-c("date","value","plot")
colnames(s2_2)<-c("date","value","plot")
colnames(s2_3)<-c("date","value","plot")
colnames(s2_4)<-c("date","value","plot")
colnames(s2_5)<-c("date","value","plot")
colnames(s2_6)<-c("date","value","plot")
colnames(s2_7)<-c("date","value","plot")
colnames(s2_8)<-c("date","value","plot")
colnames(s2_9)<-c("date","value","plot")
colnames(s2_10)<-c("date","value","plot")


s2_full <- s2%>%
  bind_rows(s2_2)%>%
  bind_rows(s2_3)%>%
  bind_rows(s2_4)%>%
  bind_rows(s2_5)%>%
  bind_rows(s2_6)%>%
  bind_rows(s2_7)%>%
  bind_rows(s2_8)%>%
  bind_rows(s2_9)%>%
  bind_rows(s2_10)

s2_full_wide<-s2_full%>%
  pivot_wider(names_from = plot, values_from = value)
  
start_day <- ymd("2018-01-01")

s1c <- s1%>%
  mutate(
    date=mdy(date),
    days=as.numeric(date)-as.numeric(start_day)+1 # convert to number of days since 1/1/1970
  )

s2c <- s2_full%>%
  mutate(
    date=mdy(date),
    days=as.numeric(date)-as.numeric(start_day)+1 # convert to number of days since 1/1/1970
  )%>%
  mutate(gp_col=case_when(
    plot==10~"A",
    TRUE~"B"
  ))%>%
  filter(value>0.05)

max(s2c$value)
min(s2c$value)
max(s1c$value)
min(s1c$value)

yax_s1 <- tibble(
  x=-10,
  y=c(-25,-20,-15)
)
yax_s2 <- tibble(
  x=-10,
  y=c(0.1,0.5,0.9)
)



xax <- tibble(
  year=seq(2018,2022,1),
  xmin=c(
    as.numeric(date("2018-01-01")),
    as.numeric(date("2018-12-31")),as.numeric(date("2019-12-31")),
    as.numeric(date("2020-12-31")),as.numeric(date("2021-12-31"))
  ),
  xmax=c(
    as.numeric(date("2018-12-31")),as.numeric(date("2019-12-31")),
    as.numeric(date("2020-12-31")),as.numeric(date("2021-12-31")),
    as.numeric(date("2022-12-31"))
  ))%>%
  mutate(
    days_min=xmin-as.numeric(start_day)+1,
    days_max=xmax-as.numeric(start_day)+1,
    mid=days_min+(days_max-days_min)/2
  )

xax

yr_col <- "grey90"
s1_col <- "#FF006D"
s2_col <- "#01BEFE"



tit<-tibble(label="Evolution of <span style='color:#06D6A0'>plant biomass</span> of cultivated fields")
sub<-"Each line represents the evolution of the Normalized Difference Vegetation Index (NDVI) for a given field"
cap<-tibble(label="**Data** Sentinel-2 **| Plot** @BjnNowak")

p2<-ggplot(s2c%>%filter(year(date)<2022))+
  geom_rect(
    data=xax%>%filter(year%in%c(2019,2021)),
    aes(xmin=days_min,xmax=days_max,ymin=0,ymax=1),
    fill=yr_col
  )+
  geom_text(
    data=xax%>%filter(year<2022),
    aes(x=mid,y=0.05,label=year),
    family="ral",size=20
  )+
  geom_line(
    aes(x=days,y=value,group=plot,color=gp_col,alpha=gp_col,lwd=gp_col)
  )+
  annotate("segment",x=30,xend=30,y=0.1,yend=0.9,color='black')+
 
  geom_richtext(
    data=tit,
    aes(x=-70,y=1.15,label=label),
    family="bit",size=30,hjust=0,fontface="bold",
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    data=cap,
    aes(x=1826-365,y=-0.1,label=label),
    family="fira",size=16,hjust=1,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  
  #annotate("text",x=-70,y=1.15,label=tit,family="bit",size=30,hjust=0,fontface="bold")+
  annotate("text",x=-70,y=1.05,label=sub,family="jost",size=20,hjust=0)+
  annotate("text",x=-65,y=0.5,label="NDVI",family="jost",size=20,hjust=0.5,vjust=1,angle=90)+
  geom_point(data=yax_s2,aes(x=30,y=y),size=1,color='black')+
  geom_text(
    data=yax_s2,
    aes(x=-5,y=y,label=y),
    size=20,family="fira",color='black',angle=90)+
  #scale_x_continuous(limits=c(-100,1826))+
  scale_x_continuous(limits=c(-70,1826-365))+
  scale_y_continuous(limits=c(-0.1,1.2))+
  scale_color_manual(values=c("#06D6A0","#74FBD7"))+
  scale_alpha_manual(values=c(1,0.4))+
  scale_linewidth_manual(values=c(1,0.75))+
  guides(color="none",alpha="none",lwd="none")+
  theme_void()+
  theme(
    plot.background=element_rect(fill="white",color=NA)
  )

p2


