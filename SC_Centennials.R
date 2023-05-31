library(tidyverse)
library(camcorder)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 60, 
  height = 60, 
  units = "cm", 
  dpi = 300 
)

centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')



cl<-centenarians%>%
  head(1)

tst<-cl%>%
  mutate(age_rd=floor(age))%>%
  select(rank,name,gender,age_rd,age)

tb<-tibble(
  year=seq(1,tst$age_rd,1)
)

cl_men<-centenarians%>%
  filter(gender=="male")%>%
  head(1)%>%
  mutate(age_rd=floor(age))%>%
  select(rank,name,gender,age_rd,age)

cl_men_alive<-centenarians%>%
  filter(gender=="male")%>%
  filter(still_alive=="alive")%>%
  head(1)%>%
  mutate(age_rd=floor(age))%>%
  select(rank,name,gender,age_rd,age)

cl_women_alive<-centenarians%>%
  filter(gender=="female")%>%
  filter(still_alive=="alive")%>%
  head(1)%>%
  mutate(age_rd=floor(age))%>%
  select(rank,name,gender,age_rd,age)

col_men <- "#335C67"
col_women <- "#E67F0D"

ggplot()+
  geom_rect(
    tb%>%filter(year==max(year)),mapping=aes(ymax=year+15),
    ymin=-10,xmin=0,xmax=1,fill="#540b0e"
  )+
  geom_rect(
    tb%>%filter(year==max(year)),mapping=aes(ymax=year+10),
    ymin=-10,xmin=0,xmax=1,fill="#fff3b0"
  )+
  geom_segment(
    tb,mapping=aes(y=year,yend=year),
    x=0,xend=1,lwd=0.5,color="#ddb892"
  )+
  geom_segment(
    tb%>%filter(year==max(year)),mapping=aes(y=year,yend=year),
    x=0,xend=1,lwd=1,lty="dashed",color=col_women
  )+
  geom_segment(
    cl_men,mapping=aes(y=age_rd,yend=age_rd),
    x=0.5,xend=1,lwd=1,lty="dashed",color=col_men
  )+
  geom_segment(
    cl_men_alive,mapping=aes(y=age_rd,yend=age_rd),
    x=0,xend=1,lwd=1,lty="solid",color=col_men
  )+
  geom_segment(
    cl_women_alive,mapping=aes(y=age_rd,yend=age_rd),
    x=0,xend=0.5,lwd=1,lty="solid",color=col_women
  )+
  scale_y_continuous(limits=c(-10,140))+
  scale_x_continuous(limits=c(0,4))+
  coord_polar()+
  theme_void()
