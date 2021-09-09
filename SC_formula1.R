
tuesdata <- tidytuesdayR::tt_load(2021, week = 37)

results <- tuesdata$results
pit_stops <- tuesdata$pit_stops
circuits <- tuesdata$circuits
races <- tuesdata$races

results

library(tidyverse)
library(ggtext)
library(glue)
library(camcorder)
library(showtext)

font_add_google("Acme", "acme")
font_add_google("Raleway", "raleway")
font_add_google("Abril Fatface", "abril")
font_add_google("Open Sans", "open")

# Automatically use {showtext} for plots
showtext_auto()

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21*1.618, 
  height = 21, 
  units = "cm", 
  dpi = 300 
)

data <- results%>%
  left_join(pit_stops,by=c('raceId','driverId'))%>%
  left_join(races,by='raceId')%>%
  left_join(circuits,by='circuitId')

test<-data%>%
  # Data on pit stops since 2012 onward
  filter(year>2011)%>%
  filter(positionOrder==1)%>%
  group_by(raceId)%>%
  summarize(
    nbStop = max(stop)
  )%>%
  mutate(
    nbStop=case_when(
      is.na(nbStop)~0,
      TRUE~nbStop
    )
  )%>%
  left_join(races,by='raceId')%>%
  left_join(circuits,by='circuitId')%>%
  mutate(circuitRef=stringr::str_replace_all(circuitRef,"_"," "))%>%
  mutate(circuitRef=stringr::str_to_title(circuitRef))%>%
  mutate(
    fullName = glue("<span style='font-family: acme'>{circuitRef}</span> <i style='font-family:raleway'>({country})</i> ")
  )

all_mean <- mean(test$nbStop)

mean<-test%>%
  group_by(fullName)%>%
  mutate(ct=1)%>%
  summarize(
    meanStop=mean(nbStop),
    minStop=min(nbStop),
    maxStop=max(nbStop),
    nbRaces=sum(ct)
  )%>%
  mutate(hjust=case_when(
    meanStop<all_mean~1,
    TRUE~0
  ))%>%
  mutate(xlabel=case_when(
    meanStop<all_mean~minStop-0.2,
    TRUE~maxStop+0.2
  ))
mean
test<-test%>%
  left_join(mean,by="fullName")

min(mean$nbRaces)
max(mean$nbRaces)

xlab <- cbind.data.frame(
  nbStop=seq(0,5,1),
  xlab=seq(0,5,1)
)
xlab

col_mean<-"#36494E"

ggplot(
  test,
  aes(y=fct_reorder(fullName,meanStop),x=nbStop)
  )+
  geom_point(
    data=mean,aes(y=fct_reorder(fullName,meanStop),x=meanStop),
    color="coral1",size=4,alpha=0)+
  annotate("rect",xmin=0.5,xmax=1.5,ymin=0.5,ymax=31.5,fill="grey90")+
  annotate("rect",xmin=2.5,xmax=3.5,ymin=0.5,ymax=31.5,fill="grey90")+
  annotate("rect",xmin=4.5,xmax=5.5,ymin=0.5,ymax=31.5,fill="grey90")+
 
  annotate(
    "text",x=-0.15,y=33.5,label="Only one pit-stop to win in Monaco",
    size=35,hjust=0,family="abril",color='black',alpha=0.8)+
 
  geom_text(
    data=xlab,aes(x=xlab,label=nbStop),
    y=-1,size=25,family="open",fontface="bold")+
  annotate(
    "text",x=-0.15,y=-3.5,label="Number of pit-stops for winning driver of F1 races",
    size=24,hjust=0,family="raleway")+
  coord_cartesian(clip = "off")+
  geom_segment(
    data=mean,
    aes(x=all_mean,y=fullName,xend=meanStop,yend=fullName,color=nbRaces),
    alpha=1,size=6)+
  annotate(
    "segment",
    x=all_mean,xend=all_mean,y=0.5,yend=31.5,
    color=col_mean,size=1)+
  annotate(
    "text",
    x=2,y=2.5,label="Overall mean:",size=18, hjust=0, family="raleway",
    color=col_mean,fontface='bold'
  )+
  annotate(
    "text",
    x=2,y=1.2,label="1.8 pit-stops",size=18, hjust=0, family="raleway",
    color=col_mean
  )+
  annotate(
    "text",
    x=4.55,y=1.2,label="Data: Ergast",size=16, hjust=0, family="raleway",
    color="black",alpha=0.8
  )+
  annotate(
    "segment",
    x=3.6,xend=4.2,y=9,yend=9,
    color="#f13c77",size=6)+
  annotate(
    "segment",
    x=3.6,xend=3.6,y=8.5,yend=9.5,
    color=col_mean,size=1)+
  annotate(
    geom='segment',
    x=3,xend=5,y=9,yend=9,
    alpha=1,color="dimgrey")+
  annotate(
    geom='point',
    x=3,y=9,size=3
    )+
  annotate(
    geom='point',
    x=5,y=9,size=3
  )+
  annotate(
    geom='text',
    x=5,y=10,size=17,family='open',label='Max'
  )+
  annotate(
    geom='text',
    x=3,y=10,size=17,family='open',label='Min'
  )+
  annotate(
    geom='text',
    x=4.2,y=10,size=17,family='open',label='Mean',color="#f13c77"
  )+
  geom_richtext(
    x=3,y=6.5,label="<span>Races from <b>2012</b><span> to </span><b>2021</b>",
    alpha=1,size=17,family='open',hjust=0,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+ 
  geom_richtext(
    x=3,y=5,label="<span>with</span><b style='color:#f9a470'> 1 </b><span>to</span><b style='color:#bc556f'> 11 </b><span>races per circuit</span>",
    alpha=1,size=17,family='open',hjust=0,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+ 
 
  geom_richtext(
    data=mean,
    aes(x=xlabel,y=fullName,label=fullName,hjust=hjust),
    alpha=0.8,size=18,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+ 
  geom_segment(
    data=mean,
    aes(x=minStop,xend=maxStop,y=fullName,yend=fullName),
    alpha=1,color="dimgrey")+
  geom_point(data=mean,
             aes(x=minStop,y=fullName),size=3)+
  geom_point(data=mean,
             aes(x=maxStop,y=fullName),size=3)+
  scale_x_continuous(limits=c(-1.5,7.5),breaks=seq(0,5,1))+
  
  scale_color_gradient(low="#f9a470",high="#bc556f")+
  guides(color=FALSE)+
  theme_minimal()+
  theme(
    panel.grid.major.x=element_blank(),
    panel.grid.major.y=element_blank(),
    panel.grid.minor.x=element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(c(2.5,1,1,1), "lines")
  )

