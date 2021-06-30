# Clear space 
rm(list=ls())
gc()

###########################################################################

# Load packages
library(tidyverse)
library(showtext)
library(plyr)
library(lubridate)
library(cowplot)

# Load data
data <- tidytuesdayR::tt_load('2021-06-29')
animal <- data$animal_rescues

# Prep data
clean <- animal%>%
  separate(
    date_time_of_call,
    into=c('date','time'),
    sep = " "
  )%>%
  mutate(
    week_day=lubridate::wday(dmy(date), week_start = 1)
  )%>%
  mutate(
    year_day=lubridate::yday(dmy(date))
  )%>%
  mutate(
    Time_s=period_to_seconds(hm(time))
  )%>%
  separate(
    time,
    into=c('hours','minute'),
    sep = ":"
  )%>%
  filter(
    cal_year!='2021'
  )%>%filter(
    animal_group_parent=="Cat"|
    animal_group_parent=="Dog"|
    animal_group_parent=="Bird"|
    animal_group_parent=="Fox"|
    animal_group_parent=="Horse"
  )

clean$animal_group_parent<-fct_relevel(
  as.factor(clean$animal_group_parent), 
  "Cat","Dog","Horse","Bird","Fox")


tab<-summary(as.factor(clean$week_day))/length(summary(as.factor(clean$cal_year)))
tab2<-cbind.data.frame(
  week_day=names(tab),
  day_name=c(
    "Monday","Tuesday","Wednesday","Thursday",
    "Friday","Saturday","Sunday"),
  count=tab,
  group="all"
)

tab3<-summary(as.factor(clean$hours))/length(summary(as.factor(clean$cal_year)))
tab4<-cbind.data.frame(
  hours=names(tab3),
  hours_name=c(seq(0,23,1)),
  count=tab3,
  group="all"
)
summary(as.factor(clean$cal_year))

tab4_simpl<-rbind.data.frame(
  tab4[1,],
  tab4[4,],
  tab4[7,],
  tab4[10,],
  tab4[13,],
  tab4[16,],
  tab4[19,],
  tab4[22,]
)


# Select font and colors
font_add_google(name = "Paytone One", family = "Paytone One")
showtext_auto()
font_add_google(name = "Quicksand", family = "Quicksand")
showtext_auto()

years<-"Paytone One"
ax<-'Quicksand'
col_years<-'#393d3f'
col_text<-'black'
col_ax<-'#393d3f'

col_pal <- c(
  "#EF959D","#208AAE","#99621E","#f9dc5c","#f26419"
)

# Set theme
them_pl<-theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  plot.margin=unit(c(t=0,r=0,b=0,l=0),"mm"),
  legend.position="none",
  axis.text.y = element_blank(),
  axis.text.x = element_blank(),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.ticks.x = element_blank()
)

# Plots 
pl1<-ggplot(
  clean,
  aes(
    x=as.factor(animal_group_parent),
    y=as.numeric(year_day),
    fill=animal_group_parent,
    color=animal_group_parent)
  )+
  scale_y_continuous(
    breaks=c(seq(15,365,30)),
    labels = c(
      "January","February","March","April",
      "May","June","July","August",
      "September","October","November","December"
    ),
    lim=c(0,450)
  )+
  
  # Seasons
  annotate(
    "rect", 
    xmin = 0.7, xmax = 6.5, ymin = 81, ymax = 172,
    alpha = .1)+
  annotate(
    "rect", 
    xmin = 0.7, xmax = 6.5, ymin = 263, ymax = 354,
    alpha = .1)+
  scale_x_discrete()+
  
  annotate(
    geom='text',
    x=6.2,y=40,label="Winter",
    hjust=0.5,family=ax,color=col_years,alpha=1,
    size=4
  )+
  annotate(
    geom='text',
    x=6.2,y=125,label="Spring",
    hjust=0.5,family=ax,color=col_years,alpha=1,
    size=4
  )+
  annotate(
    geom='text',
    x=6.2,y=215,label="Summer",
    hjust=0.5,family=ax,color=col_years,alpha=1,
    size=4
  )+
  annotate(
    geom='text',
    x=6.2,y=305,label="Fall",
    hjust=0.5,family=ax,color=col_years,alpha=1,
    size=4
  )+
  
  # Commentaries
  annotate(
    "segment", 
    x = 4, xend = 5.5, y = 380, yend = 380, 
    colour = col_years,size=0.2)+
  annotate(
    geom='text',
    x=4.8,y=390,label="Most rescues\nof 'wild' animals in\nspring and summer",
    hjust=0,family=ax,color=col_years,alpha=1,
    size=3.5
  )+
  annotate(
    "segment", 
    x = 1, xend = 3.5, y = 380, yend = 380, 
    colour = col_years)+
  annotate(
    geom='text',
    x=2.3,y=390,label="'Domestic' animals\nrescues more evenly\ndistributed over the year",
    hjust=0,family=ax,color=col_years,alpha=1,
    size=3.5
  )+
  
  scale_color_manual(values=col_pal)+
  scale_fill_manual(values=col_pal)+
  ggdist::stat_halfeye(
    alpha=0.7,
    adjust = .2, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_point(
    alpha=0.2,
    position = position_jitter(
      width=0.15,
    ),
    size=1.2
  )+
  
  # Animal species
  annotate(
    geom='text',
    x=1.4,y=5,label="Cat",
    hjust=0,family=years,color=col_years,alpha=0.4,
    size=7
  )+
  annotate(
    geom='text',
    x=2.4,y=5,label="Dog",
    hjust=0,family=years,color=col_years,alpha=0.4,
    size=7
  )+
  annotate(
    geom='text',
    x=3.4,y=5,label="Horse",
    hjust=0,family=years,color=col_years,alpha=0.4,
    size=7
  )+
  annotate(
    geom='text',
    x=4.4,y=5,label="Bird",
    hjust=0,family=years,color=col_years,alpha=0.4,
    size=7
  )+
  annotate(
    geom='text',
    x=5.4,y=5,label="Fox",
    hjust=0,family=years,color=col_years,alpha=0.4,
    size=7
  )+
  
  them_pl+
  coord_flip(clip="off")

pl1

pl2<-ggplot(
  data=tab2,
  aes(x=week_day,y=count,group=group))+
  scale_x_discrete(
    labels = c(
      "Monday","Tuesday","Wednesday","Thursday",
      "Friday","Saturday","Sunday")
  )+
  scale_y_continuous(
    lim=c(60,90)
  )+
  annotate(
    geom='text',
    x=7,y=72,label="Average number \n of rescues per \nday is higher for\n the week-end",
    hjust=1,vjust=1,family=ax,color=col_years,alpha=1,
    size=3.3
  )+
  annotate(
    "segment", 
    x = 6, xend = 7, y = 74, yend = 74, 
    colour = col_years,size=0.2)+
  geom_line(aes(x=week_day,y=count),color=col_ax)+
  geom_text(
    aes(label=round(count)),family=ax,color=col_ax,
    position = position_nudge(y=2))+
  geom_text(
    aes(label=day_name),family=ax,color='gray60',
    position = position_nudge(y=-1.5),angle=90,hjust=1,size=3.5)+
  them_pl+
  coord_cartesian(clip="off")
pl2

pl3<-ggplot(
  data=tab4,
  aes(x=hours,y=count,group=group))+
  scale_x_discrete(
    labels = tab4$hour_name
  )+
  scale_y_continuous(
    lim=c(-10,50)
  )+
  annotate(
    geom='text',
    x=19,y=19,label="Average number of \nrescues per hour is\nhigher in the afternoon",
    hjust=1,vjust=1,family=ax,color=col_years,alpha=1,
    size=3.3
  )+
  annotate(
    "segment", 
    x = 12, xend = 19, y = 23, yend = 23, 
    colour = col_years,size=0.2)+
  geom_line(aes(x=hours,y=count),color=col_ax)+
  geom_text(
    data=tab4_simpl,
    aes(label=round(count)),family=ax,color=col_ax,
    position = position_nudge(y=4))+
  geom_text(
    data=tab4_simpl,
    aes(label=paste(hours_name,"h",sep="")),family=ax,color='gray60',
    position = position_nudge(y=-1.5),angle=90,hjust=1,size=3.5)+
  them_pl+
  coord_cartesian(clip="off")

pl3


# All together
ggdraw() +
  draw_plot(pl1, x = 0, y = .39, width = 1, height = .4) +
  draw_plot(pl2, x = 0.05, y = 0.02, width = .45, height = .35) +
  draw_plot(pl3, x = 0.5, y = 0.02, width = 0.45, height = 0.35)+
  draw_plot_label(
    label = "Temporal distribution of animal rescues by London fire brigade", 
    size = 18,family=years,hjust=0,color=col_ax,
    x = 0.05, y = 0.95)+
  draw_plot_label(
    label = "Analysis for the period 2009 to 2020 for the five most frequently rescued animal types", 
    size = 12,family=ax,hjust=0,color=col_ax,
    x = 0.05, y = 0.90)+
  draw_plot_label(
    label = c("Year", "Week", "Day"), 
    size = 14,family=years,hjust=0,color=col_ax,
    x = c(0.05, 0.05, 0.5), y = c(0.84, 0.35, 0.35))+
  draw_plot_label(
    label = "Data: London.gov from Data is Plural and @geokaramanis | Plot: @BjnNowak", 
    size = 8,family=ax,color=col_ax,hjust=1,vjust=0,
    x = 0.95, y = 0.02)
