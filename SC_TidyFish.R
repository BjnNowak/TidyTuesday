###############
# Clear space #
###############

rm(list=ls())
gc()

# Load packages and fonts
#########################
library(tidyverse)
library(plyr)
library(png)
library(patchwork)
library(showtext)
font_add_google(name = "Lobster", family = "Lobster")
showtext_auto()
font_add_google(name = "Cabin", family = "Cabin")
showtext_auto()
title<-'Lobster'
ax<-'Cabin'

# Load data
###########
tuesdata <- tidytuesdayR::tt_load('2021-06-08')

#fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')
stocked <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv')

resume_sp<-plyr::ddply(
  stocked,
  .(SPECIES),
  summarize,
  tot = sum(na.omit(NO_STOCKED))
)
# Keep only the five most important species
keep<-resume_sp$SPECIES[order(-resume_sp$tot)][1:5]

resume<-plyr::ddply(
  stocked,
  .(YEAR,SPECIES),
  summarize,
  tot = sum(NO_STOCKED)
)%>%
  subset(
    SPECIES %in% keep
  )%>%
  mutate(
    sp = 
      recode (SPECIES,
      "WAE"="Walleye",
      "CHS"="Chinook Salmon",
      "COS"="Coho Salmon",
      "RBT"="Rainbow Trout",
      "LAT"="Lake Trout"
    )
  )

resume[is.na(resume)] <- 0

# Order species by date of first stocking
resume$sp<-factor(
  resume$sp,
  c(
  "Walleye",
  "Chinook Salmon",
  "Coho Salmon",
  "Rainbow Trout",
  "Lake Trout"
))

final<-resume[resume$YEAR=="2018",]
final$sp<-factor(
  final$sp,
  c(
    "Lake Trout",
    "Rainbow Trout",
    "Coho Salmon",
    "Chinook Salmon",
    "Walleye"
))
final<-final[order(final$sp),]

# Create position column to add species name
final$cum<-final$tot
final$cum2<-final$tot/2

for (i in 2:5) {
  final$cum[i]=final$cum[i-1]+final$tot[i]
  final$cum2[i]=final$cum[i-1]+final$tot[i]/2
}
final

# Set color palette
# https://coolors.co/
col_vec<-c(
  "#ff6b35",
  "#f7c59f",
  "#efefd0",
  "#00B2CA",
  "#004e89"
)

#final2<-cbind.data.frame(final,col_vec)
#final2
#resume



library(ggforce)
library(metR)
library(grob)

p1<-ggplot(
  resume,
  aes(x=YEAR,y=tot,fill=sp))+
  annotate('segment',x=1950,xend=2015,y=40000000,yend=40000000,col='dimgrey')+
  annotate('segment',x=1950,xend=2018,y=20000000,yend=20000000,col='dimgrey')+
  annotate('text',x=1950,y=42000000,col='dimgrey',label="40 million fishes released",hjust=0,family=ax)+
  annotate('text',x=1950,y=22000000,col='dimgrey',label="20 million fishes",hjust=0,family=ax)+
  annotate('text',x=1950,y=52000000,col='black',size=6,label="The evolution of fish stocking\nin the Great Lakes from 1950 to 2018",hjust=0,family=title)+
  annotate('text',x=1965,y=16000000,col='black',label="1966",hjust=1,family=ax,fontface='bold')+
  annotate('text',x=1965,y=11000000,col='black',label="First salmon\nstocking",hjust=1,family=ax)+
  geom_area()+
  annotate('segment',x=1966,xend=1966,y=0,yend=17000000,col='dimgrey',lty="dotted")+
  annotate('text',x=1970,y=-3000000,col='dimgrey',label="1970",hjust=0.5,vjust=1,family=ax,size=4.5)+
  annotate('text',x=1990,y=-3000000,col='dimgrey',label="1990",hjust=0.5,vjust=1,family=ax,size=4.5)+
  annotate('text',x=2010,y=-3000000,col='dimgrey',label="2010",hjust=0.5,vjust=1,family=ax,size=4.5)+
  annotate('text',x=1950,y=-12000000,col='dimgrey',label="Data source:  Great Lakes Fishery Commission from Tidy Tuesday | Map: Ricketts et al. (1999)",hjust=0,family=ax,size=3.5)+
  coord_cartesian(clip = "off")+
  geom_point(final,mapping=aes(x=2020,y=cum2,color=sp),size=2)+
  geom_text(final,mapping=aes(x=2021,y=cum2,label=sp),hjust=0,size=3.5,color="black",fontface='italic',family=ax)+
  annotate('text',x=2023,y=49000000,col='forestgreen',label="The Great Lakes",hjust=0.5,family=title)+
  scale_x_continuous(limits = c(1950,2025),breaks=c(1970,1990,2010) )+
  guides(
    fill=FALSE,
    color=FALSE
  )+
  scale_fill_manual(values=col_vec)+
  scale_color_manual(values=rev(col_vec))+
    theme(
    text = element_text(family = ax),
    plot.margin=unit(c(1,1,1,0),'cm'),
    axis.line = element_blank(), 
    axis.ticks.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(), 
    legend.text=element_text(size=9),
    legend.title=element_text(size=10),
    panel.background=element_blank(),
    plot.background=element_blank(),
    legend.background=element_blank(),
    plot.title=element_text(size=16, vjust=0,hjust=0,family = title,color='black'),
    plot.subtitle=element_text(size=10, vjust=0,hjust=0,family = ax,color='dimgrey'),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0),
    panel.grid = element_blank()
  )

#p1
# map available here : https://fr.wikipedia.org/wiki/Fichier:Western_Great_Lakes_forests_map.svg
image<-readPNG("Tidy/Fish/map2.png",native=TRUE)

p1+inset_element(image,left=0.75,right=1,top=0.9,bottom=0.55,align_to="full",on_top=FALSE,clip=FALSE)


