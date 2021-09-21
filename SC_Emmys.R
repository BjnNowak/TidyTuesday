# Clear space 
###############
rm(list=ls())
gc()

library(tidyverse)
library(showtext)
library(camcorder)
library(cowplot)

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21*1.618, 
  height = 21, 
  #width = 10*1.618, 
  #height = 10, 
  units = "cm", 
  dpi = 300 
)

# Load fonts
font_add_google("Libre Baskerville", "basker")
font_add_google("RocknRoll One", "rock")
font_add_google("Barlow Condensed", "barlow")
font_add_google("Roboto Condensed", "roboto")
font_add_google("Numans", "numans")
# Automatically use {showtext} for plots
showtext_auto()

# Set color and fonts

col_show <- '#332E3C'
col_bar<-'#B5AEC1'

fam_show <- 'barlow'
fam_ax <- 'roboto'

# Load data
tuesdata <- tidytuesdayR::tt_load('2021-09-21')
nominees <- tuesdata$nominees

# Plot 1 : most losses
clean_whole <- nominees%>%
  distinct(category,title,.keep_all=TRUE)%>%
  group_by(title)%>%
  mutate(ct=1)%>%
  summarize(
    win=sum(ct[type=="Winner"]),
    lose=sum(ct[type=="Nominee"]),
    all=sum(ct[type=="Nominee"])+sum(ct[type=="Winner"])
  )%>%
  mutate(ratio=lose/all)%>%
  arrange(-lose)

most <- clean_whole%>%
  head(5)

p1<-ggplot(most,aes(y=fct_reorder(title,lose)))+
  geom_segment(
    aes(x=win,xend=lose,yend=title),size=3,
    col="#F1E3D3"
    )+
  geom_point(aes(x=win),size=3,col="#33658A")+
  geom_point(aes(x=lose),size=3,col="#61304B")+
  geom_text(
    aes(label=title,x=lose+4),
    hjust=0,col='#332E3C',family=fam_show,size=14,
  )+
  geom_text(
    aes(label=lose,x=lose,y=title),
    nudge_y=0.3,col="#61304B",size=12,family=fam_ax)+
  geom_text(
    data=most%>%filter(title=='30 Rock'),
    aes(x=lose,y=title),label='Losses',fontface='italic',
    nudge_y=-0.3,col="#61304B",size=12,family=fam_ax)+
  geom_text(
    aes(label=win,x=win,y=title),
    nudge_y=0.3,col="#33658A",size=14,family=fam_ax)+
  geom_text(
    data=most%>%filter(title=='30 Rock'),
    aes(label=win,x=win,y=title),label='Wins',fontface='italic',
    nudge_y=-0.3,col="#33658A",size=14,family=fam_ax)+
  scale_x_continuous(limits=c(0,140))+
  coord_cartesian(clip='off')+
  theme_minimal()+
  theme(
    panel.grid=element_blank(),
    axis.text.y=element_blank(),
    axis.title.y = element_blank(),
    axis.text.x=element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust=0.5)
  )

# Plot 2 : zero win
# See here for tutorial on polar barplot: 
# https://bjnnowak.netlify.app/2021/08/31/r-polar-barplots/

zero<-clean_whole %>%
  filter(win==0) %>%
  head(10)

zero_blank<-zero%>%
  select(c(title,lose))%>%
  add_row(
    lose=c(0,0),  
    .before = 1  
  )%>%
  mutate(
    id = row_number()
  )%>%mutate(
    angle=90-360*(id-0.5)/max(id)
  )%>%
  mutate(
    hjust=case_when(
      angle<=-90~1,
      TRUE~0
    )
  )%>%
  mutate(
    angle=case_when(
      angle<=-90~angle+180,
      TRUE~angle
    )
  )

grid_manual <- data.frame(
  x = c(1.5,1.5),
  xend = c(2.4,2.4),
  y = c(15,30)
  
)

p2<-ggplot()+
  geom_bar(
    data=zero_blank,
    aes(x=id, y=lose),   
    stat="identity",fill="#61304B")+
  ylim(
    -max(zero_blank$lose)/2,
    max(zero_blank$lose)*1.5
  )+
  coord_polar(start=0)+
  geom_text(
    data=zero_blank,
    aes(x=id,y=lose+2,label=title,angle=angle,hjust=hjust),
    size=12,
    family=fam_show
  )+
  geom_segment(
    data=grid_manual,
    aes(x=x,xend=xend,y=y,yend=y),
    col="grey50"
  )+
  geom_text(
    data=grid_manual,
    aes(x=1,y=y,label=y),
    size=15,col="grey50",family=fam_ax,
    hjust=0
  )+
  annotate(
    geom='text',
    x=1,y=40,
    label="Number of nominations",
    size=12,col="grey50",family=fam_ax,
    hjust=0
  )+
  theme_minimal()+
  theme(                          
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")
  )

# Plot 3 : most losses in a year

clean_whole_year <- nominees%>%
  distinct(category,title,.keep_all=TRUE)%>%
  group_by(title,year)%>%
  mutate(ct=1)%>%
  summarize(
    win=sum(ct[type=="Winner"]),
    lose=sum(ct[type=="Nominee"]),
    all=sum(ct[type=="Nominee"])+sum(ct[type=="Winner"])
  )%>%
  mutate(ratio=lose/all)%>%
  arrange(-lose)%>%
  mutate(lab=paste0(title," (",year,")"))

p3<-ggplot(data=clean_whole_year%>%head(5),
  aes(y=fct_reorder(lab,lose)))+
  geom_bar(aes(x=lose),stat='identity',fill="#61304B",width=0.7)+
  geom_bar(aes(x=win),stat='identity',fill="#33658A",width=0.35)+
  geom_text(
    aes(x=lose,label=title),nudge_x = 0.2,hjust=0,
    family=fam_show,size=14
  )+
  geom_text(
    aes(x=lose,label=lose),nudge_x = -0.2,hjust=1,
    family=fam_ax,size=12,color="white"
  )+
  geom_text(
    aes(x=win,label=win),nudge_x = -0.2,hjust=1,
    family=fam_ax,size=12,color="white"
  )+
  geom_text(
    aes(label=year),hjust=0.5,x=8,fontface='italic',
    family='numans',size=15,color="white"
  )+
  theme_minimal()+

  scale_x_continuous(limits=c(0,20))+
  coord_cartesian(clip='off')+
  theme(                          
    panel.grid=element_blank(),
    axis.text.y=element_blank(),
    axis.title.y = element_blank(),
    axis.text.x=element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust=0.5)
  )

# Assemble with cowplots

ggdraw() +
  draw_plot(p2, x = 0, y = 0.08, width = 0.4, height = 0.70)+
  draw_plot(p1, x = 0.5, y = 0.5, width = 0.5, height = 0.4)+
  draw_plot(p3, x = 0.47, y = 0.05, width = 0.5, height = 0.4)+
  draw_text(
    text = "Source: emmys.com",  
    size = 40,
    family="roboto",
    hjust=0,color="dimgrey",
    x = 0, y = 0.07,vjust=0.5)+
  draw_text(
    text = "Lost in the Emmy Awards",  
    size = 95,
    family="rock",
    hjust=0,color="#343a40",
    x = 0, y = 0.88,vjust=0)+
  draw_text(
    text = "Highest total of losses",  
    size = 60,
    family="numans",
    hjust=0,color="#343a40",
    x = 0.5, y = 0.90,vjust=0)+
  draw_text(
    text = "Most losses in a year",  
    size = 60,
    family="numans",
    hjust=0,color="#343a40",
    x = 0.5, y = 0.45,vjust=0)+
  draw_text(
    text = "Most losses without a single win",  
    size = 60,
    family="numans",
    hjust=0,color="#343a40",
    x = 0, y = 0.8)
  
