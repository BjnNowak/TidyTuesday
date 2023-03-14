library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)
library(ggforce)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Calistoga","cal")
font_add_google("Staatliches","sta")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 65, 
  units = "cm", 
  dpi = 300 
)

# Load data
drugs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')

# Data preparation
##################

# First grouping by active substance
res3<-drugs%>%
  filter(category=="human")%>%
  mutate(ct=1)%>%
  group_by(active_substance,product_number)%>%
  summarize(
    sm=sum(ct),
    th=therapeutic_area[1],
    gr=pharmacotherapeutic_group[1])%>%
  ungroup()%>%
  mutate(
    total=sum(sm),
    ratio=sm/total
  )%>%
  arrange(-ratio)%>%
  mutate(cm=cumsum(ratio))%>%
  mutate(ct=1)%>%
  group_by(active_substance)%>%
  mutate(nb_use=sum(ct))%>%
  ungroup()

# Sort by id number to place on pills tab later
clean <- res3%>%
  filter(nb_use>8)%>%
  group_by(active_substance)%>%
  arrange(gr)%>%
  mutate(id=row_number())%>%
  ungroup()%>%
  mutate(x=case_when(
    id<4~1,
    id<7~2,
    id<10~3,
    id<13~4,
    id<16~5,
    id<19~6
  ))%>%
  mutate(y=case_when(
    id%in%seq(1,19,3)~0.25,
    id%in%seq(2,20,3)~0,
    id%in%seq(3,21,3)~-0.25
  ))

# Select most used active sustance to use
# row number as y pos
act <- clean%>%
  group_by(active_substance)%>%
  summarize(sm=sum(nb_use))%>%
  arrange(sm)%>%
  mutate(ypos=row_number())%>%
  select(active_substance,ypos)

# Add y pos to data
cl<-clean%>%
  left_join(act)%>%
  mutate(yfin=y+ypos)%>%
  mutate(ct=1)%>%
  # Show only top 8 therapeutic use
  group_by(gr)%>%
  mutate(tot=sum(ct))%>%
  mutate(gr_simp=case_when(
    tot>8~gr,
    TRUE~"Others"
  ))

# Create some tibbles for plot
##############################

# Line for grid
yline <- tibble(
  y=seq(1,11),
  xmax=max(tab_long$x)
)

# Labels for active substances
act_lab <- act%>%
  mutate(label=case_when(
    active_substance=="insulin human"~"insulin",
    active_substance=="pioglitazone hydrochloride"~"pioglitazone<br>hydrochloride",
    TRUE~active_substance
  ))

# Tibble for empty pill
tib<-tibble(
  x=rep(rep(seq(1,6),3),11),
  y=rep(c(rep(-0.25,6),rep(0,6),rep(0.25,6)),11),
  yoff=c(
    rep(1,18),rep(2,18),rep(3,18),rep(4,18),rep(5,18),rep(6,18),
    rep(7,18),rep(8,18),rep(9,18),rep(10,18),rep(11,18)
))%>%
  mutate(yfin=y+yoff)


# Tibble to mimick pill tabs
xoff<-0.5
yoff<-0.2

tab<-tibble(y_id=seq(1,11,1))%>%
  mutate(
    ax=0-xoff,
    bx=0-xoff,
    cx=6+xoff,
    dx=6+xoff,
    ay=y_id-0.25-yoff,
    by=y_id+0.25+yoff,
    cy=y_id+0.25+yoff,
    dy=y_id-0.25-yoff
  )

# Pivot from wide to long 
tab_long <- tab %>%
  pivot_longer(
    !y_id,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )

# tibble with title and subtitle
tit<-tibble(
  x=(min(tab_long$x)+max(tab_long$x))/2,
  xmin=min(tab_long$x),
  y=max(tab_long$y)+0.1,
  label="Most commonly used<br>medicinal active substances",
  sub="Each pill is one drug using the substance<br>The color represents the therapeutic use",
  leg=
    "<span style='color:#8338ec'>Antiepileptics</span> <span style='color:#fb5607'>Antihistaminic</span> <span style='color:#ffbe0b'>Antineoplastic</span> <span style='color:#55D6BE'>Antithrombotic</span><br>
  <span style='color:#EFBDEB'>Diabetes</span> <span style='color:#3a86ff'>Immunostimulants</span> <span style='color:#ff006e'>Immunosuppressants</span> <span style='color:#00A878'>Psycholeptics</span> <span style='color:grey60'>Others</span>"
)

# caption
cap<-tibble(
  label = "**Data** European Medicines Agency | **Plot** @BjnNowak",
  xmin=min(tab_long$x)
)


# Set color palette
levels(as.factor(cl$gr_simp))
pal<-c(
  "Antiepileptics,"="#8338ec",
  "Antihistamines for systemic use,"="#fb5607",
  "Antineoplastic agents"="#ffbe0b",           
  "Antithrombotic agents"="#55D6BE",
  "Drugs used in diabetes"="#EFBDEB",          
  "Immunostimulants," ="#3a86ff",              
  "Immunosuppressants"="#ff006e",              
  "Others"="grey60"  ,                        
  "Psycholeptics,"="#00A878"  
)

back <- "#14213d"

# Make plot

ggplot()+
  # Add tabs
  ggforce::geom_shape(
    data=tab_long,
    aes(x=x,y=y,group=y_id),
    fill="grey90",color="black",
    radius = unit(0.5, 'cm')
  )+
  # Add empty pills
  geom_point(data=tib,aes(x=x,y=yfin),pch=21,size=12,fill=NA)+
  # Add colored pills
  geom_point(data=cl,aes(x=x,y=yfin,color=gr_simp),size=9)+
  
  # Add grid
  geom_segment(
    data=yline,
    mapping=aes(x=0.5,xend=xmax,y=y+0.25/2,yend=y+0.25/2),
    linewidth=0.15,alpha=0.5 
  )+
  geom_segment(
    data=yline,
    mapping=aes(x=0.5,xend=xmax,y=y-0.25/2,yend=y-0.25/2),
    linewidth=0.15,alpha=0.5 
  )+
  
  geom_segment(
    data=yline,
    mapping=aes(x=0.5,xend=0.5,y=y-0.45,yend=y+0.45),
    linewidth=0.15,alpha=0.5 
  )+
  geom_segment(
    data=yline,
    mapping=aes(x=1.5,xend=1.5,y=y-0.45,yend=y+0.45),
    linewidth=0.15,alpha=0.5 
  )+
  geom_segment(
    data=yline,
    mapping=aes(x=2.5,xend=2.5,y=y-0.45,yend=y+0.45),
    linewidth=0.15,alpha=0.5 
  )+
  geom_segment(
    data=yline,
    mapping=aes(x=3.5,xend=3.5,y=y-0.45,yend=y+0.45),
    linewidth=0.15,alpha=0.5 
  )+
  geom_segment(
    data=yline,
    mapping=aes(x=4.5,xend=4.5,y=y-0.45,yend=y+0.45),
    linewidth=0.15,alpha=0.5 
  )+
  geom_segment(
    data=yline,
    mapping=aes(x=5.5,xend=5.5,y=y-0.45,yend=y+0.45),
    linewidth=0.15,alpha=0.5 
  )+
  
  # Add active subst names
  geom_richtext(
    data=act_lab,
    aes(x=0.1,y=ypos,label=label),
    size=15,family="cal",color="black",lineheight=0.45,
    fontface="bold",angle=90,alpha=0.9,
    fill = NA, label.color = NA,hjust=0.5,vjust=0.5, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  
  # Add title
  geom_richtext(
    data=tit,
    aes(x=xmin,y=y+0.8,label=label),
    size=28,family="ral",color="white",lineheight=0.45,
    fontface="bold",
    fill = NA, label.color = NA,hjust=0,vjust=0, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    data=tit,
    aes(x=xmin,y=y+0.35,label=sub),
    size=20,family="fira",color="white",lineheight=0.45,
    fill = NA, label.color = NA,hjust=0,vjust=0, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    data=tit,
    aes(x=xmin,y=y+0.02,label=leg),
    size=14,family="cond",color="white",lineheight=0.45,
    fill = NA, label.color = NA,hjust=0,vjust=0, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    data=cap,
    aes(x=xmin,y=0.45,label=label),
    size=12,family="cond",color="white",lineheight=0.45,
    fill = NA, label.color = NA,hjust=0,vjust=1, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  
  scale_x_continuous(limits=c(-2,8))+
  scale_y_continuous(limits=c(0.45,13))+
  scale_color_manual(values=pal)+
  guides(color="none")+
  theme_void()+
  theme(
    panel.background = element_rect(fill=back,color=back)
  )

