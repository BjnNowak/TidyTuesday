
library(tidyverse)
library(ggforce)
library(ggtext)
library(camcorder)
library(showtext)

# Set plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10*1.618, 
  units = "cm", 
  dpi = 300 
)

# Fonts
# Load fonts 
font_add_google("Public Sans","public")
font_add_google("Patrick Hand","hand")
font_add_google("Rubik","khand")
font_add_google("Patrick Hand SC","hand3")
# Automatically use {showtext} for plots
showtext_auto()

tx<-"hand3"
tit<-"khand"

# Colors
back<-"#fde4d0"
bl<-"#161411"

# Load data
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

# Prepare data
data<-airmen%>%
  separate(graduation_date, c("year", "month","day"))%>%
  mutate(ct=1)%>%
  group_by(year)%>%
  summarize(graduates=sum(ct))%>%
  ungroup()%>%
  drop_na()%>%
  mutate(year=as.numeric(year))


# Create both segments

off_x <- 10
off_y <- 0.16
year_bot <- 1939

coord<-data%>%
  mutate(
    ax=-off_x,
    bx=graduates+off_x,
    cx=graduates-off_x,
    dx=-off_x,
    ay=year+off_y,
    by=year+off_y,
    cy=year-off_y,
    dy=year-off_y
  )

coord2<-data%>%
  mutate(
    ax=graduates-off_x,
    bx=graduates-off_x,
    cx=graduates+off_x,
    dx=graduates+off_x,
    ay=year_bot,
    by=year-off_y,
    cy=year+off_y,
    dy=year_bot
  )

coord_long<-coord%>%
  select(
    year,
    ax,bx,cx,dx,
    ay,by,cy,dy
  )%>%
  pivot_longer(
    !year,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )

coord2_long<-coord2%>%
  select(
    year,
    ax,bx,cx,dx,
    ay,by,cy,dy
  )%>%
  pivot_longer(
    !year,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )

# Make plot

yaxis<-tibble(
  y=c(1942,1943,1944,1945,1946,1948),
  x=-40
)

l1<-1938.6
l2<-1938.8

xaxis<-tibble(
  x=c(10,39,63,241,273,369),
  y=c(l1,l2,l1,l1,l2,l1)
)

ggplot()+
  geom_shape(
    data=coord2_long,
    aes(x=x,y=y),
    fill=bl,
    color=bl,
    alpha=1
  )+
  geom_shape(
    data=coord_long,
    aes(x=x,y=y),
    fill=back,
    color=bl,
    alpha=1
  )+
  scale_y_continuous(limits=c(1938,1950))+
  scale_x_continuous(limits=c(-100,380))+
  theme_minimal()+
  annotate('segment',x=-off_x,xend=-off_x,y=1940,yend=1948.5,color=back)+
  annotate('segment',x=0,xend=380,y=year_bot,yend=year_bot,color=back)+
  annotate(
    'text',x=-70,y=1938.7,
    label="NUMBER OF\nGRADUATES",family=tx,
    hjust=0.5,size=10,lineheight=0.25,color=bl
  )+
  annotate(
    'text',x=190,y=1949.5,
    label="AFRICAN AMERICAN PILOTS\nGRADUATION IN THE\nUS ARMY AIR FORCES.",family=tit,
    hjust=0.5,size=12,lineheight=0.28,color=bl,fontface='bold'
  )+
  geom_text(
    data=yaxis,
    aes(x=x,y=y,label=y),
    family=tx,color=bl,size=10
  )+
  geom_text(
    data=xaxis,
    aes(x=x,y=y,label=x),
    family=tx,color=bl,size=10
  )+
  theme(
    plot.margin = margin(0.5,1,0.2,1, "cm"),
    plot.background = element_rect(fill="#fde4d0",color=NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )
  
