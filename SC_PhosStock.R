library(tidyverse)
library(ggforce)
library(camcorder)
library(showtext)
library(ggtext)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Pacifico","pac")
font_add_google("Crete Round","cre")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 29.7, 
  height = 21, 
  units = "cm", 
  dpi = 300 
)

# https://www.sciencedirect.com/science/article/pii/S0921344914001967?via%3Dihub
data<-tibble(
  ref = c('Steen<br>(1998)', 'Smil<br>(2000)','Cordell<br>*et al.* (2009)','Van Vuuren<br>*et al.* (2010)','Cordell<br>*et al.* (2011)','Sverdrup and<br>Ragnarsdottir (2011)','Scholz and<br>Wellmer (2013)'),
  year_study = c(1998,2000,2009,2010,2011,2011,2013),
  type=c('range','single','single','range','single','range','single'),
  stock_P = c(21.5,22.7,2.4,22.95,7.3,5.65,24),
  year_remaining = c(720,760,80,770,240,185,800),
  min = c(550,NA,NA,240,NA,170,NA),
  max = c(890,NA,NA,1300,NA,200,NA),
  y =c(1,1.66,1.66,3,1.66,1,2.33)
)

clean <- data%>%
  mutate(finish=year_study+year_remaining)%>%
  mutate(finish_min=year_study+min)%>%
  mutate(finish_max=year_study+max)%>%
  arrange(-finish)%>%
  mutate(nb=row_number())

col_point <- "#e63946"
col_range <- "#9d0208"

font_back2 <- "#EBEBEB" 

col_arrow <- '#3d5a80'

yr<-tibble(
  yr=c(2022,seq(2200,3200,200)),
  lab=c('Today',seq(2200,3200,200))
)

caption <- tibble(
  x=3390,
  y=1.2,
  lab="**Data:** Reijnders (2014) **| Plot:** @BjnNowak "
)

ggplot()+
  annotate(
    'rect',xmin=-Inf,xmax=2200,ymin=-Inf,ymax=Inf,
    fill=font_back2)+
  annotate(
    'rect',xmin=2400,xmax=2600,ymin=-Inf,ymax=Inf,
    fill=font_back2)+
  annotate(
    'rect',xmin=2800,xmax=3000,ymin=-Inf,ymax=Inf,
    fill=font_back2)+
  annotate(
    'rect',xmin=3200,xmax=3400,ymin=-Inf,ymax=Inf,
    fill=font_back2)+

  geom_text(
    data=yr,inherit.aes=FALSE,
    aes(
      x=yr,
      #x=yr-20,
      label=lab),y=0.50,hjust=1,
    size=15,angle=90,family='cond'
  )+
  geom_text(
    data=yr,inherit.aes=FALSE,
    aes(
      x=yr,
      #x=yr-20,
      label='I'),y=0.6,hjust=0.5,
    size=15,angle=0,family='cond'
  )+
  annotate(
    'segment',x=2022,xend=2022,y=0.7,yend=3.4,
    color="black",lty='dotted'
  )+
  
  geom_point(
    clean%>%filter(type=='single'),
    mapping=aes(x=finish,y=y),
    size=8,color=col_point
  )+
  geom_segment(
    inherit.aes=FALSE,
    clean%>%filter(type=='range'),
    mapping=aes(x=finish_min,xend=finish_max,y=y,yend=y),
    size=9,col=col_point,alpha=0.5
  )+
  geom_point(
    clean%>%filter(type=='range'),
    mapping=aes(x=finish_min,y=y),
    size=8,color=col_point
  )+
  geom_point(
    clean%>%filter(type=='range'),
    mapping=aes(x=finish_max,y=y),
    size=8,color=col_point
  )+
  geom_richtext(
    data=clean,
    aes(x = finish, y = y+0.2, label = ref),
    inherit.aes=FALSE,
    family='cond',size=12,
    hjust=0.5,vjust=0.5,lineheight=0.45,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  
  annotate(
    "segment",
    x = 2022, y = 1.66,
    xend = 2089, yend = 1.66,
    lineend = "butt", 
    linejoin = "mitre",
    size = 3, 
    arrow = arrow(length = unit(0.5, "cm")),
    colour = col_arrow, alpha=0.5 
  ) + 
  
  annotate(
    "segment",
    x = 2022, y = 2.33,
    xend = 2813, yend = 2.33,
    lineend = "butt",
    linejoin = "mitre",
    size = 3, 
    arrow = arrow(length = unit(0.5, "cm")),
    colour = col_arrow, alpha=0.5 
  ) + 
  
  annotate(
    'text',x=2026,y=1.5,
    label='Lowest\nestimate:\n2089',
    size=12,family='cond',lineheight=0.35,
    hjust=0,vjust=1,color=col_arrow,fontface='italic')+
  
  annotate(
    'text',x=2026,y=2.4,
    label='Among highest assessments, this study estimates total depletion of phosphorus reserves in 2813',
    size=12,family='cond',lineheight=0.35,
    hjust=0,vjust=0,color=col_arrow,fontface='italic')+
  
  annotate(
    'text',x=2680,y=3.8,
    label='When will we run out of phosphorus?',
    size=40,family='cre',hjust=0.5,color='#293241',fontface='bold')+
  
  annotate(
    'text',x=3390,y=2.7,
    label=
'Like oil or gas, phosphorus is a non-renewable resource.
More than 90% of current use of phosphorus resources
is as fertilizer in agriculture. Phosphorus is essential for
plant growth and yields depend on available fertilizers.

There are now concerns about the depletion of reserves 
but estimates of remaining stocks vary between studies.

Based on these stock estimates, the year of complete
depletion of phosphorus reserves has been estimated 
here by considering a constant annual consumption of 
30,000,000 tons of phosphorus.',
    size=14,family='cond',hjust=1,vjust=1,
    color='#293241',lineheight=0.35)+

  geom_richtext(
    caption,mapping=aes(x=x,y=y, label=lab),
    inherit.aes = FALSE,color='#293241',
    size=12, family='cond',hjust=1,lineheight=0.25,vjust=1,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  
  scale_y_continuous(limits=c(0.2,4))+
  scale_x_continuous(limits=c(2020,3400))+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#FFFFFF",color=NA))
