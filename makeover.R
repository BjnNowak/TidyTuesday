library(tidyverse)
library(ggtext)

# Data
data<-tibble(
  name=c('Nathalie<br>**Arthaud**','Fabien<br>**Roussel**','Jean-Luc<br>**Mélenchon**','Marine<br>**Tondelier**','François<br>**Hollande**','Edouard<br>**Philippe**','Dominique<br>**de Villepin**','Xavier<br>**Bertrand**','Nicolas<br>**Dupont-Aignan**','Jordan<br>**Bardella**','Eric<br>**Zemmour**'),
  val=c(1,3,12,5.5,6.5,19,4,5.5,2.5,35,6),
  side=c("ext_g","ext_g","ext_g","vert",'g','p','dv','xb','xb','jb','ez')
)

# Color palette
pal<-c(
  'ext_g'='#ff043a',
  'vert'='#18b85e',
  'g'='#fb038d',
  'p'='#fdd72c',
  'dv'='#0fa9f9',
  'xb'='#1362c7',
  'jb'='#8411c6',
  'ez'='#000000'
)

# Plot
pl<-ggplot(data)+
  geom_segment(
    aes(x=0,xend=val,y=fct_reorder(name,val),color=side),
    linewidth=3
  )+
  geom_richtext(
    aes(y=fct_reorder(name,val),x=val+0.5,label=name),
    hjust=0,size=1.5,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  annotate(
    geom='segment',
    x=0,xend=0,y=-Inf,yend=Inf,
    color='dimgrey',
    linewidth=0.75
  )+
  scale_x_continuous(
    expand=c(0,0),limits=c(0,45),
    breaks=seq(0,30,10),
    labels=glue::glue("{seq(0,30,10)} %")
  )+
  labs(
    title="Présidentielle 2027 - intentions de vote au premier tour",
    subtitle="Elabe pour La Tribune et BFMTV (échantillon de 1501 personnes)",
    x="",y=""
  )+
  scale_color_manual(values=pal)+
  guides(color='none')+
  theme(
    plot.title = element_text(face='bold',size=7),
    plot.subtitle = element_text(color='dimgrey',size=5.5),
    ##plot.subtitle = element_markdown(),
    panel.background =element_rect(fill="white",color=NA),
    panel.grid.major.x=element_line(
      color=alpha("dimgrey",0.15),linewidth=0.25
    ),
    axis.ticks=element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_text(size=5.5,color="dimgrey")
  )

pl

ggsave(plot=pl,filename="makeover.png", width = 10.8, height = 6.75, units = "cm")
