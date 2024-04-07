library(tidyverse)
library(ggridges)
library(camcorder)
library(showtext)
library(ggtext)
library(scico)
library(MetBrewer)


# Set fonts
font_add_google("Anton","anton")
font_add_google("Londrina Solid","lon")
font_add_google("Quicksand","quick")
font_add_google("Open Sans","open")
font_add_google("Raleway","ral")
font_add_google("Staatliches","sta")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 23.3, 
  height = 32, 
  units = "cm", 
  dpi = 300 
)

data<-read_delim('Data/wheat_yield_evo.csv')

clean<-data%>%
  filter(Value<13*10000)%>%
  mutate(
    mn=min(Value),
    mx=max(Value),
    nd=(Value-mn)/(mx-mn)
  )%>%
  #filter(Year%in%seq(1965,2025,5))
  filter(Year%in%seq(1962,2025,4))

ggplot()+
  geom_density_ridges_gradient(
    clean,
    mapping=aes(x=Value,y=as.factor(-Year),fill = after_stat(x)),
    #color="dimgrey",
    jittered_points = FALSE, 
    scale = 4, 
    #scale = 0.975, 
    #rel_min_height = .001,
    point_shape = "|", 
    point_size = 10, 
    #size = 0.25,
    position = position_points_jitter(height = 0, width=0),color="white"
  )+
  geom_text(
    clean,
    mapping=aes(x=max(clean$Value)-1000,y=as.factor(-Year),label=Year),
    hjust=1,vjust=0,size=20,
    family='quick',color="white",fontface='bold'
  )+
  guides(fill='none')+
  #MetBrewer::scale_fill_met_c(name = "Benedictus")+
  scale_fill_scico(palette = 'vik',direction=-1)+ 
  scale_x_continuous(
    limits=c(0,max(clean$Value)),
    breaks = seq(20000,80000,20000),
    labels = c(glue::glue("{seq(2,8,2)} t.ha<sup>-1</sup>"))
    #labels=rep("|",4)
  )+
  labs(
    #title="<b>The Yield Gap</b>",
    #subtitle = "The graph below shows the distribution of yields between countries. Each bar represents<br>a country's mean yield for year 2021. Yields have been normalized so that different crops<br>can be compared on the same plot. The **distribution peaks on the left** show that the<br>majority of world countries have low yields, while only a minority show high yields.",
    #caption="<b>Data</b> FAOStats <b>| Plot</b> Benjamin Nowak"
  )+
  coord_cartesian(clip = 'off') +
  theme_void()+
  theme(
    plot.margin = margin(1,1,1,1,"cm"),
    #plot.background = element_rect(fill="#222725",color=NA),
    plot.title = element_markdown(size=100,family='lon',margin=margin(1,0,0.5,1.2,'cm'),color='white'),
    plot.subtitle = element_markdown(size=45,family='open',margin=margin(0,0,1,1.2,'cm'),color='white',lineheight=0.40),
    plot.caption = element_markdown(size=35,family='open',margin=margin(0.5,0,0.5,1.2,'cm'),hjust=0,color='white'),
    axis.text.x = element_markdown(size=45,color='white',family='ral',lineheight = 0.35,face="bold")
  )



