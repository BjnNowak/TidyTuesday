library(tidyverse)
library(patchwork)
library(camcorder)
library(showtext)
library(ggtext)
library(waffle)

# Set fonts
font_add_google("Staatliches","sta")
font_add_google("Raleway","ral")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  height = 13.5, 
  width = 16, 
  units = "cm", 
  dpi = 300 
)

# Data uploaded from OWID :
# https://ourworldindata.org/grapher/share-cereals-animal-feed
data<-read_csv('https://raw.githubusercontent.com/BjnNowak/TidyTuesday/main/data/share-cereals-animal-feed.csv')%>%
  filter(Year==2021)%>%
  filter(Entity%in%c('Asia (FAO)','Africa (FAO)','Europe (FAO)','Americas (FAO)','Oceania (FAO)'))

colnames(data)[4]<-'feed'

data<-data%>%
  mutate(feed=round(feed))%>%
  mutate(other=100-feed)%>%
  mutate(name=case_when(
    Entity=='Asia (FAO)'~'Asia',
    Entity=='Africa (FAO)'~'Africa',
    Entity=='Europe (FAO)'~'Europe',
    Entity=='Americas (FAO)'~'Americas',
    Entity=='Oceania (FAO)'~'Oceania'
  ))%>%
  mutate(lab=glue::glue('<b>{name}</b><br>{feed} %'))%>%
  select(lab,feed,other)%>%
  pivot_longer(!lab, names_to = "type", values_to = "percent")

data$lab <- factor(data$lab, levels = c(
  '<b>Europe</b><br>66 %',
  '<b>Oceania</b><br>59 %',
  '<b>Americas</b><br>53 %',
  '<b>Asia</b><br>32 %',
  '<b>Africa</b><br>21 %'
  ))

ggplot(data, aes(fill = type, values = percent))+
  geom_waffle(na.rm=TRUE, n_rows=4, flip=F, size = 0.33, colour = "white")+
  facet_wrap(~reorder(lab, percent),ncol=1,strip.position = "left")+
  coord_equal()+
  guides(fill='none')+
  labs(
    title="<b>Share of cereals used as <span style='color:#f72585;'>animal feeds</span></b>",
    caption="<b>Data</b> OWID (year 2021) <b>| Plot</b> Benjamin Nowak"
  )+
  scale_fill_manual(values=c('#f72585','#4F0325'))+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#222725",color=NA),
    plot.title = element_markdown(size=60,family='sta',margin=margin(0.5,0,0.5,-0.75,'cm'),color='white'),
    strip.text = element_markdown(hjust=0.5,size=30,family='ral',angle=90,margin=margin(0,0,0,0,'cm'),lineheight = 0.45,color='white'),
    plot.caption = element_markdown(size=25,family='ral',margin=margin(0.5,0,0.5,-0.75,'cm'),hjust=0,color='white'),
  )

         