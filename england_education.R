library(tidyverse)
library(ggBubbles)
library(scico)
library(MoMAColors)
library(camcorder)
library(showtext)
library(ggtext)

# Set fonts
font_add_google("Bebas Neue","beb")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Fira Sans","fira")
font_add_google("Jost","jost")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 25, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)


english_education <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv')

summary(english_education$education_score)

clean<-english_education%>%
  drop_na(education_score)%>%
  mutate(rk=case_when(
    education_score<(-3)~1,
    education_score<(-1)~2,
    education_score<1~3,
    education_score<3~4,
    TRUE~5
  ))%>%
  filter(size_flag%in%c('Small Towns','Medium Towns','Large Towns'))

clean$income_flag <- factor(
  clean$income_flag, 
  levels=c('Higher deprivation towns', 'Mid deprivation towns', 'Lower deprivation towns')
)

display.all.moma(5)

pal<-moma.colors("Kippenberger", n=5, type="discrete")

ggplot(clean,aes(x=income_flag,y=size_flag,color=as.factor(rk)))+
  geom_point(position = position_surround(offset = .04), size =1)+
  scale_color_manual(
    values=pal
    #labels=c('Low','','Mid','','High'),
    #guide=guide_legend(
    #  direction='horizontal',
    #  title.position = "top",
    #  label.position = "bottom",
    #  override.aes = list(size = 7.5)
    #)
  )+
  labs(
    title="Relationship between level of education and city features in England",
    subtitle = "**Each dot represents an English city**, colored according to level of education (from <b><span style='color:#c1447e'>low</span></b> to <b><span style='color:#6e8537'>high</span></b>).<br>Regardless of city size, there is **a positive relationship between education and income** (horizontal axis).",
    caption = "**Data** UK Office for National Statistics **| Plot** Benjamin Nowak",
    #color = "**Education**"
  )+
  guides(color='none')+
  theme_void()+
  theme(
    plot.margin=margin(1,1,1,1,'cm'),
    plot.background = element_rect(fill="#191919"),
    axis.text=element_text(color='white',size=40,family='jost',face='bold'),
    axis.text.y=element_text(hjust=1),
    #legend.position = "bottom",
    #legend.margin = margin(0.5,0,0,0,'cm'),
    #legend.spacing.x = unit(-0.2, 'cm'),
    #legend.title = element_markdown(family="bit",color="white",size=40,angle=0,vjust=0.5,hjust=0.5,margin=margin(0,0,-0.5,0,'cm')),
    #legend.text =element_markdown(family="jost",color="white",size=40,angle=0,vjust=0.5,hjust=0.5,margin=margin(-1,0,0,0,'cm')),
    plot.title = element_markdown(family="beb",color="white",size=70,margin=margin(0.5,0,0,-2.5,'cm')),
    plot.subtitle = element_markdown(family="jost",color="white",size=40,lineheight=0.45,margin=margin(0.5,0,0.5,-2.5,'cm')),
    plot.caption = element_markdown(family="jost",color="white",size=30,hjust=0,margin=margin(1,0,0,-2.5,'cm'))
  )

