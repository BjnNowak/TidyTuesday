library(tidyverse)
library(ggtext)
library(camcorder)
library(showtext)



# Colors
green_str <- "#067655"
darkgreen <- "#1e3832"
yellow <- "#d0a14b"

# Load fonts
font_add_google("Kanit","kanit")
# Automatically use {showtext} for plots
showtext_auto()

starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')


tall <- starbucks%>%
  filter(size=='tall')

milk<-tall%>%
  filter(milk!=0)%>%
  filter(whip==0)

names_whip <- tall%>%
  filter(milk==1)%>%
  filter(whip==1)%>%
  pull(product_name)

whip<-tall%>%
  filter(milk==1)%>%
  filter(product_name%in%names_whip)%>%
  group_by(product_name,whip)%>%
  summarize(
    clean_calories=mean(calories)
  )


mean_whip<-whip%>%
  group_by(whip)%>%
  summarize(
    mean_whip=mean(clean_calories),
    min_whip=min(clean_calories),
  )



max_name <- tibble(
  name=c('White<br>Chocolate<br>Mocha','Oprah<br>Cinnamon<br>Chai'),
  x=c(1.05,-0.05),
  y=c(330,140),
  hjust=c(0,1)
)

tit <- tibble(
  label=c(
    "To add whip<br>
    Or not to add?"
  ),
  x=0,
  y=330
)

av <- tibble(
  label=c(
    "On average, one shot of whip<br>
    add **72 KCal** to your tall<br>
    (354 ml) drink at Starbucks"
  ),
  x=0.75,
  y=140
)


ggplot(data=milk,aes(x=milk,y=calories,group=product_name))+
  geom_line()+
  geom_point()


lab_cream<-tibble(
  x=c(0,1)
)

lab_cream_cal <- mean_whip%>%
  mutate(whip_cal=round(mean_whip))%>%
  mutate(label=glue::glue('{whip_cal} KCal'))%>%
  mutate(label_cream=case_when(
    whip==0~"No whip",
    whip==1~"One shot<br>of whip"
  ))

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

ggplot(data=whip,aes(x=whip,y=clean_calories,group=as.factor(product_name)))+
  geom_line(col=yellow)+
  geom_point(col=yellow)+
  geom_point(
    data=mean_whip,aes(x=whip,y=mean_whip),
    col=green_str,
    size=4,inherit.aes = FALSE
  )+
  geom_line(
    data=mean_whip,aes(x=whip,y=mean_whip),
    col=green_str,
    size=2,inherit.aes = FALSE
  )+
  geom_richtext(
    data=lab_cream_cal,
    aes(x=whip,y=mean_whip+15,label=label),
    inherit.aes=FALSE,family="kanit",
    size=12,lineheight = 0.1,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_richtext(
    data=lab_cream_cal,
    aes(x=whip,y=min_whip-30,label=label_cream),
    inherit.aes=FALSE,family="kanit",fontface='bold',
    size=12,lineheight = 0.1,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_richtext(
    data=max_name,
    aes(x=x,y=y,label=name,hjust=hjust),
    inherit.aes=FALSE,family="kanit",color=yellow,
    size=9,lineheight = 0.1,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  geom_richtext(
    data=av,
    aes(x=x,y=y,label=label),
    inherit.aes=FALSE,family="kanit",color="black",
    size=10,lineheight = 0.4,hjust=0.5,
    fill = "#fdfcdc", label.color = green_str, 
    label.padding = grid::unit(rep(5, 4), "pt")
  )+
  geom_richtext(
    data=tit,
    aes(x=x,y=y,label=label),
    inherit.aes=FALSE,family="kanit",color=green_str,fontface='bold',
    size=14,lineheight = 0.4,hjust=0,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  
  scale_x_continuous(limits = c(-0.25,1.25))+
  scale_y_continuous(limits = c(100,360))+
  theme_void()
  #geom_text_repel(aes(label=product_name))




