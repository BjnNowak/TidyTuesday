library(tidyverse)
library(camcorder)
library(showtext)
library(glue)
library(ggtext)
library(patchwork)


# Fonts
font_add_google("Playfair Display","play")
font_add_google("Roboto","rob")
font_add_google("Roboto Condensed","cond")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 12.5, 
  units = "cm", 
  dpi = 300 
)


# Load data (downloaded from FAO Stat)
data<-readr::read_csv('https://raw.githubusercontent.com/BjnNowak/TidyTuesday/main/data/south_asia.csv')

# Keep only main cereal crops
clean<-data%>%
  filter(
    Item=="Rice, paddy"|
    Item=="Barley"|
    Item=="Maize"|
    Item=="Wheat"|
    Item=="Sorghum"|
    Item=="Millet"|
    Item=="Buckwheat"
  )

# Prep plots
# Color palette
col_neutral <- "grey80"

pal <- c(
  "Millet" = "#6B0504",
  "Sorghum" = "#E56B6F",
  "Wheat" = "#2B50AA",
  "Maize" = "#96CDFF",
  "Rice, paddy" = col_neutral,
  "Barley" = col_neutral,
  "Buckwheat" = col_neutral
  
)

# Y positions
max_harv<-max(clean$Value[clean$Element=="Area harvested"])
max_yield<-max(clean$Value[clean$Element=="Yield"])

y_millet<-clean$Value[clean$Element=="Area harvested"&clean$Item=='Millet'&clean$Year==2018]
y_wheat<-clean$Value[clean$Element=="Area harvested"&clean$Item=='Wheat'&clean$Year==2018]

y_maize<-clean$Value[clean$Element=="Yield"&clean$Item=='Maize'&clean$Year==2018]
y_sorghum<-clean$Value[clean$Element=="Yield"&clean$Item=='Sorghum'&clean$Year==2018]

# Make plots
area<-ggplot(
  clean%>%filter(Element=="Area harvested"),
  aes(x=Year,y=Value,color=Item))+
  annotate(
    "text",x=1980+(2018-1980)/2,y=max_harv+max_harv/5,
    color="#2E382E",family="rob",label="Area harvested",
    size=12,hjust=0.5,vjust=1
  )+
  annotate("segment",x=1980,xend=1980,y=0,yend=max_harv+max_harv/20,color="black")+
  annotate("segment",x=2018,xend=2018,y=0,yend=max_harv+max_harv/20,color="black")+
  annotate(
    "text",x=1978,y=60000000,
    color="#2E382E",family="cond",label="60 Mha",
    size=10,hjust=1,vjust=0.5
  )+
  annotate(
    "text",x=1978,y=30000000,
    color="#2E382E",family="cond",label="30 Mha",
    size=10,hjust=1,vjust=0.5
  )+
  annotate(
    "text",x=2020,y=y_millet,
    color="#6B0504",family="cond",label="Millet",
    size=9,hjust=0,vjust=0.5, fontface = 'italic'
  )+
  annotate(
    "text",x=2020,y=y_wheat,
    color="#2B50AA",family="cond",label="Wheat",
    size=9,hjust=0,vjust=0.5, fontface = 'italic'
  )+
  geom_point()+
  geom_line()+
  scale_color_manual(values=pal)+
  scale_y_continuous(limits=c(0,max_harv+max_harv/5))+
  scale_x_continuous(limits=c(1970,2028), breaks=c(1980,2018))+
  guides(color='none')+
  theme_minimal()+
  theme(
    plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
    plot.background = element_rect(fill="white",color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x=element_text(family='rob',size=30),
    axis.title.x=element_blank()
  )

yield<-ggplot(
  clean%>%filter(Element=="Yield"),
  aes(x=Year,y=Value,color=Item))+
  annotate(
    "text",x=1980+(2018-1980)/2,y=max_yield+max_yield/5,
    color="#2E382E",family="rob",label="Yield",
    size=12,hjust=0.5,vjust=1
  )+
  annotate("segment",x=1980,xend=1980,y=0,yend=max_yield+max_yield/20,color="black")+
  annotate("segment",x=2018,xend=2018,y=0,yend=max_yield+max_yield/20,color="black")+
  annotate(
    "text",x=1978,y=40000,
    color="#2E382E",family="cond",label="4 t/ha",
    size=10,hjust=1,vjust=0.5
  )+
  annotate(
    "text",x=1978,y=20000,
    color="#2E382E",family="cond",label="2 t/ha",
    size=10,hjust=1,vjust=0.5
  )+
  annotate(
    "text",x=2020,y=y_sorghum,
    color="#E56B6F",family="cond",label="Sorghum",
    size=9,hjust=0,vjust=0.5, fontface = 'italic'
  )+
  annotate(
    "text",x=2020,y=y_maize,
    color="#96CDFF",family="cond",label="Maize",
    size=9,hjust=0,vjust=0.5, fontface = 'italic'
  )+
  geom_point()+
  geom_line()+
  scale_color_manual(values=pal)+
  scale_y_continuous(limits=c(0,max_yield+max_yield/5))+
  scale_x_continuous(limits=c(1970,2028), breaks=c(1980,2018))+
  guides(color='none')+
  theme_minimal()+
  theme(
    plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
    plot.background = element_rect(fill="white",color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x=element_text(family='rob',size=30),
    axis.title.x=element_blank()
  )

# Patchwork!
yield+area+
  plot_annotation(
    title = "Why cereal production is increasing in South Asia",
    subtitle = 
'As shown by Hannah Ritchie, the amount of grain produced in South Asia has increased over the past few decades,\n
although the cultivated area has not changed much. Two factors can explain this observation. First, the yields of\n
all cereal crops have increased (left plot). But there is also a trend towards the substitution of more "rustic"\n
crops by more productive ones (right plot). For example, sorghum may be replaced by maize for cattle feeding.',
    caption = 'Data: FAO | Plot: @BjnNowak'
  )&
  theme(
    plot.margin = margin(0.5,0.5,0.5,0.5,unit='cm'),
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(size=40,hjust=0,family = "play",face='bold',color='#2E382E'),
    plot.subtitle = element_text(size=25,hjust=0,family = "cond",lineheight=0.2,margin=margin(0.15,0,0,0,'cm'),color="#2E382E"),
    plot.caption = element_text(size=20,hjust=0,family='cond',color="#2E382E"),
    plot.caption.position = "panel"
  )
