library(tidyverse)
library(ggstream)
library(patchwork)
library(camcorder)
library(showtext)
library(ggtext)
library(MoMAColors)
library(MetBrewer)

# Set fonts
font_add_google("Raleway","ral")
font_add_google("Maitree","mait")
font_add_google("Suez One","suez")
font_add_google("Open Sans","open")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  height = 24, 
  width = 6.75*2, 
  units = "cm", 
  dpi = 300 
)

# Plot design inspired by:
# https://r-graph-gallery.com/web-streamchart-with-ggstream.html

# Load data
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv')

# Clean data
clean<-data%>%
  mutate(
    dt=lubridate::mdy(data$Date)
  )%>%
  drop_na(Weight)%>%
  filter(Weight<10)

clean$yr<-lubridate::year(clean$dt)
clean$ym<-format_ISO8601(clean$dt, precision = "ym")

# Data preparation: estimation of weight for each type of weight:
# Rough item weight estimation (in g)
w_PlasticBottles = 30
w_Polystyrene = 5 # Yoghurt
w_CigaretteButts = 0.22
w_GlassBottles = 750
w_PlasticBags = 5
w_Wrappers = 5
w_SportBalls = 400


cpl<-clean%>%
  # Convert number to weight for each item
  mutate(
    pb=PlasticBottles*w_PlasticBottles,
    pl=Polystyrene*w_Polystyrene,
    cb=CigaretteButts*w_CigaretteButts,
    gb=GlassBottles*w_GlassBottles,
    pg=PlasticBags*w_PlasticBags,
    wr=Wrappers*w_Wrappers,
    sb=SportsBalls*w_SportBalls
  )%>%
  # Compute amount collected per month
  group_by(ym,Name)%>%
  summarize(
    pb=sum(pb),
    pl=sum(pl),
    cb=sum(cb),
    gb=sum(gb),
    pg=sum(pg),
    wr=sum(wr),
    sb=sum(sb),
  )%>%
  ungroup()%>%
  # Fill empty case
  complete(
    ym,Name, 
    fill=list(pb=0.01,pl=0.01,cb=0.01,gb=0.01,pg=0.01,wr=0.01,sb=0.01)
  )%>%
  mutate(ym=ym(ym))%>%
  # Pivot to long
  pivot_longer(!c(ym,Name),names_to="type",values_to="value")%>%
  mutate(dt_num=as.numeric(ym))

# Make plot:
############

# Set color palette
pal<-met.brewer("Hiroshige", n=7, type="continuous")

temp<-pal[1]
pal[1]<-pal[3]
pal[3]<-temp

bck <- "#162521"

# Select a few years for x axis
dt_sub<-ymd(c("2015-01-01","2017-01-01","2019-01-01","2021-01-01","2023-01-01"))
dt<-cpl%>%
  filter(ym%in%dt_sub)

# Order devices from oldest to newest
cpl$Name <-as.factor(cpl$Name)
cpl$Name <- factor(cpl$Name,levels=c("Mister Trash Wheel", "Professor Trash Wheel","Captain Trash Wheel",  "Gwynnda Trash Wheel"))

# Prepare labels:
texts <- tibble(
  dt_num = c(17167),
  value = c(200000),
  Name = c("Gwynnda Trash Wheel"),
  text = c(
    'Whatever the year and the device, <b><span style="color:#E76254;">plastic bottles</span></b> are always the main thrash collected.'
  )
)

theme_cust<-theme(
  plot.title = element_text(
    size = 60, face = "bold",
    hjust=0.5,family='suez',
    color="white",
    margin=margin(0.5,0,0.5,0,'cm') 
  ),
  plot.subtitle = element_markdown(
    size = 25,
    hjust=0,family='mait',
    color="white",lineheight = 0.45,
    margin=margin(0,0,0.5,0.5,'cm') 
  ),
  plot.caption = element_markdown(
    size = 18, family='mait',
    color = "white",
    hjust = .5,
    margin = margin(0.5, 0, 0.25, 0, 'cm')
  ),
  strip.text = element_text(
    color="white",size=28,hjust=0,
    face='bold', family="ral",
    margin=margin(0,0,0.5,0.5,'cm')  
  ),
  axis.text.x= element_text(
    size=30,color="white",
    family='open'
  ),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title = element_blank(),
  plot.background = element_rect(fill = bck, color = NA),
  panel.background = element_rect(fill = NA, color = NA),
  panel.grid = element_blank(),
  panel.spacing.y = unit(1, "lines"),
  strip.background = element_blank()
)



ggplot(cpl,aes(x=dt_num,y=value,color=type,fill=type))+
  geom_stream(
    geom = "contour",
    color = "white",
    size = .75,extra_span = .1, true_range = "none",
    bw = .65 # Controls smoothness
  ) +
  geom_stream(
    geom="polygon",
    bw=.65, extra_span = .1, true_range = "none"
  )+
  geom_vline(
    data = dt,
    aes(xintercept = dt_num),
    inherit.aes = FALSE, 
    color = alpha(bck,0.25), 
    size = .25,
    linetype = "dashed"
  ) +
  geom_textbox(
    data = texts,
    aes(
      x=dt_num,y=value, 
      label = text
    ),
    family = "mait", size = 6.5, color='white',
    maxwidth = unit(7.25, "lines"),
    hjust = 0,lineheight=0.45,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt"), 
    box.colour = NA
  )+
  facet_wrap( 
    as.factor(Name)~., ncol = 1,
    scales = "free_y"
  )+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=pal)+
  scale_x_continuous(
    breaks=as.numeric(levels(as.factor(dt$dt_num))),
    labels=seq(2015,2023,2)
  )+
  guides(color='none',fill='none')+
  labs(
    title = "Plastic in the water",
    subtitle = "The graph below shows the estimated weight of different types of waste collected<br>by one of the four **Trash Wheel** devices in the water of Baltimore harbor.<br>Types of waste are: <b><span style='color:#FFD06F;'>cigarette butts</span></b>, <b><span style='color:#F39A4F;'>glass bottles</span></b>, <b><span style='color:#E76254;'>plastic bottles</span></b>, <b><span style='color:#D4E1CB;'>plastic bags</span></b>,<br><b><span style='color:#72BCD5;'>polystyrene</span></b>, <b><span style='color:#447BA1;'>sport balls</span></b> or <b><span style='color:#1E466E;'>wrappers</span></b>.",
    caption = "**Data** Trash Wheel Collection Data **| Plot** Benjamin Nowak"
  )+
  theme_cust

