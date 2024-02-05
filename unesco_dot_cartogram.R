library(tidyverse)
library(rnaturalearth)
library(sf)
library(packcircles)
library(camcorder)
library(showtext)
library(ggtext)

# Set fonts
font_add_google("Cabin","cab")
font_add_google("Raleway","ral")
font_add_google("Playfair Display","play")
font_add_google("Fira Sans","fira")
font_add_google("Jost","jost")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 25, 
  units = "cm", 
  dpi = 300 
)

# Data preparation
##################

# Load tidytuesday / UNESCO data
heritage <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv')
colnames(heritage)<-c('admin','nb_2004','nb_2022')

# Load countries maps
swe <- ne_states(country="Sweden",returnclass = 'sf')%>%
  st_union()%>%
  st_as_sf()%>%
  st_transform(crs="EPSG:3152")%>%
  mutate(admin="Sweden")%>%
  select(admin)

nor <- ne_states(country="Norway",returnclass = 'sf')%>%
  filter(type_en=="County")%>%
  st_union()%>%
  st_as_sf()%>%
  st_transform(crs="EPSG:3152")%>%
  mutate(admin="Norway")%>%
  select(admin)

den <- ne_states(country="Denmark",returnclass = 'sf')%>%
  st_union()%>%
  st_as_sf()%>%
  st_transform(crs="EPSG:3152")%>%
  mutate(admin="Denmark")%>%
  select(admin)

wrld<-swe%>%
  rbind(nor)%>%
  rbind(den)

# Join UNESCO data to map
wrld<-wrld%>%
  left_join(heritage)

# Make dot cartogram
####################
# "tidy" adaptation of this script by Nicolas Lambert:
# https://neocarto.github.io/dotcartogram/

# Extract centroids
cent<-wrld%>%
  st_centroid()%>%
  select(admin,nb_2004,nb_2022)%>%
  mutate(diff=nb_2022-nb_2004)

# Quick dot valued map
ggplot()+
  geom_sf(wrld,mapping=aes(geometry=x))+
  geom_sf(cent,mapping=aes(geometry=x,size=nb_2022))

# Extract centroid coordinates
coord<-cent%>%
  st_coordinates()%>%
  bind_cols(v1=cent$nb_2004,v2=cent$diff,admin=cent$admin)

# Create one row per case:
# Create empty tibble
dots<-tibble(x=c(),y=c(),v=c(),admin=c(),yr=c())
# Fill tibble
for (i in 1:dim(coord)[1]){

  dots <- dots%>%
    rbind(tibble(
      x=rep(coord$X[i],coord$v1[i]),
      y=rep(coord$Y[i],coord$v1[i]),
      v=1,
      admin=rep(coord$admin[i],coord$v1[i]),
      yr=2004
    ))%>%
    rbind(tibble(
      x=rep(coord$X[i],coord$v2[i]),
      y=rep(coord$Y[i],coord$v2[i]),
      v=1,
      admin=rep(coord$admin[i],coord$v2[i]),
      yr=2022
    ))
  
}

# Some parameters
radius = 30000
itermax = 10

dots$x <- jitter(dots$x)
dots$y <- jitter(dots$y)
dots$v <- radius

# Compute each point centroid position
simulation <- circleRepelLayout(
  x = dots, xysizecols = 1:3,
  wrap = FALSE, sizetype = "radius",
  maxiter = itermax, weights =1)$layout

# Convert point to "circle" polygon
circles <- simulation%>%
  sf::st_as_sf(coords =c('x', 'y'),crs = sf::st_crs(wrld))%>%
  st_buffer(dist = radius)%>%
  bind_cols(admin=dots$admin,yr=dots$yr)

# Make map
##########
pal_countries <- c(
  "Sweden"="#DB5461",
  "Norway"="#2191FB",
  "Denmark"="#1B998B"
)

pal_year <- c(
  "2004" = alpha("white",0),
  "2022" = "white"
)

ggplot()+
  geom_sf(
    wrld, 
    mapping=aes(geometry=x),
    fill="#172B3A",color=alpha("#FFF8F0",0.1)
  )+
  geom_sf(
    circles, 
    mapping=aes(geometry=geometry, fill=admin,color=as.factor(yr)),
    lwd=1.5
  )+
  scale_fill_manual(values=pal_countries)+
  scale_color_manual(values=pal_year)+
  guides(fill='none',color='none')+
  labs(
    title='**UNESCO World Heritage Sites**',
    subtitle='Number of sites registered in <b><span style="color:#1B998B">Denmark</span></b>, <b><span style="color:#2191FB">Norway</span></b> or <b><span style="color:#DB5461">Sweden</span></b>.<br>
    Circles with **white borders** were registered after 2004.',
    caption="<br>**Data** UNESCO **| Plot** Benjamin Nowak"
  )+
  theme_void()+
  theme(
    plot.margin = margin(0,1,0,1,'cm'),
    plot.background = element_rect(fill="#0d1821",color=NA),
    plot.title = element_markdown(family='play',size=65, color="white"),
    plot.subtitle = element_markdown(family='ral',size=40, color="white",lineheight=0.40),
    plot.caption = element_markdown(family='ral',size=35, color="white", hjust=0,lineheight=0.3)
  )
