# Load packages
library(tidyverse)
library(sf) # To make maps

# Idea of suppressive bivariate palette based on this article:
# https://medium.com/@uwdata/value-suppressing-uncertainty-palettes-426130122ce9

# Load map of France made of several hexagons
hex<-read_sf('https://github.com/BjnNowak/frex_db/raw/main/map/hex_grid.gpkg')

# Create dummy variables
########################

# Number of hexagons on the map
nb_hex<-dim(hex)[1]

# Dummy data for each hexagon for ten years (from 2010 to 2019)
data<-tibble(
  hex_id=rep(hex$hex_id,10),
  year=c(
    rep(2010,nb_hex),
    rep(2011,nb_hex),
    rep(2012,nb_hex),
    rep(2013,nb_hex),
    rep(2014,nb_hex),
    rep(2015,nb_hex),
    rep(2016,nb_hex),
    rep(2017,nb_hex),
    rep(2018,nb_hex),
    rep(2019,nb_hex)
  ),
  # Severity note
  note=rep(runif(n=nb_hex, min=1, max=10),10),
  # Number of observations
  number_obs=rep(runif(n=nb_hex, min=1, max=10),10)
)


# Prepare the data
###################

# Make categories for each hex
clean <- data%>%
  # Compute mean, standard deviation 
  # and number of observations for each hex
  group_by(hex_id)%>%
  summarize(
    mn_septo=mean(note),
    sd_septo=sd(note),
    tot=sum(number_obs)
  )%>%
  ungroup()%>%
  # Create classes for mean and sd
  mutate(mn_cl=case_when(
    mn_septo<3~"A",
    mn_septo<4~"B",
    mn_septo<5~"C",
    mn_septo<6~"D",
    mn_septo<7~"E",
    mn_septo<8~"F",
    mn_septo<9~"G",
    TRUE~"H"
  ))%>%
  mutate(sd_cl=case_when(
    sd_septo<3~"1",
    sd_septo<5~"2",
    sd_septo<7~"3",
    TRUE~"4"
  ))%>%
  # Combine classes of mean and sd
  mutate(cl=glue::glue('{mn_cl}{sd_cl}'))


# Join map and data
map<-hex%>%
  left_join(clean)

# Make plot
###########

# Color palette
pal<-c(
  "A1"="#f8ce54",
  "B1"="#f2b851",
  "C1"="#eca647",
  "D1"="#df9141",
  "E1"="#d87f3c",
  "F1"="#cc6b38",
  "G1"="#bf5a36",
  "H1"="#b04e3f",
  #2nd row
  "A2"="#f7d278",
  "B2"="#f7d278",
  "C2"="#f0b26e",
  "D2"="#f0b26e",
  "E2"="#e39763",
  "F2"="#e39763",
  "G2"="#d37e61",
  "H2"="#d37e61",
  # 3rd row
  "A3"="#f8d69d",
  "B3"="#f8d69d",
  "C3"="#f8d69d",
  "D3"="#f8d69d",
  "E3"="#eaaf94",
  "F3"="#eaaf94",
  "G3"="#eaaf94",
  "H3"="#eaaf94",
  # 4rd row -> all categories have the same color
  "A4"="#f9e1c8",
  "B4"="#f9e1c8",
  "C4"="#f9e1c8",
  "D4"="#f9e1c8",
  "E4"="#f9e1c8",
  "F4"="#f9e1c8",
  "G4"="#f9e1c8",
  "H4"="#f9e1c8"
)

# Draw map
m <- ggplot()+
  geom_sf(
    hex,
    mapping=aes(geometry=geom),
    fill="grey95",
    color=NA
  )+
  geom_sf(
    map%>%
      st_centroid(),
    mapping=aes(color=cl,size=tot,geometry=geom)
  )+
  scale_size(
    range=c(0.05,2.25)
  )+
  scale_color_manual(values=pal)+
  guides(color='none',size='none')+
  theme_void()+
  theme(plot.background=element_rect(fill="white",color=NA))

m

# Make legend
#############

# Data to draw legend
data<-tibble(
  xmin=c(
    seq(0,14,2),
    seq(0,12,4),
    seq(0,8,8),
    0
  ),
  xmax=c(
    seq(2,16,2),
    seq(4,16,4),
    seq(8,16,8),
    16
  ),
  ymin=c(
    rep(6,8),
    rep(4,4),
    rep(2,2),
    0
  ),
  ymax=c(
    rep(8,8),
    rep(6,4),
    rep(4,2),
    2
  ),
  cl=c(
    "A1","B1",'C1','D1','E1','F1','G1','H1',
    'A2','C2','E2','G2',
    'A3','E3',
    'A4'
  )
)

# Radial legend

l<-ggplot(
    data,
    aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=cl)
  )+
  geom_rect(color='white')+
  scale_fill_manual(values=pal)+
  scale_x_continuous(limits=c(0,16*5))+
  scale_y_continuous(limits=c(0,8))+
  guides(fill='none')+
  coord_polar(start=-pi/5)+
  theme_void()+
  theme(plot.background=element_rect(fill="white",color=NA))

l

# Save plots
ggsave(
  filename="map.png",
  plot=m,
  width = 15, 
  height = 15,
  dpi = 300,
  units = "cm"
)

ggsave(
  "legend.png",
  plot=l,
  width = 15, 
  height = 15,
  dpi = 300,
  units = "cm"
)
