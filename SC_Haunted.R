library(tidyverse)
library(sf)
library(showtext)
library(camcorder)

# Set fonts
font_add_google("Cinzel","cin")

# Loading font awesome icons
# tuto Albert Rapp: https://albert-rapp.de/posts/ggplot2-tips/08_fonts_and_icons/08_fonts_and_icons.html
# First argument = name in R
# Second argument = path to .otf-file
font_add('fa-reg', 'otfs/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'otfs/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20*7600/5385, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

haunted_places <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv'
  )%>%
  mutate(ct=1)%>%
  group_by(state_abbrev)%>%
  summarize(sm=sum(ct))



# Hex files for USA from R Graph Gallery
# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.
# Load file
map <- read_sf("Data/Bees/data/us_states_hexgrid.geojson")%>%
  filter(iso3166_2!='HI')%>%
  filter(iso3166_2!='AK')%>%
  st_transform(crs="+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

txt_only <- map%>%st_drop_geometry()

cent <- map%>%
  st_centroid()%>%
  st_coordinates()

clean <- txt_only%>%
  bind_cols(cent)%>%
  left_join(haunted_places,by=c('iso3166_2'='state_abbrev'))%>%
  mutate(label="<span style='font-family:fa-solid'>&#xf6e2;</span>")

col_gh <- "#F36E21"
col_bo <- "#F9EB4B"

ggplot()+
  geom_sf(
    map,mapping=aes(geometry=geometry),
    fill=NA, color=col_bo, alpha=0.05,lwd=0.05
  )+
  ggtext::geom_richtext(
    clean, mapping=aes(x=X,y=Y,label=label,size=sm),
    family='fontawesome-webfont', 
    color= col_gh,
    label.colour = NA, fill = NA
  )+
  geom_text(
    clean, mapping=aes(x=X,y=Y,label=iso3166_2),
    family='cin', color= "white",size=30,alpha=0.15
  )+
  guides(size='none')+
  scale_size(range=c(5,80))+
  coord_sf(clip = 'off')+
  theme_void()+
  theme(
    plot.margin=margin(1,0,1,0,'cm'),
    plot.background = element_rect(fill="#3E2469",color=NA)
  )

