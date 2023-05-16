library(tidyverse)
library(sf)
library(maps)
library(camcorder)
library(showtext)

# Set fonts
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Fira Sans","fira")
showtext_auto()


gg_record(
  dir = file.path(tempdir(), "recording"), 
  device = "png", # we need to set the Cairo device
  width = 20,
  height = 10.5,
  dpi=320,
  unit="cm"
)

# Load data
############
# Load tidy tuesday data
tornados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')
# Load world map
wrld<-map('world',fill=TRUE)%>%
  st_as_sf()

# Data processing
#################

clean<-tornados%>%
  filter(fat>0)%>% # Keep only "deadly" tornadoes
  mutate(
    lat=slat+(elat-slat)/2,
    lon=slon+(elon-slon)/2
  )%>%
  st_as_sf(coords=c("lon","lat"))%>% # make sf object
  st_set_crs(st_crs(wrld)) # set crs

# Make map
##########

# To use later for point size scale 
mn<-min(clean$fat)
mx<-max(clean$fat)

# Map bounding box
xlim=c(-130,-65)
ylim=c(25,52)

# Function to create plot
fun_pl<-function(i){

  pl<- ggplot(clean%>%filter(yr==i),aes(size=fat))+
    geom_sf(
      wrld,
      mapping=aes(geometry=geom),
      fill="#edf2f4",
      inherit.aes=FALSE
    )+
    annotate(
      geom="text",x=-125,y=50,label=i,
      fontface="bold",
      size=40,color="#ffc300",family="ral")+
    geom_sf(alpha=0.5,color="#ef476f")+
    scale_size(range=c(2,15),limits=c(mn,mx),breaks = c(10,50,120))+
    scale_x_continuous(limits=xlim)+
    scale_y_continuous(limits=ylim)+
    guides(
      size = guide_legend(title.position="top", title.hjust = 0)
    )+
    labs(size="Number of fatalities")+
    theme_void()+
    theme(
      legend.direction="horizontal",
      legend.title = element_text(
        size=30,color="white",face="bold",family="fira",
        margin=margin(0,0,-0.5,0,"cm")
      ),
      legend.text = element_text(
        size=30,color="white",family="fira",margin=margin(0,0,0,-0.5,"cm")
      ),
      legend.position = c(0.15,0.15),
      legend.spacing.x = unit(0.25, 'cm'),
      plot.background=element_rect(fill="#293241",color=NA)
    )

  return(pl)

}

# Loop over the years
for (i in (1950:2022)){
  p=fun_pl(i)
  print(p)
}

# Make animation
gg_playback(
  name = file.path(tempdir(), "recording", "tornadoes.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .5,
  image_resize = 1200
)
