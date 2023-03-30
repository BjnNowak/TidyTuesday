library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(sf)
library(terra)
library(ggridges)
library(raster)
library(cartogram)

# Set fonts

font_add_google("Fira Sans","fira sans")
font_add_google("Bitter","bit")
font_add_google("Staatliches","staat")
font_add_google("Jost","jo")

showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load data
###########

# Bertin's map :
#https://transcarto.github.io/rcartograms/TRANSCARTO_cartograms.html

# dataset for population : 
# https://ghsl.jrc.ec.europa.eu/download.php?ds=pop
# Population density in km2
pop <- terra::rast('Data/france_pop_l93_2.tif')
# France map
fr <- read_sf('Data/population-francaise-par-departement-2018.shp')

# Data cleaning
###############

# Keep only metropolitans dep
dom <- c('Guadeloupe','Martinique','Guyane','La Réunion','Mayotte')
metro<-fr%>%filter(departement%!in%dom)%>%
  st_transform(2154) # change crs to lambert 93

# Compute area per dep
metro$area_km2 <- st_area(metro)
metro<-metro%>%
  mutate(density=as.numeric((population/area_km2)*1000000))

quantile(metro$density)

# Rename layer
names(pop) <- "po"

# Factor to reduce nrow to 100
factor <- round(nrow(pop) / 90)
# Aggregate
pop_agg <- aggregate(pop, factor, fun=mean)
# Replace NAs
pop_agg[is.na(pop_agg)] <- 0
# Make data frame
# For Bertin
pop_df <- as.data.frame(pop_agg, xy = TRUE, na.rm = TRUE)%>%
  mutate(po=case_when(
    x>650522&x<660108&y>6853029&y<6866435~2000,
    TRUE~po
  ))
# For joyplot
pop_joy <- as.data.frame(pop_agg, xy = TRUE, na.rm = FALSE)%>%
  mutate(po=case_when(
    po>0~po+1000,
    TRUE~po
  ))

# make sf_object
sf_pop <- st_as_sf(
  pop_df, coords = c("x","y"), crs= 2154)%>%
  mutate(dens=case_when(
    po<20~"A",
    po<70~"B",
    po<140~"C",
    po<200~"D",
    TRUE~"E"
  ))

# Keep only points inside the country
sf_pop_metro <- st_join(sf_pop, metro, left = FALSE, largest = FALSE)

# Dorling cartogram
# Making Dorling cartogram based on pop density
dorl<-cartogram_dorling(metro, weight="density", k = 1, m_weight = 1, itermax = 1000)

# Make map
##########

# Choropleth
pal <- c(
  "#FFEBF2",
  "#FFADCA",
  "#FF70A2",
  "#FF4788",
  "#FF1B6B"
)

ggplot()+
  geom_sf(
    metro,
    mapping=aes(fill=density,geometry=geometry),
    color=alpha("black",0.1))+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal,
    breaks = seq(40,160,40),
    labels = glue::glue("{seq(40,160,40)} inhabitants"),
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "vertical",
      barheight = unit(20, units = "mm") , 
      barwidth = unit(5, units = "mm"),
      title.position = 'top',
      label.position = "right"
    )
  )+
  labs(
    fill="**Population density**<br><span style='color:grey; font-size:30px;'>Inhabitants per km<sup>2</sup></span>"
  )+
  theme_void()+
  theme(
    plot.margin = margin(1,1,0.5,1,"cm"),
    plot.background = element_rect(fill="#1D201F",color=NA),
    legend.position = c(0.92,0.5),
    legend.title = element_markdown(
      family="bit",size=28,color="white",lineheight=0.35,
      margin=margin(0,0,-0.35,0,"cm")
    ),
    legend.text = element_text(
      family="fira",size=20,color=alpha("white",0.8),
      margin=margin(0,0,0,-0.25,"cm")
    )
  )

# Jacques Bertin
ggplot()+
  geom_sf(
    metro,
    mapping=aes(geometry=geometry),
    color="black",
    fill="#c6f8ff")+
  geom_sf(
    metro,
    mapping=aes(geometry=geometry),
    color=alpha("black",0.1),
    fill="#c6f8ff")+
  geom_sf(
    sf_pop_metro,
    mapping=aes(geometry=geometry,size=dens),
    pch=21,color=alpha("white",0.8),fill="#ff1b6b",stroke=0.1
  )+
  scale_size_manual(
    values=c(0.5,1,1.5,2,2.6),
    label=c("<20 inhabitants","20 to 69","70 to 139","140 to 199","≥200 inhabitants")
    )+
  labs(
    #title="Population density in France",
    #subtitle="in the way of Jacques Bertin",
    size="**Population density**<br><span style='color:grey; font-size:30px;'>Inhabitants per km<sup>2</sup></span>",
    #caption="**Data** GHSL **| Plot** @BjnNowak"
  )+
  theme_void()+
  theme(
    plot.margin = margin(1,1,0.5,1,"cm"),
    plot.background = element_rect(fill="#1D201F",color=NA),
    legend.position = c(0.92,0.5),
    legend.title = element_markdown(
      family="bit",size=28,color="white",lineheight=0.35,
      margin=margin(0,0,-0.35,0,"cm")
    ),
    legend.text = element_text(
      family="fira",size=20,color="white",
      margin=margin(0,0,0,-0.45,"cm")
    ),
    plot.title = element_text(
      family = "staat",size=60,hjust=0.5,color="grey20"
    ),
    plot.subtitle = element_text(
      family = "fira",size=40,hjust=0.5,color="grey40"
    ),
    plot.caption = element_markdown(
      family="jo",size=20,color="grey20",
      hjust=0.5,lineheight=0.35,
      margin=margin(-0.5,0,0,0,"cm")
    )
  )

# Joyplot
ggplot()+
  geom_density_ridges(
    data=pop_joy,
    aes(x=x,y=y,group=y,height=po),
    stat="identity",
    scale=30,alpha=1,
    color="#c6f8ff",
    fill="#1D201F",
    size = .25,
    rel_min_height=0
  )+
  coord_equal()+
  theme_void()+
  theme(plot.background = element_rect(fill = "#1D201F",color=NA))

# Dorling cartogram
dorl<-cartogram_dorling(metro, weight="population", k = 1, m_weight = 1, itermax = 1000)

ggplot()+
  geom_sf(
    metro,
    mapping=aes(geometry=geometry),
    color="black",
    fill="#c6f8ff")+
  geom_sf(
    metro,
    mapping=aes(geometry=geometry),
    color=alpha("black",0.1),
    fill="#c6f8ff")+
  geom_sf(
    dorl,mapping=aes(geometry=geometry,fill=density),
    color="white")+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal,
    breaks = seq(40,160,40),
    labels = glue::glue("{seq(40,160,40)} inhabitants"),
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "vertical",
      barheight = unit(20, units = "mm") , 
      barwidth = unit(5, units = "mm"),
      title.position = 'top',
      label.position = "right"
    )
  )+
  labs(
    fill="**Population density**<br><span style='color:grey; font-size:30px;'>Inhabitants per km<sup>2</sup></span>"
  )+
  theme_void()+
  theme(
    plot.margin = margin(1,1,0.5,1,"cm"),
    plot.background = element_rect(fill="#1D201F",color=NA),
    legend.position = c(0.92,0.4),
    legend.title = element_markdown(
      family="bit",size=28,color="white",lineheight=0.35,
      margin=margin(0,0,-0.35,0,"cm")
    ),
    legend.text = element_text(
      family="fira",size=20,color=alpha("white",0.8),
      margin=margin(0,0,0,-0.25,"cm")
    )
  )
 
# Cartogram
carto <- cartogram_cont(metro, "density", prepare = "none", itermax = 6, maxSizeError = 1.15)

ggplot()+
  geom_sf(
    carto,mapping=aes(geometry=geometry,fill=density),
    color="white")+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal,
    breaks = seq(40,160,40),
    labels = glue::glue("{seq(40,160,40)} inhabitants"),
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "vertical",
      barheight = unit(20, units = "mm") , 
      barwidth = unit(5, units = "mm"),
      title.position = 'top',
      label.position = "right"
    )
  )+
  labs(
    fill="**Population density**<br><span style='color:grey; font-size:30px;'>Inhabitants per km<sup>2</sup></span>"
  )+
  theme_void()+
  theme(
    plot.margin = margin(1,1,0.5,1,"cm"),
    plot.background = element_rect(fill="#1D201F",color=NA),
    legend.position = c(0.92,0.6),
    legend.title = element_markdown(
      family="bit",size=28,color="white",lineheight=0.35,
      margin=margin(0,0,-0.35,0,"cm")
    ),
    legend.text = element_text(
      family="fira",size=20,color=alpha("white",0.8),
      margin=margin(0,0,0,-0.25,"cm")
    )
  )

# Olson cartogram
olson<-cartogram_ncont(metro, "population", k = 1, inplace = TRUE)

ggplot()+
  geom_sf(
    metro,
    mapping=aes(geometry=geometry),
    color="black",
    fill="#c6f8ff")+
  geom_sf(
    metro,
    mapping=aes(geometry=geometry),
    color=alpha("black",0.1),
    fill="#c6f8ff")+
  geom_sf(
    olson,mapping=aes(geometry=geometry,fill=density),
    color="white")+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal,
    breaks = seq(40,160,40),
    labels = glue::glue("{seq(40,160,40)} inhabitants"),
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "vertical",
      barheight = unit(20, units = "mm") , 
      barwidth = unit(5, units = "mm"),
      title.position = 'top',
      label.position = "right"
    )
  )+
  labs(
    fill="**Population density**<br><span style='color:grey; font-size:30px;'>Inhabitants per km<sup>2</sup></span>"
  )+
  theme_void()+
  theme(
    plot.margin = margin(1,1,0.5,1,"cm"),
    plot.background = element_rect(fill="#1D201F",color=NA),
    legend.position = c(0.92,0.6),
    legend.title = element_markdown(
      family="bit",size=28,color="white",lineheight=0.35,
      margin=margin(0,0,-0.35,0,"cm")
    ),
    legend.text = element_text(
      family="fira",size=20,color=alpha("white",0.8),
      margin=margin(0,0,0,-0.25,"cm")
    )
  )
