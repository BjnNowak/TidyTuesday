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

# Set fonts
font_add_google("Open Sans","open")
font_add_google("Acme","acme")
font_add_google("Fira Sans","fira sans")
font_add_google("Fira Sans Condensed","fira")
font_add_google("Bitter","bit")
font_add_google("Staatliches","staat")
font_add_google("Raleway","ral")

showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)


# dataset : 
# https://ghsl.jrc.ec.europa.eu/download.php?ds=pop
# tutorial :
# https://dieghernan.github.io/202205_Unknown-pleasures-R/

pop <- terra::rast('Data/france_pop_l93_2.tif')
pop_it <- terra::rast('Data/italy_pop_l93.tif')
pop_all <- terra::rast('Data/allemagne_pop_l93.tif')


terra::plot(pop_all)

# Rename layer
names(pop) <- "po"
names(pop_it) <- "po"
names(pop_all) <- "po"

nrow(pop)

# Factor to reduce nrow to 100
factor <- round(nrow(pop) / 90)

pop_agg <- aggregate(pop, factor)
pop_it_agg <- aggregate(pop_it, factor)
pop_all_agg <- aggregate(pop_all, factor)

# Replace NAs
pop_agg[is.na(pop_agg)] <- 0
pop_it_agg[is.na(pop_it_agg)] <- 0
pop_all_agg[is.na(pop_all_agg)] <- 0

# Make data frame
pop_df <- as.data.frame(pop_agg, xy = TRUE, na.rm = FALSE)%>%
  mutate(po=case_when(
    po>0~po+1000,
    TRUE~po
  ))

pop_it_df <- as.data.frame(pop_it_agg, xy = TRUE, na.rm = FALSE)%>%
  mutate(po=case_when(
    po>0~po+1000,
    TRUE~po
  ))

pop_all_df <- as.data.frame(pop_all_agg, xy = TRUE, na.rm = FALSE)%>%
  mutate(po=case_when(
    po>0~po+1000,
    TRUE~po
  ))

# Make plots

fr<-ggplot()+
  geom_density_ridges(
    data=pop_df,
    aes(x=x,y=y,group=y,height=po),
    stat="identity",
    scale=30,alpha=1,
    color="white",
    fill="black",
    size = .25,
    rel_min_height=0
  )+
  coord_equal()+
  theme_void()+
  theme(plot.background = element_rect(fill = "black"))

fr

it<-ggplot()+
  geom_density_ridges(
    data=pop_it_df,
    aes(x=x,y=y,group=y,height=po),
    stat="identity",
    scale=30,alpha=1,
    color="white",
    fill="black",
    size = .25,
    rel_min_height=0
  )+
  coord_equal()+
  theme_void()+
  theme(plot.background = element_rect(fill = "black"))

it

all<-ggplot()+
  geom_density_ridges(
    data=pop_all_df,
    aes(x=x,y=y,group=y,height=po),
    stat="identity",
    scale=30,alpha=1,
    color="white",
    fill="black",
    size = .25,
    rel_min_height=0
  )+
  coord_equal()+
  theme_void()+
  theme(plot.background = element_rect(fill = "black"))

all
