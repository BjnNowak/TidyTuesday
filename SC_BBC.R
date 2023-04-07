library(tidyverse)
library(lubridate)
library(patchwork)
library(ggridges)
library(sf)
library(patchwork)
library(ggtext)

# Load data
hex <- read_sf('maps/hex_grid_clipped_wheat.shp')


ggplot()+
  #geom_sf(hex,mapping=aes(geometry=geometry),fill=NA,color="black")+
  geom_sf(hex,mapping=aes(fill=per_wh),color=NA)+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal,
    breaks = c(5,10,15,20,25),
    labels = glue::glue("{c(5,10,15,20,25)}%"),
    na.value = "grey95",
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "vertical",
      barheight = unit(30, units = "mm") , 
      barwidth = unit(5, units = "mm"),
      title.position = 'top',
      label.position = "right"
    ))+
  labs(
    title="Where do the French grow their wheat?",
    subtitle="Percentage of wheat area by hexagon, 2018",
    fill="Wheat area",
    caption="**Data** RPG2018 | **Plot** @BjnNowak")+
  theme_void()+
  theme(
    plot.margin=margin(0.5,1,0.5,1,"cm"),
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(size=20,face="bold"),
    plot.subtitle = element_text(size=16),
    legend.title = element_text(size=13,face="bold"),
    legend.text = element_text(size=11,color="grey30",hjust=1),
    plot.caption = element_markdown(size=8,hjust=0)
  )

ggsave(
  "bbc.png",
  width=20,
  height=20,
  units="cm",
  dpi=300
)
