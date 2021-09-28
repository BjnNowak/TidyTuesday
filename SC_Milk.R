###############
# Clear space #
###############

rm(list=ls())
gc()

library(tidyverse)
library(camcorder)
library(cowplot)
library(showtext)
library(geofacet)

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21*1.618, 
  height = 21, 
  units = "cm", 
  dpi = 300 
)

# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-01-29
production<-read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv')

# First try
ggplot(
  production, aes(x=year,y=milk_produced))+
  geom_line()+
  facet_geo(~state)+
  theme_minimal()

# First try
ggplot(
  production, aes(x=year,y=milk_produced))+
  geom_line()+
  facet_geo(~state)+
  scale_x_continuous(breaks=c(1970,1990,2010))+
  theme_minimal()

# 100 pounds of milk ~ 44 liters

production <- production %>%
  mutate(milk_liter=milk_produced*(44/100))

# With milk in liters
ggplot(
  production, aes(x=year,y=milk_liter/10^9))+
  geom_line()+
  facet_geo(~state)+
  scale_x_continuous(breaks=c(1970,1990,2010))+
  scale_y_continuous(breaks=c(0,8,16))+
  labs(
    x="",
    y="Milk production (billion liters)"
  )+
  theme_minimal()

# Removing District of Columbia
ggplot(
  production, aes(x=year,y=milk_liter/10^9))+
  geom_line()+
  facet_geo(~state)+
  scale_x_continuous(breaks=c(1970,1990,2010))+
  scale_y_continuous(breaks=c(0,8,16))+
  labs(
    x="",
    y="Milk production (billion liters)"
    )+
  facet_geo(~state, grid = "us_state_without_DC_grid3")+
  theme_minimal()

# Compute percentage produced by each state and by year
production<-production%>%
  dplyr::group_by(year)%>%
  dplyr::mutate(tot=sum(milk_liter))%>%
  ungroup()%>%
  mutate(per=milk_liter/tot*100)

ggplot(
  production, aes(x=year,y=milk_liter/10^9))+
  geom_rect(
    data=production%>%filter(year==2017),
    aes(fill=per),xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,
    inherit.aes = FALSE)+
  scale_fill_gradient(low="#e1e5f2",high="#1f7a8c")+
  geom_line(color="#d1495b")+
  facet_geo(~state)+
  scale_x_continuous(breaks=c(1970,1990,2010))+
  scale_y_continuous(breaks=c(0,8,16))+
  labs(
    x="",
    y="Milk production (billion liters)"
  )+
  facet_geo(~state, grid = "us_state_without_DC_grid3")+
  theme_minimal()

grid <- tibble(
  x=c(1970,1990,2010),
  y=seq(0,12,6)
)

grid <- tibble(
  x=c(1986,2002),
  y=c(7,14)
)


font_add_google("Barlow Condensed", "barlow")
font_add_google("Open Sans", "open")
font_add_google("Playfair Display", "playfair")
showtext_auto()

ggplot(
  production, aes(x=year,y=milk_liter/10^9))+
  geom_rect(
    data=production%>%filter(year==2017),
    aes(fill=per),xmin=1970,xmax=Inf,ymin=0,ymax=Inf,
    inherit.aes = FALSE)+
  geom_segment(
    data=grid,aes(x=x,xend=x),y=0,yend=Inf,color="white"
  )+
  geom_segment(
    data=grid,aes(y=y,yend=y),x=0,xend=Inf,color="white"
  )+
  scale_fill_gradient(
    low="#e1e5f2",high="#1f7a8c",
    breaks=c(5,10,15),
    labels=c("5%","10%","15%"),
    guide=guide_colorbar(
      nbin=1000,
      raster=F, 
      barwidth=10, 
      barheight = 3,
      frame.colour=c('white'),
      frame.linewidth=1, 
      ticks.colour='white',  direction="horizontal",
      title.position = "top")
  )+
  geom_line(color="#dd6e42",size=1)+
  scale_x_continuous(breaks=c(1970,1986,2002))+
  scale_y_continuous(breaks=c(0,7,14))+
  labs(
    title="Milk production in the US from 1971 to 2017",
    subtitle="Background color shows % of country production for each state in 2017",
    x="",
    y="Milk production (billion liters)",
    fill="% of US production in 2017"
  )+
  facet_geo(~state, grid = "us_state_without_DC_grid3")+
  theme_minimal()+
  theme(
    plot.title = element_text(
      margin = margin(0, 0, 0, 0), 
      size = 80, 
      family = "playfair", 
      face = "bold", 
      vjust = -8,
      hjust=0.24,
      color = "grey25"
    ),
    plot.subtitle = element_text(
      margin = margin(0, 0, 0, 0), 
      size = 50, 
      family = "open",
      vjust = -15,
      hjust=0.255
   
    ),
    legend.pos = c(0.9, 0.24),
    legend.title = element_text(family='open',size=40),
    legend.text = element_text(family='open',size=40),
    axis.text.x = element_text(family='open',size=30,angle=90,hjust=1),
    axis.text.y = element_text(family='open',size=30),
    axis.title.y = element_text(family='open',size=45),
    strip.text = element_text(family = "barlow",size=40)
  )


ggsave("milk.png", width = 21*1.618, height = 21, units = "cm")
