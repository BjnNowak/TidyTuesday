library(tidyverse)
library(camcorder)
library(showtext)
library(glue)
library(ggtext)


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Fonts
font_add_google("Open Sans","open")
font_add_google("Bebas Neue","bebas")
font_add_google("League Spartan","spart")
font_add_google("Fira Sans","fira")
font_add_google("Fira Sans Extra Condensed","cond")
showtext_auto()

main <- tibble(
  type = c("Fossile fuel","Pedologic pool","Atmospheric pool"),
  xmin = 0,
  xmax = 1,
  ymin=c(0,4130,6630),
  ymax = c(4130,6630,7390)
) 

sub <- tibble(
  type = c(
    rep("Fossile fuel",2),
    rep("Pedologic pool",2),
    "Atmospheric pool"
  ),
  stock = c(
    rep("4130 Pg",2),
    rep("2500 Pg",2),
    "760 Pg"
  ),
  sub = c(
    'Coal','Others<br>(Oil...)',
    'Organic carbon','Inorganic carbon',
    NA
  ),
  level=c(
    3,1,
    3,1,
    3
  ),
  xmin = c(
    0,3510/4130,
    0,1550/2500,
    0
  ),
  xmax = c(
    3510/4130,1,
    1550/2500,1,
    1
  ),
  ymin = c(
    rep(0,2),
    rep(4130,2),
    6630
  ),
  ymax = c(
    rep(4130,2),
    rep(6630,2),
    7390
  )  
)

sub <- sub%>%
  mutate(label=glue("**{type}** ({stock})"))

border <- '#FFFBFF'

pal<-c(
  "Fossile fuel" = '#0F1A20',
  "Pedologic pool" = '#804A23',
  "Atmospheric pool" = '#9dd7ef'
)

pal_color<-c(
  "Fossile fuel" = border,
  "Pedologic pool" = border,
  "Atmospheric pool" = 'black'
)

ggplot(data=sub)+
  geom_rect(
    aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=type,alpha=level),
    color=border
  )+
  geom_richtext(
    data=sub%>%filter(level==3),
    aes(x = xmin+0.02, y = ymax-150, label = label, color = type),
    inherit.aes=TRUE,
    family='fira',size=14,
    hjust=0,vjust=1,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  geom_richtext(
    data=sub,
    aes(x = xmin+0.02, y = ymax-450, label = sub, color = type),
    inherit.aes=TRUE,
    family='cond',size=10,
    hjust=0,vjust=1,lineheight=0.45,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  guides(
    fill='none',
    alpha='none',
    color='none'
  )+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=pal_color)+
  scale_alpha(range=c(0.7,1))+
  theme_void()
