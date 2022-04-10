
library(tidyverse)
library(ggtext)
library(camcorder)
library(showtext)
library(maps)
library(sf)
library(cowplot)
library(grid)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Calistoga","cal")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 29.7, 
  height = 18, 
  units = "cm", 
  dpi = 300 
)

# Create data table
data <- tibble(
  period=c(0,1,2,3,4,5,6),
  # Year (before present)
  year = c(-11000,-10000,-5000,-3000,-1000,-85,0),
  # Population (millions inhabitants)
  pop = c(4.5,5,50,100,250,1600,7800),
  next_rev = c(
'',
'**Slash and burn**<br>
Allows to feed much higher population densities<br>
(10 to 30 inhabitants per km<sup>2</sup>) than previous<br>
predatory systems.',
'**Hydro-agricultural civilizations**<br>
Floodplain and irrigation cropping systems of the<br>
Indus, Mesopotamia and Nile valleys. Small extent<br>
but high efficiency (100 inhabitants per km<sup>2</sup>).',
'**Flooded rice cultivation**<br>
Development of hydraulic systems for rice cultivation<br>
in the deltas of China, India and Southeast Asia.',
'**Agricultural revolution of the Middle Ages**<br>
Heavy ploughing and manure transportation with animal<br>
traction allow to quadruple the European population<br>
in the Middle Ages.',
'**Green revolution**  Modern crops, inputs and mechanization',
''
)
)

# Preparing world map
states <- st_as_sf(maps::map(database="world", plot = FALSE, fill = TRUE))
country_to_remove <- c(
  'Antarctica','Greenland', 'French Southern and Antarctic Lands'
)
# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# To allow dissolve with broken geometries
sf::sf_use_s2(FALSE)
# Dissolve world map
states <- states %>%
  filter(ID %!in% country_to_remove)%>%
  mutate(entity="World")%>%
  group_by(entity)%>%
  summarise()%>%
  ungroup()%>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=270"))

# Make world map
world<-ggplot()+
  geom_sf(
    data=states,fill="#f4a261",
    color=scales::alpha("black",0),size=0
  )+
  theme_void()+
  theme(
    plot.background = element_rect(fill=NA,color=NA),
    plot.title = element_text(
      family='cal',size=120,margin=margin(0.2,0,0.5,0, unit = "cm"),
      angle=0,vjust=1,hjust=0.5,face='bold'),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank()
  )

# Make populatopn chart
pop<-ggplot(data,aes(y=pop,x=year))+
  geom_area(alpha=0.5,fill="#2a9d8f")+
  geom_point(
    data=data%>%filter(period!=0&period!=6),
    col="#2a9d8f",size=5)+
  scale_x_continuous(
    breaks=c(-10000,-7500,-5000,-2500,0),
    labels=c("-10,000 years\nbefore present","-7,500","-5,000","-2,500","Present\ndays"),
    expand = c(0, 0)
  )+
  scale_y_continuous(
    trans='log2',
    breaks=c(7,70,700,7000),
    labels = c("7M","70M","700M","7,000M"),
    expand = c(0, 0),position='right')+
  labs(
    y="World population (million inhabitants)"
  )+
  theme_minimal()+
  theme(
    plot.background = element_rect(fill=NA,color=NA),
    plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x=element_text(
      family='cond',size=50,margin=margin(0.2,0,0.2,0, unit = "cm"),
      angle=0,vjust=0.5,
      hjust=c(0.5,0.5,0.5,0.5,1),
      lineheight=0.35),
    axis.title.x=element_blank(),
    axis.text.y.right=element_text(
      family='cond',size=50,margin=margin(0.2,0,0.2,0, unit = "cm"),
      angle=270,vjust=0.5,hjust=0),
    axis.title.y.right=element_blank()

  )

# Set text and colors before assembling plots

tot<-tibble(
  lab=c(
    '1. Slash and burn',
    '2. Hydro-agricultural civilizations',
    '3. Flooded rice cultivation',
    '4. Agricultural revolution of the Middle Ages',
    '5. Green revolution'
  ),
  x=c(0.05,0.36,0.63,0.48,0.09),
  y=c(0.40,0.455,0.50,0.71,0.705)
)

sub <- tibble(
  lab = c(
"Allows to feed much higher population\n
densities (10 to 30 inhabitants per km2)\n
than previous predatory systems.\n
First developed in Mexico,\n
the Middle East and China.",
"Floodplain and irrigation cropping systems\n
of the Indus, Mesopotamia and Nile valleys.\n
Small extent but high efficiency\n 
(100 inhabitants per km2).",
"Development of hydraulic systems\n
for rice growing in the deltas of,\n
China, India and Southeast Asia.",
"Heavy ploughing and manure transportation with animal traction\n
allow to quadruple the European population in the Middle Ages.",
"Modern crop varieties, chemical inputs\n
and mechanization. Started in Mexico\n
with the selection of dwarf wheat\n
varieties by Norman Borlaug."
))

col_line <- "#2a9d8f"
alpha_line <- 0.2
size_line <- 3

col_text <- '#402E2A'

col_back <- "#F7F7FF"

# Set color backgrounds
back <- rectGrob(
  x = unit(0, "cm"),
  y = unit(0, "cm"),
  width = unit(29.7, "cm"),
  height = unit(21, "cm"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = col_back, alpha = 1,col=NA)
)

back_tit <- rectGrob(
  x = unit(0, "cm"),
  y = unit(21, "cm"),
  width = unit(29.7, "cm"),
  height = unit(6, "cm"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "#1F2041", alpha = 1,col=NA)
)

# Assemble plots
ggdraw()+
  draw_grob(back)+
  draw_grob(back_tit)+
  draw_plot(world,x=0.05,width=0.9,height=1)+
  draw_plot(pop,x=0.03,y=0.02,width=0.94,height=0.6)+
  # Title
  draw_text(
    "Major farming systems (r)evolutions", 
    x=0.5,y=0.92,
    hjust=0.5,vjust=0,lineheight=0.35,
    color = col_back, size = 90, 
    family = 'cal',fontface='bold')+
  # Subtitle
  draw_text(
    "The development of farming systems has supported world population growth over time.", 
    x=0.5,y=0.89,
    hjust=0.5,vjust=1,lineheight=0.35,
    color = col_back, size = 65, 
    family = 'cond')+
  # Legends
  #########
  # Lines
  # Slash and burn
  draw_line(x=c(0.13,0.27),y=c(0.22,0.54),size=size_line,color=col_line,alpha=alpha_line)+
  draw_line(x=c(0.13,0.60),y=c(0.22,0.55),size=size_line,color=col_line,alpha=alpha_line)+
  draw_line(x=c(0.13,0.75),y=c(0.22,0.575),size=size_line,color=col_line,alpha=alpha_line)+
  
  # Hydro
  # Egypte + Mesopotamia
  draw_line(x=c(0.535,0.58),y=c(0.345,0.56),size=size_line,color=col_line,alpha=alpha_line+0.15)+
  # Indus
  draw_line(x=c(0.535,0.67),y=c(0.345,0.55),size=size_line,color=col_line,alpha=alpha_line+0.15)+
  
  # Flooded rice
  # India
  draw_line(x=c(0.695,0.68),y=c(0.38,0.52),size=size_line,color=col_line,alpha=alpha_line+0.3)+
  draw_line(x=c(0.695,0.75),y=c(0.38,0.555),size=size_line,color=col_line,alpha=alpha_line+0.3)+
 
  # Middle ages
  draw_line(x=c(0.855,0.80,0.485),y=c(0.42,0.635,0.635),size=size_line,color=col_line,alpha=alpha_line+0.45)+
  
  # Green revolution
  draw_line(x=c(0.930,0.85,0.65,0.38, 0.27),y=c(0.51,0.80,0.80,0.80,0.54),size=size_line,color=col_line,alpha=alpha_line+0.60)+
  
  # Text
  draw_text(
    tot$lab, 
    x=tot$x,y=tot$y,
    hjust=0,vjust=0,lineheight=0.35,
    color = col_text, size = 50, 
    family = 'cond',fontface='bold')+
  draw_text(
    sub$lab, 
    x=tot$x,y=tot$y-0.01,
    hjust=0,vjust=1,lineheight=0.15,
    color = col_text, size = 40, 
    family = 'cond')+
  draw_text(
   "Evolution of world population", 
   x=0.5,y=0.22,
   hjust=0.5,vjust=0,lineheight=0.15,
   color = col_back, size = 70, 
   family = 'fira',fontface='bold')+
  draw_text(
    "Million inhabitants (logarithmic scale)", 
    x=0.5,y=0.20,
    hjust=0.5,vjust=1,lineheight=0.15,
    color = col_back, size = 55, 
    family = 'cond')+
  # Caption
  draw_text(
    "Data: Mazoyer and Roudart (2002)\n
    Plot: @BjnNowak", 
    x=0.925,y=0.20,
    hjust=1,vjust=1,lineheight=0.15,
    color = col_back, size = 40, 
    family = 'cond')










