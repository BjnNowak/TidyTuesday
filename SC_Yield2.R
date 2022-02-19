
library(tidyverse)
library(camcorder)
library(showtext)
library(ggforce)
library(ggtext)
library(patchwork)

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 30, 
  height = 30, 
  units = "cm", 
  dpi = 300 
)

# Load fonts 
font_add_google("Fira Sans","fira")
font_add_google("Roboto Condensed","roboto")
font_add_google("Passion One","passion")
font_add_google("Ultra","ultra")
# Automatically use {showtext} for plots
showtext_auto()

ft_ax <- 'fira'

# Set colors
front_wheat <- '#FDECD8'
back_wheat <- '#E56B6F'

front_maize <- '#BCF0DA'
back_maize <- '#1C7C54'

front_barley <- '#C3E9DE'
back_barley <- '#355070'

whi<-"#FFFFEB"

# Data from Schauberger et al. (2022) available here:
# https://datapub.gfz-potsdam.de/download/10.5880.PIK.2021.001najvndf/
wheat <- readr::read_delim('Data/wheat_total_filtered.txt',delim=';')
maize <- readr::read_delim('Data/maize_total_filtered.txt',delim=';')
barley <- readr::read_delim('Data/barley_total_filtered.txt',delim=';')
#rape <- readr::read_delim('Data/rape_total_filtered.txt',delim=';')


wheat_clean<-wheat%>%
  mutate(prod=area*yield)%>%
  mutate(decade=case_when(
    year<1930~1920,
    year<1960~1960,
    year<1980~1980,
    year<2000~2000,
    TRUE~2018
  ))%>%
  group_by(decade)%>%
  summarize(
    surf=sum(na.omit(area)),
    pr=sum(na.omit(prod)),
    yield=pr/surf
  )

maize_clean<-maize%>%
  mutate(prod=area*yield)%>%
  mutate(decade=case_when(
    year<1930~1920,
    year<1960~1960,
    year<1980~1980,
    year<2000~2000,
    TRUE~2018
  ))%>%
  group_by(decade)%>%
  summarize(
    surf=sum(na.omit(area)),
    pr=sum(na.omit(prod)),
    yield=pr/surf
  )

barley_clean<-barley%>%
  mutate(prod=area*yield)%>%
  mutate(decade=case_when(
    year<1930~1920,
    year<1960~1960,
    year<1980~1980,
    year<2000~2000,
    TRUE~2018
  ))%>%
  group_by(decade)%>%
  summarize(
    surf=sum(na.omit(area)),
    pr=sum(na.omit(prod)),
    yield=pr/surf
  )


label_y<-wheat_clean$decade
label_x<-round(wheat_clean$yield,1)
label_x<-label_x[-2]
label_x_maize<-round(maize_clean$yield,1)
label_x_barley<-round(barley_clean$yield,1)
label_x_barley<-label_x_barley[-1]

xoff <- 0.15
yoff <- 1.5

# 1st year of y-axis
bottom_year <- 1900

# Coords for vertical polygon
vertical<-wheat_clean%>%
  mutate(
    ax=yield-xoff,
    bx=yield-xoff,
    cx=yield+xoff,
    dx=yield+xoff,
    ay=bottom_year,
    by=decade-yoff,
    cy=decade+yoff,
    dy=bottom_year
  )%>%
  select(decade,ax,bx,cx,dx,ay,by,cy,dy)%>%
  pivot_longer(
    !decade,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )

vertical_maize<-maize_clean%>%
  mutate(
    ax=yield-xoff,
    bx=yield-xoff,
    cx=yield+xoff,
    dx=yield+xoff,
    ay=bottom_year,
    by=decade-yoff,
    cy=decade+yoff,
    dy=bottom_year
  )%>%
  select(decade,ax,bx,cx,dx,ay,by,cy,dy)%>%
  pivot_longer(
    !decade,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )

vertical_barley<-barley_clean%>%
  mutate(
    ax=yield-xoff,
    bx=yield-xoff,
    cx=yield+xoff,
    dx=yield+xoff,
    ay=bottom_year,
    by=decade-yoff,
    cy=decade+yoff,
    dy=bottom_year
  )%>%
  select(decade,ax,bx,cx,dx,ay,by,cy,dy)%>%
  pivot_longer(
    !decade,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )

# Coords for horizontal polygon
horizontal<-wheat_clean%>%
  mutate(
    ax=0,
    bx=yield+xoff,
    cx=yield-xoff,
    dx=0,
    ay=decade+yoff,
    by=decade+yoff,
    cy=decade-yoff,
    dy=decade-yoff
  )%>%
  select(decade,ax,bx,cx,dx,ay,by,cy,dy)%>%
  pivot_longer(
    !decade,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )

horizontal_maize<-maize_clean%>%
  mutate(
    ax=0,
    bx=yield+xoff,
    cx=yield-xoff,
    dx=0,
    ay=decade+yoff,
    by=decade+yoff,
    cy=decade-yoff,
    dy=decade-yoff
  )%>%
  select(decade,ax,bx,cx,dx,ay,by,cy,dy)%>%
  pivot_longer(
    !decade,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )

horizontal_barley<-barley_clean%>%
  mutate(
    ax=0,
    bx=yield+xoff,
    cx=yield-xoff,
    dx=0,
    ay=decade+yoff,
    by=decade+yoff,
    cy=decade-yoff,
    dy=decade-yoff
  )%>%
  select(decade,ax,bx,cx,dx,ay,by,cy,dy)%>%
  pivot_longer(
    !decade,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )


# Wheat
wh <- ggplot()+
  geom_rect(
    data=horizontal%>%filter(point=='c'),
    aes(xmax=x,ymax=y),xmin=0,ymin=1900,fill=front_wheat
  )+
  ggforce::geom_shape(
    data=horizontal,
    aes(x=x,y=y,group=decade),
    fill=front_wheat,color=back_wheat
  )+
  ggforce::geom_shape(
    data=vertical,
    aes(x=x,y=y,group=decade),
    fill=back_wheat,color=back_wheat
  )+
  annotate('segment',x=0,xend=0,y=bottom_year,yend=2018+yoff,color=front_wheat)+
  annotate('text',
           x=6.6,y=2025,label="Wheat",hjust=1,
           size=25,family="passion",color=back_wheat)+
  scale_x_continuous(
    breaks=label_x,limits=c(0,10)
  )+
  scale_y_continuous(
    breaks=label_y,
    labels = c("1900\nto 1920","1920\nto 1960","1960\nto 1980","1980\nto 2000","2000\nto 2018"),
    limits = c(bottom_year,2026)
  )+
  theme_minimal()+
  theme(
    axis.title=element_blank(),
    axis.text=element_text(family=ft_ax,size=40),
    axis.text.y = element_text(vjust=1,lineheight = 0.30,hjust=0.5,margin=margin(0,8,0,0)),
    axis.text.x = element_text(angle=0,margin=margin(0,0,0,0),hjust=0.5,vjust=0.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

wh

# Maize
ma <- ggplot()+
  scale_y_reverse(
    breaks=label_y,
    labels = c("1900\nto 1920","1920\nto 1960","1960\nto 1980","1980\nto 2000","2000\nto 2018"),
    limits = c(2026,bottom_year)
  )+
  annotate(
    geom = 'rect',
    xmin = 0, xmax = 8.5, ymin = bottom_year,ymax=2018,
    fill = front_maize,color=front_maize
  )+
  ggforce::geom_shape(
    data=horizontal_maize,
    aes(x=x,y=y,group=decade),
    fill=front_maize,color=back_maize
  )+
  ggforce::geom_shape(
    data=vertical_maize,
    aes(x=x,y=y,group=decade),
    fill=back_maize,color=back_maize
  )+
  annotate('segment',x=0,xend=0,y=bottom_year,yend=2018+yoff,color=front_maize)+
  annotate('text',
           x=8.4,y=2025,label="Maize",hjust=1,
           size=25,family="passion",color=back_maize)+
  scale_x_continuous(
    breaks=label_x_maize,limits=c(0,10),
    position="top"
  )+

  theme_minimal()+
  theme(
    axis.title=element_blank(),
    axis.text=element_text(family=ft_ax,size=40),
    axis.text.y = element_text(vjust=1,lineheight = 0.30,hjust=0.5,margin=margin(0,8,0,0)),
    axis.text.x = element_text(angle=0,margin=margin(0,0,0,0),hjust=0.5,vjust=0.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )
ma

# Barley
ba <- ggplot()+
  geom_rect(
    data=horizontal_barley%>%filter(point=='c'),
    aes(xmax=x,ymax=y),xmin=0,ymin=1900,fill=front_barley
  )+
  ggforce::geom_shape(
    data=horizontal_barley,
    aes(x=x,y=y,group=decade),
    fill=front_barley,color=back_barley
  )+
  ggforce::geom_shape(
    data=vertical_barley,
    aes(x=x,y=y,group=decade),
    fill=back_barley,color=back_barley
  )+
  labs(
    x='Mean yield for the period <span style="color:dimgrey">(tons per ha)</span>                                                   '
  )+
  annotate('text',
           x=6,y=2024,label="Barley",hjust=0,
           size=25,family="passion",color=back_barley)+
  annotate('segment',x=0,xend=0,y=bottom_year,yend=2018+yoff,color=front_barley)+
  scale_x_reverse(
    breaks=label_x_barley,limits=c(10,0)
  )+
  scale_y_continuous(
    breaks=label_y,
    labels = c("1900\nto 1920","1920\nto 1960","1960\nto 1980","1980\nto 2000","2000\nto 2018"),
    limits = c(bottom_year,2025)
  )+
  theme_minimal()+
  theme(
    axis.title.x=element_markdown(family=ft_ax,size=40,hjust=1),
    axis.title.y=element_blank(),
    axis.text=element_text(family=ft_ax,size=40),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle=0,margin=margin(0,0,4,0),hjust=0.5,vjust=1),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

# Title and sub
fake <- tibble(
  x=c(0,10),
  y=c(0,10)
)

lab<-tibble(
  x=10,
  y=7.8,
  lab="
This plot shows the evolution of yields of three major crops in France **from 1900 to 2018**.
Size of each shape is in part proportional to the mean yield over the given period.
<br><br>
The yields of the three crops were similar in 1900 (about 1.2 t.ha<sup>-1</sup>) and hardly changed until the end of the Second World War.
Yields then increased sharply until the early 1990s for all crops. 
From this date, the trends depend on the crops: **wheat yields tend to stagnate** while **maize yields continue to increase.** 
"
)

cap <- tibble(
  x=9.5,y=1,
  label="**Data:** Schauberger *et al.* (2022)  **| Plot:** @BjnNowak"
)

tit<-ggplot(data=fake,aes(x=x,y=y))+
  scale_x_continuous(limits=c(0,10))+
  scale_y_continuous(limits=c(0,10))+
  annotate(
    "text",x=10,y=9.5,label="The evolution of\ncrop yields in France",
    size=21,family="ultra",hjust=1,vjust=1,lineheight=0.35,
    alpha=1,col="#101919" 
  )+
  geom_textbox(
    data=lab,
    aes(x=x, y=y, label = lab),
    #width = unit(5, "npc"),
    size=15,family="roboto",hjust=1,vjust=1,lineheight=0.40,
    fill = NA, box.color = NA,halign=1,
    width = grid::unit(0.66, "npc"),
    box.padding = grid::unit(rep(0, 4), "pt"),
    box.margin = grid::unit(rep(0, 4), "pt")
  )+
  geom_richtext(
    data=cap,
    aes(x=x, y=y, label = label),
    col="grey20",
    size=13,family="roboto",hjust=1,vjust=1,lineheight=0.40,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  theme_void()+
  theme(
    plot.margin = margin(0,0,0,0),
    title=element_blank()
  )




ba+wh+tit+ma+
  plot_layout(ncol = 2)&
  plot_annotation(theme = theme(plot.background = element_rect(fill=whi,color=NA)))
