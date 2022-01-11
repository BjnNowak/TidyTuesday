library(tidyverse)
library(sf)
library(maps)
library(cowplot)
library(camcorder)
library(showtext)
library(grid)

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 12, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

# Add fonts
font_add_google("Roboto Condensed","con")
font_add_google("Ranchers","ran")
# Automatically use {showtext} for plots
showtext_auto()

violet_pal<- c(
  '#3C2A3C',
  '#604360',
  '#845C84',
  '#A37BA3',
  '#BC9FBC',
  '#D5C3D5'
)

na_pal<-'lightgrey'

# Load data
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

stress <- stressor%>%
  group_by(months,stressor,state)%>%
  summarize(whole_stress=mean(na.omit(stress_pct)))%>%
  ungroup()

max<-max(na.omit(stress$whole_stress))
max

# Load and clean world map
us <- st_as_sf(maps::map(database="state", plot = FALSE, fill = TRUE))%>%
  mutate(state=str_to_title(us$ID))

sub<-stress%>%
  filter(months=="April-June",strs=)%>%
  filter(stressor=="Other pests/parasites")


fun_plot <- function(mths,strs){
  
  sub<-stress%>%
    filter(months==mths)%>%
    filter(stressor==strs)
  
  us<-us%>%
    left_join(sub)

  gg<-ggplot(us)+
    geom_sf(aes(fill=whole_stress),size=0.2,color="white")+
    scale_fill_gradientn(
      colors=rev(violet_pal),na.value = na_pal,
      limits=c(0,max)
    )+
    guides(
      fill='none'
    )+
    theme_void()

  return(gg)
}

fun_plot_legend <- function(mths,strs){
  
  sub<-stress%>%
    filter(months==mths)%>%
    filter(stressor==strs)
  
  us<-us%>%
    left_join(sub)
  
  gg<-ggplot(us)+
    geom_sf(aes(fill=whole_stress),size=0.2,color="white")+
    scale_fill_gradientn(
      colors=rev(violet_pal),na.value = na_pal,
      limits=c(0,max),
      breaks=c(10,30,50),
      labels=c("10%","30%","50%")
    )+
    guides(fill = guide_colorbar(
      barheight = unit(4, units = "mm"),
      barwidth = unit(20, units = "mm"),
      direction = "horizontal",
      ticks.colour = "white",
      ticks.linewidth = 2,
      title.position = "top",
      label.position = "bottom",
      title.hjust = 0.5))+
    labs(fill="Colony exposed to stress")+
    theme_void()+
    theme(
      #legend.title = element_text(size=20,family="con"),
      #legend.text= element_text(size=15,family="con", margin(0, 0, 0, 0, "cm")),
      legend.title = element_blank(),
      legend.text= element_blank()
    )
  
  return(gg)
}

# Extract legend
legend <- cowplot::get_legend(fun_plot_legend(mths="January-March",strs="Varroa mites"))

# Varoa
V1<-fun_plot(mths="January-March",strs="Varroa mites")
V2<-fun_plot(mths="April-June",strs="Varroa mites")
V3<-fun_plot(mths="July-September",strs="Varroa mites")
V4<-fun_plot(mths="October-December",strs="Varroa mites")

# Other pests/parasites
O1<-fun_plot(mths="January-March",strs="Other pests/parasites")
O2<-fun_plot(mths="April-June",strs="Other pests/parasites")
O3<-fun_plot(mths="July-September",strs="Other pests/parasites")
O4<-fun_plot(mths="October-December",strs="Other pests/parasites")

# Diseases
D1<-fun_plot(mths="January-March",strs="Disesases")
D2<-fun_plot(mths="April-June",strs="Disesases")
D3<-fun_plot(mths="July-September",strs="Disesases")
D4<-fun_plot(mths="October-December",strs="Disesases")

# Pesticides
P1<-fun_plot(mths="January-March",strs="Pesticides")
P2<-fun_plot(mths="April-June",strs="Pesticides")
P3<-fun_plot(mths="July-September",strs="Pesticides")
P4<-fun_plot(mths="October-December",strs="Pesticides")


# Make plot

x1<-0.18
x2<-0.38
x3<-0.58
x4<-0.78

y1<-0.5
y2<-0.35
y3<-0.20
y4<-0.05

h<-0.2
w<-0.2


rect <- rectGrob(
  x = 0,
  y = 0,
  width = 4,
  height = 4,
  #hjust = 0, vjust = 1,
  gp = gpar(fill = "white", alpha = 1)
)

ggdraw()+
  draw_grob(rect)+
  # Varoa
  draw_plot(V1,x=x1,y=y1,height=h,width=w)+
  draw_plot(V2,x=x1,y=y2,height=h,width=w)+
  draw_plot(V3,x=x1,y=y3,height=h,width=w)+
  draw_plot(V4,x=x1,y=y4,height=h,width=w)+
  # Other
  draw_plot(O1,x=x2,y=y1,height=h,width=w)+
  draw_plot(O2,x=x2,y=y2,height=h,width=w)+
  draw_plot(O3,x=x2,y=y3,height=h,width=w)+
  draw_plot(O4,x=x2,y=y4,height=h,width=w)+
  # Diseases
  draw_plot(D1,x=x3,y=y1,height=h,width=w)+
  draw_plot(D2,x=x3,y=y2,height=h,width=w)+
  draw_plot(D3,x=x3,y=y3,height=h,width=w)+
  draw_plot(D4,x=x3,y=y4,height=h,width=w)+
  # Pesticides
  draw_plot(P1,x=x4,y=y1,height=h,width=w)+
  draw_plot(P2,x=x4,y=y2,height=h,width=w)+
  draw_plot(P3,x=x4,y=y3,height=h,width=w)+
  draw_plot(P4,x=x4,y=y4,height=h,width=w)+
  # Legend
  draw_plot(legend,x=0.6,y=0.7,height=0.25,width=0.4)+
  draw_label(
    "% of colonies\nexposed to stress",x=0.72,y=0.86,size=24,vjust=0,
    fontfamily='con', lineheight = 0.3,fontface = 'bold',
    hjust=0
  )+
  draw_label(
    "10%",x=0.73,y=0.78,size=22,vjust=0,
    fontfamily='con', lineheight = 0.3,
    hjust=0
  )+
  draw_label(
    "30%",x=0.785,y=0.78,size=22,vjust=0,
    fontfamily='con', lineheight = 0.3,
    hjust=0
  )+
  draw_label(
    "50%",x=0.84,y=0.78,size=22,vjust=0,
    fontfamily='con', lineheight = 0.3,
    hjust=0
  )+
  # Title
  draw_label(
    "The causes of the decline\nof bees in the United States",x=0.02,y=0.80,size=60,vjust=0,
    fontfamily='ran', lineheight = 0.3,fontface = 'bold',color="#f77f00",
    hjust=0
  )+
  
  # Labels
  # Time period
  draw_label(
    "From January\nto March",x=x1-0.1,y=y1+0.12,size=22,vjust=0.5,
    fontfamily='con', lineheight = 0.3,
    hjust=0.5
  )+
  draw_label(
    "From April\nto June",x=x1-0.1,y=y2+0.12,size=22,vjust=0.5,
    fontfamily='con', lineheight = 0.3,
    hjust=0.5
  )+
  draw_label(
    "From July\nto September",x=x1-0.1,y=y3+0.12,size=22,vjust=0.5,
    fontfamily='con', lineheight = 0.3,
    hjust=0.5
  )+
  draw_label(
    "From October\nto December",x=x1-0.1,y=y4+0.12,size=22,vjust=0.5,
    fontfamily='con', lineheight = 0.3,
    hjust=0.5
  )+
  # Time period
  draw_label(
    "From January\nto March",x=x1-0.1,y=y1+0.12,size=22,vjust=0.5,
    fontfamily='con', lineheight = 0.3,
    hjust=0.5
  )+
  draw_label(
    "From April\nto June",x=x1-0.1,y=y2+0.12,size=22,vjust=0.5,
    fontfamily='con', lineheight = 0.3,
    hjust=0.5
  )+
  draw_label(
    "From July\nto September",x=x1-0.1,y=y3+0.12,size=22,vjust=0.5,
    fontfamily='con', lineheight = 0.3,
    hjust=0.5
  )+
  # Causes
  draw_label(
    "Varoa mites",x=x1+0.08,y=y1+0.22,size=22,vjust=0.5,hjust=0.5,
    fontfamily='con', lineheight = 0.3
  )+
  draw_label(
    "Other pests/parasites",x=x2+0.08,y=y1+0.22,size=22,vjust=0.5,hjust=0.5,
    fontfamily='con', lineheight = 0.3
  )+
  draw_label(
    "Diseases",x=x3+0.08,y=y1+0.22,size=22,vjust=0.5,hjust=0.5,
    fontfamily='con', lineheight = 0.3
  )+
  draw_label(
    "Pesticides",x=x4+0.08,y=y1+0.22,size=22,vjust=0.5,hjust=0.5,
    fontfamily='con', lineheight = 0.3
  )+
  draw_label(
    "Data: USDA (from 2015 to 2021) | Plot: @BjnNowak",x=0.95,y=0.05,size=18,vjust=0.5,hjust=1,
    fontfamily='con', lineheight = 0.3
  )
