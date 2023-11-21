library(rnaturalearth)
library(sf)
library(tidyverse)
library(camcorder)
library(scico)
library(patchwork)
library(showtext)
library(ggtext)
library(ggtern)

# Set fonts
font_add_google("Abril Fatface","abril")
font_add_google("Concert One","concert")

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


france <- ne_states(country = "United States of America", returnclass = "sf") %>% 
  filter(!name %in% c("Alaska", "Hawaii"))%>%
  mutate(ct=1)%>%
  group_by(iso_a2)%>%
  summarize(sm=sum(ct))

ggplot(france)+
  geom_sf()

country_grid<-st_make_grid(
    france,
    square=FALSE,
    n=c(100,100)
  )%>%
  st_as_sf()%>%
  st_intersection(france)%>%
  mutate(id_str=as.character(row_number()))

ggplot(country_grid)+
  geom_sf()

# here extracting vector for texture extraction in SoilGrids
#st_write(country_grid, "us_grid.shp")
#class(country_grid$id_str)

# Back from SoilGrids for the plot

us_soil<-read_csv('data/us_soil.csv')%>%
  mutate(id_str=as.character(id_str))%>%
  mutate(
    clay=clay_5_15cm/10,
    silt=silt_5_15cm/10,
    sand=sand_5_15cm/10,
    x=clay,
    y=silt,
    z=sand
  )%>%
  select(id_str,clay,silt,sand,x,y,z)%>%
  mutate(
    clay2=10+clay*0.8,
    silt2=10+silt*0.8,
    sand2=sand*0.8
  )%>%
  mutate(clay_cl=case_when(
    clay<(100*1/3)~"A",
    clay<(100*2/3)~"B",
    TRUE~"C"
  ))%>%
  mutate(silt_cl=case_when(
    silt<(100*1/3)~"A",
    silt<(100*2/3)~"B",
    TRUE~"C"
  ))%>%
  mutate(sand_cl=case_when(
    sand<(100*1/3)~"A",
    sand<(100*2/3)~"B",
    TRUE~"C"
  ))%>%
  mutate(cl=glue::glue("{clay_cl}{silt_cl}{sand_cl}"))%>%
  mutate(cl2=case_when(
    cl=="CAA"~"p1",
    cl=="BAA"~"p2",
    cl=="BAB"~"p3",
    cl=="AAB"~"p4",
    cl=="AAC"~"p5",
    cl=="BBA"~"p6",
    cl=="ABA"~"p7",
    cl=="ABB"~"p8",
    cl=="ACA"~"p9"
  ))


# Merge data and map
data<-country_grid%>%
  rename(geometry=x)%>%
  left_join(us_soil, by=c('id_str'))

# Make texture map

pal_fill2<-c(
  "p1"="#01a0c6",
  "p2"="#3EBAD4",
  "p3"="#8ACCC5",
  "p4"="#feffab",
  "p5"="#feff00",
  "p6"="#6DACCF",
  "p7"="#f882be",
  "p8"="#ffb4a9",
  "p9"="#f21c8d"
)

ggplot(data,aes(fill=cl2))+
  geom_sf(color=alpha("white",0))+
  scale_fill_manual(values=pal_fill2)+
  guides(fill="none")+
  coord_sf(crs=5070)+
  theme_void()+
  theme(plot.background = element_rect(fill=NA,color=NA))

# Make ternary legends

# Pixel distribution
tern1<-ggtern(data%>%drop_na(clay,silt,sand),aes(x=x,y=y,z=z,color=cl2))+
  geom_point()+
  scale_color_manual(values=pal_fill2)+
  scale_T_continuous(limits=c(0,1))+ 
  scale_L_continuous(limits=c(0,1))+ 
  scale_R_continuous(limits=c(0,1))+
  guides(color="none")+
  theme_void()+
  theme_nolabels()+
  theme_custom(
    tern.plot.background = NA,
    tern.panel.background ="#EAEAEA",
    col.grid.minor = NA,
    col.L=NA,
    col.T=NA,
    col.R=NA
  )

ggsave(
  plot=tern1,
  filename="t1.png",
  width=15,
  height=15,
  units="cm"
  #bg=NULL
  
)

print(tern1)  

# Color legend

pol1 <- tibble(
  x=c(100*2/3,100*2/3,100),
  y=c(100*1/3,0,0),
  z=c(0,1/3*100,0),
  col="p1"
)

pol2 <- tibble(
  x=c(100*2/3,100*1/3,2/3*100),
  y=c(100*1/3,100*1/3,0),
  z=c(0,100*1/3,1/3*100),
  col="p2"
)

pol3 <- tibble(
  x=c(100*2/3,100*1/3,1/3*100),
  y=c(0,100*1/3,0),
  z=c(100*1/3,100*1/3,100*2/3),
  col="p3"
)

pol4 <- tibble(
  x=c(100*1/3,100*1/3,0),
  y=c(0,100*1/3,1/3*100),
  z=c(100*2/3,100*1/3,100*2/3),
  col="p4"
)

pol5<-tibble(
  x=c(0,1/3*100,0),
  y=c(0,0,1/3*100),
  z=c(100,2/3*100,2/3*100),
  col="p5"
)

pol6 <- tibble(
  x=c(100*2/3,100*1/3,100*1/3),
  y=c(100*1/3,2/3*100,100*1/3),
  z=c(0,0,100*1/3),
  col="p6"
)

pol7 <- tibble(
  x=c(100*1/3,0,100*1/3),
  y=c(2/3*100,2/3*100,100*1/3),
  z=c(0,100*1/3,100*1/3),
  col="p7"
)

pol8 <- tibble(
  x=c(0,0,100*1/3),
  y=c(2/3*100,1/3*100,100*1/3),
  z=c(1/3*100,100*2/3,100*1/3),
  col="p8"
)


pol9 <- tibble(
  x=c(0,1/3*100,0),
  y=c(2/3*100,2/3*100,100),
  z=c(1/3*100,0,0),
  col="p9"
)



multipol<-pol1%>%
  bind_rows(pol2)%>%
  bind_rows(pol3)%>%
  bind_rows(pol4)%>%
  bind_rows(pol5)%>%
  bind_rows(pol6)%>%
  bind_rows(pol7)%>%
  bind_rows(pol8)%>%
  bind_rows(pol9)%>%
  mutate(
    x=10+x*0.8,
    y=10+y*0.8,
    z=z*0.8
  )


t2<-ggtern(multipol,aes(x=x,y=y,z=z,fill=col))+
  geom_polygon(color=NA)+
  #geom_point(mp,mapping=aes(x=clay,y=silt,z=sand,color=cl))+
  scale_fill_manual(values=pal_fill2)+
  scale_T_continuous(limits=c(.1,.9))+ 
  scale_L_continuous(limits=c(.1,.9))+ 
  scale_R_continuous(limits=c(0,.8))+
  guides(fill="none")+
theme_void()+
  theme_nolabels()+
  theme_custom(
    tern.plot.background = NA,
    tern.panel.background = NA,
    col.grid.minor = NA,
    col.L=NA,
    col.T=NA,
    col.R=NA
  )

t2

# Individual soil maps

pal_clay <- c(
  "#EBFBFF",
  "#AEEEFF",
  "#5DDEFE",
  "#01C3F4",
  "#01A0C6"
  
)

pal_silt <- c(
  "#FEECF5",
  "#FBC5E2",
  "#F99FCF",
  "#F665B2",
  "#F21C8D"
  
)

pal_sand <- c(
  "#FFFF99",
  "#FEFF00",
  "#F5F500",
  "#E0E000",
  "#CCCC00"
  
)

th_cus <- theme(
  plot.margin = margin(0.25,0.25,0.25,0.25,"cm"),
  plot.title = element_blank(),
  legend.title = element_markdown(
    size=56,family="open",vjust=0,margin=margin(0,0,-0.5,0,"cm")),
  legend.text = element_markdown(
    size=46,hjust=0.5,family="open",margin=margin(-0.5,0,0,0,"cm")),
  legend.position = "bottom"
)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 15, 
  units = "cm", 
  dpi = 300 
)

clay<-ggplot()+
  geom_sf(
    data,
    mapping=aes(geometry=geometry,fill=clay),
    color=NA)+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal_clay,
    breaks = c(14,24,34,44,54),
    na.value = NA,
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "horizontal",
      barheight = unit(5, units = "mm") , 
      barwidth = unit(100, units = "mm"),
      title.position = 'top',
      label.position = "bottom",
      title.hjust = 0.5
    ))+
  labs(
    title="**Clay** content",
    fill="**Clay content** (%)"
  )+
  coord_sf(crs=5070)+
  theme_void()+
  th_cus
clay

silt<-ggplot()+
  geom_sf(
    data,
    mapping=aes(geometry=geometry,fill=silt),
    color=NA)+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal_silt,
    breaks = c(20,30,40,50,90),
    na.value = NA,
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "horizontal",
      barheight = unit(5, units = "mm") , 
      barwidth = unit(100, units = "mm"),
      title.position = 'top',
      label.position = "bottom",
      title.hjust = 0.5
    ))+
  labs(
    title="**Silt** content",
    fill="**Silt content** (%)"
  )+
  theme_void()+
  th_cus

silt

sand<-ggplot()+
  geom_sf(
    data,
    mapping=aes(geometry=geometry,fill=sand),
    color=NA)+
  binned_scale(
    aesthetics = "fill",
    scale_name = "stepsn", 
    palette = function(x) pal_sand,
    breaks = c(20,30,40,50,90),
    na.value = NA,
    show.limits = FALSE, 
    guide = guide_colorsteps(
      nrow=1,
      direction = "horizontal",
      barheight = unit(5, units = "mm") , 
      barwidth = unit(100, units = "mm"),
      title.position = 'top',
      label.position = "bottom",
      title.hjust = 0.5
    ))+
  labs(
    title="**Sand** content",
    fill="**Sand content** (%)"
  )+
  theme_void()+
  th_cus

sand

