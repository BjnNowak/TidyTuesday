library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)
library(rnaturalearth)
library(sf)
library(cartogram)

# Set fonts
font_add_google("Catamaran","cata")
font_add_google("Cabin","cab")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Roboto Slab","rob")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 32, 
  height = 26, 
  units = "cm", 
  dpi = 300 
)

# Load data
###########
# Dataset
cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv')
# Map
# Load basemap
world <- ne_countries(
  type = "countries",
  # Choose scale
  scale = 110,
  # Return {sf} object
  returnclass = "sf"      
)%>%
  st_transform(crs='+proj=robin')

clean<-cheeses%>%
  separate_rows(milk)%>%
  separate_rows(country, sep=", ")%>%
  filter(milk%in%c('cow','goat','sheep'))%>%
  mutate(ct=1)%>%
  group_by(country,milk)%>%
  summarize(sm=sum(ct))%>%
  ungroup()%>%
  drop_na(country)%>%
  pivot_wider(names_from=milk,values_from=sm)%>%
  replace_na(list(cow=0,goat=0,sheep=0))%>%
  mutate(
    total=cow+goat+sheep,
    per_v=cow/total*100,
    per_b=sheep/total*100,
    per_c=goat/total*100
  )%>%
  mutate(v_cl=case_when(
    per_v<(100*1/3)~"A",
    per_v<(100*2/3)~"B",
    TRUE~"C"
  ))%>%
  mutate(c_cl=case_when(
    per_c<(100*1/3)~"A",
    per_c<(100*2/3)~"B",
    TRUE~"C"
  ))%>%
  mutate(b_cl=case_when(
    per_b<(100*1/3)~"A",
    per_b<(100*2/3)~"B",
    TRUE~"C"
  ))%>%
  mutate(cl=glue::glue("{v_cl}{c_cl}{b_cl}"))%>%
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

w <- world%>%
  #st_drop_geometry()%>%
  mutate(geounit=case_when(
    geounit=="Czechia"~"Czech Republic",
    geounit=="Republic of Serbia"~"Serbia",
    TRUE~geounit
  ))%>%
  select(geounit,continent)

for_map<-clean%>%
  mutate(country=case_when(
    country=="Holland"~"Netherlands",
    #country=="Great Britain"~"United Kingdom",
    country=="United States"~"United States of America",
    TRUE~country
  ))%>%
  left_join(w,by=c('country'="geounit"))%>%
  drop_na()%>%
  st_as_sf()
  

dorl<-cartogram::cartogram_dorling(
  for_map,              # input map
  weight='total',  # variable of intereset
  k = 4,                    # Share of the bounding box of x filled by the larger circle
  m_weight = 0.05,          # Movement from centroid (from 0 to 1, with 0=no movement)
  itermax = 1000
)

extr<-dorl%>%
  st_centroid()%>%
  st_coordinates()

for_labs<-dorl%>%
  st_drop_geometry()%>%
  bind_cols(extr)%>%
  mutate(country=case_when(
    country=="United States of America"~"USA",
    country=="United Kingdom"~"UK",
    TRUE~country
  ))%>%
  mutate(lab=glue::glue("**{country}**<br><span style='color:gray20;'>{total}"))

# Make plot
# Create graticule
grat <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9))%>%
  st_transform(crs='+proj=robin')

# Trivariate color palette
pal<-c(
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

col_count<-"#474954"

ggplot()+
  geom_sf(
    world,
    mapping=aes(geometry=geometry),
    color=alpha(col_count,0.65),fill=alpha(col_count,0.5)
  )+
  geom_sf(
    grat,
    mapping=aes(geometry=geometry),
    color=alpha("#D5C9DF",0.3)
  )+
  geom_sf(
    dorl,
    mapping=aes(geometry=geometry,fill=cl2)
  )+
  geom_richtext(
    for_labs%>%arrange(-total)%>%head(10),
    mapping=aes(x=X,y=Y,label=lab,size=total),
    lineheight=0.4,family="cab",
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  scale_fill_manual(values=pal)+
  scale_size(range=c(3,20))+
  guides(fill='none')+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#1D201F",color=NA)
  )



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

ggsave(t2,width = 40, 
       height = 40, 
       units = "cm", bg='transparent',
       dpi = 600 ,file='tst.svg')  




