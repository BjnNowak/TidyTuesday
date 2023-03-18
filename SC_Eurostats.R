library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(geomtextpath)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Jost","jost")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 40, 
  height = 40, 
  units = "cm", 
  dpi = 300 
)

# Data:
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Agri-environmental_indicator_-_cropping_patterns#Analysis_of_cropping_patterns_at_country_level
data <- read_delim('Data/Prec/data/assolement_eurostat.csv',delim=';')



dt20 <- data%>%
  filter(year==2020)%>%
  filter(area!="EU")%>%
  mutate(total=arable+permanent_grassland+permanent_crops+kitchen_gardens)%>%
  mutate(
    per_arable = arable/total,
    per_zgrass = permanent_grassland/total,
    per_orch = permanent_crops/total,
    per_kitch = kitchen_gardens/total
  )%>%
  select(area,per_arable,per_zgrass,per_orch,per_kitch)%>%
  pivot_longer(!area, names_to = "crop", values_to = "ratio")

perm<-dt20%>%
  filter(crop=="per_orch")%>%
  arrange(-ratio)%>%
  head(5)%>%
  pull(area)

dt10 <- data%>%
  filter(year==2010)%>%
  filter(area!="EU")%>%
  mutate(total=arable+permanent_grassland+permanent_crops+kitchen_gardens)%>%
  mutate(
    per_arable = arable/total,
    per_zgrass = permanent_grassland/total,
    per_orch = permanent_crops/total,
    per_kitch = kitchen_gardens/total
  )%>%
  select(area,per_arable,per_zgrass,per_orch,per_kitch)%>%
  pivot_longer(!area, names_to = "crop", values_to = "ratio")

eu <- data%>%
  filter(year==2020)%>%
  filter(area=="EU")%>%
  mutate(total=arable+permanent_grassland+permanent_crops+kitchen_gardens)%>%
  mutate(
    per_arable = arable/total,
    per_zgrass = permanent_grassland/total,
    per_orch = permanent_crops/total,
    per_kitch = kitchen_gardens/total
  )%>%
  select(area,per_arable,per_zgrass,per_orch,per_kitch)%>%
  pivot_longer(!area, names_to = "crop", values_to = "ratio")

eu_10 <- data%>%
  filter(year==2010)%>%
  filter(area=="EU")%>%
  mutate(total=arable+permanent_grassland+permanent_crops+kitchen_gardens)%>%
  mutate(
    per_arable = arable/total,
    per_zgrass = permanent_grassland/total,
    per_orch = permanent_crops/total,
    per_kitch = kitchen_gardens/total
  )%>%
  select(area,per_arable,per_zgrass,per_orch,per_kitch)%>%
  pivot_longer(!area, names_to = "crop", values_to = "ratio")

crop<-dt20%>%
  filter(crop=="per_arable")%>%
  arrange(ratio)%>%
  mutate(id=row_number())%>%
  select(area,id)

dt20_id <- dt20%>%
  left_join(crop)%>%
  group_by(area)%>%
  arrange(crop)%>%
  mutate(
    xmax=cumsum(ratio),
    xmin=xmax-ratio
  )%>%
  ungroup()

dt10_id <- dt10%>%
  left_join(crop)%>%
  group_by(area)%>%
  arrange(crop)%>%
  mutate(
    xmax=cumsum(ratio),
    xmin=xmax-ratio
  )%>%
  ungroup()

dt20_lab <- dt20_id %>%
  group_by(area)%>%
  summarize(id=mean(id))

dt10_lab <- dt10_id %>%
  group_by(area)%>%
  summarize(id=mean(id))

eu_id <- eu%>%
  #left_join(crop)%>%
  group_by(area)%>%
  arrange(crop)%>%
  mutate(
    xmax=cumsum(ratio),
    xmin=xmax-ratio
  )%>%
  ungroup()%>%
  mutate(label="European Union")

eu_10_id <- eu_10%>%
  #left_join(crop)%>%
  group_by(area)%>%
  arrange(crop)%>%
  mutate(
    xmax=cumsum(ratio),
    xmin=xmax-ratio
  )%>%
  ungroup()%>%
  mutate(label="European Union")

eu_lab<- eu_id%>%
  filter(crop=="per_arable")%>%
  mutate(label="European Union")

pal<-c(
  "per_arable"="#FFD166",
  "per_kitch"="#0075F2",
  "per_orch"="#EF476F",
  "per_zgrass"="#06D6A0"
)

ggplot()+
  geom_rect(
    dt20_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=id-0.25,ymax=id+0.25,fill=crop),
    alpha=0.6
  )+
  geom_rect(
    eu_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=29-0.75,ymax=29+0.75,fill=crop)
  )+
  geom_textpath(
    dt20_lab,
    mapping=aes(y=id,x=0.5,label=area),
    upright=TRUE,
    size=7,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  geom_textpath(
    dt20_lab,
    mapping=aes(y=id,x=0.15,label=area),
    upright=TRUE,
    size=7,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  geom_textpath(
    dt20_lab,
    mapping=aes(y=id,x=0.85,label=area),
    upright=TRUE,
    size=7,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  
  geom_textpath(
    eu_id,
    mapping=aes(y=29,x=0.5,label=label),
    upright=TRUE,fontface="bold",
    size=9,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  geom_textpath(
    eu_id,
    mapping=aes(y=29,x=0.15,label=label),
    upright=TRUE,fontface="bold",
    size=9,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  geom_textpath(
    eu_id,
    mapping=aes(y=29,x=0.85,label=label),
    upright=TRUE,fontface="bold",
    size=9,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  
  coord_polar(start=pi+pi/2)+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(-20,30))+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()


irl<-ggplot()+
  geom_rect(
    dt20_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=id-0.25,ymax=id+0.25,fill=crop),
    alpha=0.15
  )+
  geom_rect(
    dt20_id%>%filter(area=="Ireland"),
    mapping=aes(xmin=xmin,xmax=xmax,ymin=id-0.25,ymax=id+0.25,fill=crop),
    alpha=1
  )+
  geom_rect(
    eu_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=29-0.75,ymax=29+0.75,fill=crop),
    alpha=0.15
  )+
  
  
  coord_polar(start=pi+pi/2)+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(-20,30))+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()



orch<-ggplot()+
  geom_rect(
    dt20_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=id-0.25,ymax=id+0.25,fill=crop),
    alpha=0.15
  )+
  geom_rect(
    dt20_id%>%filter(area%in%perm),
    mapping=aes(xmin=xmin,xmax=xmax,ymin=id-0.25,ymax=id+0.25,fill=crop),
    alpha=1
  )+
  geom_rect(
    eu_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=29-0.75,ymax=29+0.75,fill=crop),
    alpha=0.15
  )+
  
  
  coord_polar(start=pi+pi/2)+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(-20,30))+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()

orch

malt<-ggplot()+
  geom_rect(
    dt20_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=id-0.25,ymax=id+0.25,fill=crop),
    alpha=0.15
  )+
  geom_rect(
    dt20_id%>%filter(area=="Malta"),
    mapping=aes(xmin=xmin,xmax=xmax,ymin=id-0.25,ymax=id+0.25,fill=crop),
    alpha=1
  )+
  geom_rect(
    eu_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=29-0.75,ymax=29+0.75,fill=crop),
    alpha=0.15
  )+
  
  
  coord_polar(start=pi+pi/2)+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(-20,30))+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()

cr<-ggplot()+
  geom_rect(
    dt20_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=id-0.25,ymax=id+0.25,fill=crop),
    alpha=0.15
  )+
  geom_rect(
    dt20_id%>%filter(area%in%c("Finland","Denmark")),
    mapping=aes(xmin=xmin,xmax=xmax,ymin=id-0.25,ymax=id+0.25,fill=crop),
    alpha=1
  )+
  geom_rect(
    eu_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=29-0.75,ymax=29+0.75,fill=crop),
    alpha=0.15
  )+
  
  
  coord_polar(start=pi+pi/2)+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(-20,30))+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()

cr

ggplot()+
  geom_rect(
    dt10_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=id-0.25,ymax=id+0.25,fill=crop),
    alpha=0.6
  )+
  geom_rect(
    eu_10_id,
    mapping=aes(xmin=xmin,xmax=xmax,ymin=29-0.75,ymax=29+0.75,fill=crop)
  )+
  geom_textpath(
    dt10_lab,
    mapping=aes(y=id,x=0.5,label=area),
    upright=TRUE,
    size=7,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  geom_textpath(
    dt10_lab,
    mapping=aes(y=id,x=0.15,label=area),
    upright=TRUE,
    size=7,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  geom_textpath(
    dt10_lab,
    mapping=aes(y=id,x=0.85,label=area),
    upright=TRUE,
    size=7,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  
  geom_textpath(
    eu_id,
    mapping=aes(y=29,x=0.5,label=label),
    upright=TRUE,fontface="bold",
    size=9,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  geom_textpath(
    eu_id,
    mapping=aes(y=29,x=0.15,label=label),
    upright=TRUE,fontface="bold",
    size=9,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  geom_textpath(
    eu_id,
    mapping=aes(y=29,x=0.85,label=label),
    upright=TRUE,fontface="bold",
    size=9,family="jost",hjust=0.5,vjust=0.5,spacing=-200)+
  
  coord_polar(start=pi+pi/2,direction=-1)+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(-20,30))+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()
