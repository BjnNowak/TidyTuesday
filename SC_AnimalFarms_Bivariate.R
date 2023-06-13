
library(frex) # https://github.com/BjnNowak/frex
library(tidyverse)
library(sf)
library(camcorder)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

data<-get_map()%>%
  st_drop_geometry()%>%
  left_join(get_static_layer("herds_municipality"))%>%
  left_join(get_static_layer("human"))

clean<-data%>%
  mutate(cow_km2=milk_cow_km2+meat_cow_km2)%>%
  mutate(cow_cl=case_when(
    cow_km2<25~"A2",
    cow_km2<50~"B2",
    TRUE~"C2"
  ))%>%
  mutate(pig_cl=case_when(
    pig_km2<25~"A2",
    pig_km2<50~"B2",
    TRUE~"C2"
  ))%>%
  mutate(poultry_cl=case_when(
    poultry_km2<25~"A2",
    poultry_km2<50~"B2",
    TRUE~"C2"
  ))%>%
  mutate(goat_cl=case_when(
    goat_km2<25~"A2",
    goat_km2<50~"B2",
    TRUE~"C2"
  ))%>%
  mutate(sheep_cl=case_when(
    sheep_km2<25~"A2",
    sheep_km2<50~"B2",
    TRUE~"C2"
  ))%>%
  mutate(human_cl=case_when(
    human_km2<25~"A1",
    human_km2<50~"B1",
    TRUE~"C1"
  ))%>%
  mutate(
    cow=glue::glue("{cow_cl}_{human_cl}"),
    pig=glue::glue("{pig_cl}_{human_cl}"),
    poultry=glue::glue("{poultry_cl}_{human_cl}"),
    goat=glue::glue("{goat_cl}_{human_cl}"),
    sheep=glue::glue("{sheep_cl}_{human_cl}"),
  )

pal <- c(
  "A2_A1" = "#e8e8e8",
  "A2_B1" = "#e89abf",
  "A2_C1" = "#e8006d",
  "B2_A1" = "#8bd7e8",
  "B2_B1" = "#8b9abf",
  "B2_C1" = "#8b006d",
  "C2_A1" = "#01bee8",
  "C2_B1" = "#019abf",
  "C2_C1" = "#01006d"
)

pal <- c(
  "A2_A1" = "#efff94",
  "A2_B1" = "#f2a67d",
  "A2_C1" = "#f70053",
  "B2_A1" = "#9fdd65",
  "B2_B1" = "#a19056",
  "B2_C1" = "#a40039",
  "C2_A1" = "#4ab934",
  "C2_B1" = "#4b782c",
  "C2_C1" = "#4c001d"
)

# Bivariate color palette
pal<-c(
  "A2_A1"="#f3f3f3",
  "A2_B1"="#ebc5de",
  "A2_C1"="#e6a2d1",
  "B2_A1"="#c2efce", 
  "B2_B1"="#9ec5d4",
  "B2_C1"="#bb9fcf",
  "C2_A1"="#8ae1ae",
  "C2_B1"="#7cc4b2", 
  "C2_C1"="#7890aa"
)

cw<-ggplot(get_map()%>%left_join(clean),aes(fill=cow))+
  geom_sf(color=alpha("grey95",0.25))+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()

pg<-ggplot(get_map()%>%left_join(clean),aes(fill=pig))+
  geom_sf(color=alpha("grey95",0.25))+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()

py<-ggplot(get_map()%>%left_join(clean),aes(fill=poultry))+
  geom_sf(color=alpha("grey95",0.25))+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()

go<-ggplot(get_map()%>%left_join(clean),aes(fill=goat))+
  geom_sf(color=alpha("grey95",0.25))+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()

sh<-ggplot(get_map()%>%left_join(clean),aes(fill=sheep))+
  geom_sf(color=alpha("grey95",0.25))+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()

cw
pg
py
go
sh

# Make legend
#############
tib<-tibble(
  x1 = rep(c("A","B","C"),3),
  y2 = c(rep("A",3),rep("B",3),rep("C",3)),
  value = glue::glue("{y2}2_{x1}1")
)


leg<-ggplot(data=tib,aes(x=y2,y=x1,fill=value))+
  geom_point(pch=21,size=40,color="grey90")+
  #geom_tile()+
  #geom_text(aes(label=value),size=10)+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()

leg

