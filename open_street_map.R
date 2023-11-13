#install.packages('osmdata')
library(osmdata)
library(sf)
library(tidyverse)
library(camcorder)
library(scico)
library(patchwork)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

fun_grid <- function(city){
  
  bb <- getbb(place_name = city)
  k<-(bb[4]-bb[2])/(bb[3]-bb[1])
  
  if (k<1) {
    n_x = 10
    n_y = round(10*k)
  } else {
    n_y = 10
    n_x = round(10*1/k)
  }
  
  oslo_road <- getbb(place_name = city) %>%
    opq(timeout = 100) %>%
    add_osm_feature(
      key = "highway", 
      value = c("motorway", "primary", "secondary","tertiary")
    )%>%
    osmdata_sf()

  oslo_cycle <- getbb(place_name = city) %>%
    opq(timeout = 100) %>%
    add_osm_feature(
      key = "highway", 
      value = c("cycleway")
    )%>%
    osmdata_sf()


  grid_oslo<-st_make_grid(
      oslo_road$osm_lines,
      #square=FALSE,
      n=c(n_x,n_y)
    )%>%
    st_as_sf()%>%
    mutate(id=row_number())

  inter_cycle<-st_intersection(grid_oslo,oslo_cycle$osm_lines)
  inter_road<-st_intersection(grid_oslo,oslo_road$osm_lines)

  clean_cycle<-inter_cycle%>%
    mutate(cycle_length=st_length(inter_cycle))%>%
    group_by(id)%>%
    summarize(lgt_cycle=sum(cycle_length))%>%
    ungroup()%>%
    st_drop_geometry()

  clean_road<-inter_road%>%
    mutate(road_length=st_length(inter_road))%>%
    group_by(id)%>%
    summarize(lgt_road=sum(road_length))%>%
    ungroup()%>%
    st_drop_geometry()

  grid_clean<-grid_oslo%>%
    left_join(clean_cycle)%>%
    left_join(clean_road)%>%
    mutate(ratio=as.numeric(lgt_cycle)/as.numeric(lgt_road))%>%
    drop_na()%>%
    mutate(ratio_cl=case_when(
      ratio<0.2~'A',
      ratio<0.4~'B',
      ratio<0.6~'C',
      ratio<0.8~'D',
      ratio<1~'E',
      TRUE~'F'
    ))
  
  return(grid_clean)
  
}

grid_oslo = fun_grid('Oslo')
grid_amsterdam = fun_grid('Amsterdam')
grid_paris = fun_grid('Paris')
grid_madrid = fun_grid('Madrid')
grid_roma = fun_grid('Roma')
grid_london = fun_grid('London')
grid_berlin = fun_grid('Berlin')
grid_helsinki = fun_grid('Helsinki')
grid_stockholm = fun_grid('Stockholm')
grid_lisbon = fun_grid('Lisbon')
grid_prague = fun_grid('Prague')
grid_warsaw = fun_grid('Warsaw')
grid_athenes = fun_grid('Athenes')
grid_dublin = fun_grid('Dublin')
grid_budapest = fun_grid('Budapest')
grid_bucarest = fun_grid('Bucarest')
grid_copenhagen = fun_grid('Copenhagen')
grid_sofia = fun_grid('Sofia')



# Make plot
col_pal <- scico(6, palette = 'buda')

pal<-c(
  'A'=col_pal[1],
  'B'=col_pal[2],
  'C'=col_pal[3],
  'D'=col_pal[4],
  'E'=col_pal[5],
  'F'=col_pal[6]
)

copenhagen<-ggplot(grid_copenhagen,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
copenhagen

berlin<-ggplot(grid_berlin,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
berlin

helsinki<-ggplot(grid_helsinki,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
helsinki

warsaw<-ggplot(grid_warsaw,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
warsaw

prague<-ggplot(grid_prague,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
prague

stockholm<-ggplot(grid_stockholm,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
stockholm

athenes<-ggplot(grid_athenes,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
athenes

lisbon<-ggplot(grid_lisbon,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
lisbon

dublin<-ggplot(grid_dublin,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
dublin

budapest<-ggplot(grid_budapest,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
budapest

bucarest<-ggplot(grid_bucarest,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
bucarest

oslo<-ggplot(grid_oslo,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
oslo

paris<-ggplot(grid_paris,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
paris

amsterdam<-ggplot(grid_amsterdam,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()

amsterdam

madrid<-ggplot(grid_madrid,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()

madrid

roma<-ggplot(grid_roma,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
roma

london<-ggplot(grid_london,aes(fill=ratio_cl))+
  geom_sf()+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()
london


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 100, 
  height = 100, 
  units = "cm", 
  dpi = 300 
)




ggplot()+
  geom_sf(grid_amsterdam,mapping=aes(fill=ratio_cl,color=ratio_cl,geometry=x))+
  geom_sf(grid_oslo,mapping=aes(fill=ratio_cl,geometry=x),lwd=0.01)+
  geom_sf(grid_paris,mapping=aes(fill=ratio_cl,geometry=x))+
  geom_sf(grid_roma,mapping=aes(fill=ratio_cl,geometry=x))+
  geom_sf(grid_madrid,mapping=aes(fill=ratio_cl,geometry=x))+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=pal)+
  guides(fill='none',color='none')+
  theme_void()


