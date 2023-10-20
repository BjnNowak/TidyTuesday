library(tidyverse)
library(tidygraph)
library(ggraph)
library(camcorder)
library(rnaturalearth)
library(sf)
library(sfheaders)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21, 
  height = 21, 
  units = "cm", 
  dpi = 300 
)

# map
world <- ne_countries(scale = 110, type = "countries", returnclass = "sf")%>%
  mutate(un_a3=case_when(
    name=="Norway"~'578',
    name=='Malta'~'470',
    name=='Singapore'~'702',
    TRUE~un_a3
  ))

cent <- world%>%
  # Convert WGS84 to projected crs (here Robinson)
  sf::st_transform(crs="ESRI:54030")%>%
  #sf::st_transform(crs="+proj=eck6")%>%
  st_centroid()
# Convert WGS84 to projected crs (here Robinson)
#sf::st_transform(crs="EPSG:4326")



# M49 code for Brazil==076

cent_with_coord<-cent%>%
  bind_cols(st_coordinates(cent))%>%
  st_drop_geometry()

st_coordinates(lns)


ggplot(world)+
  geom_sf()+
  geom_sf(
    cent%>%filter(admin=="Brazil"),
    mapping=aes(geometry=geometry)
  )+
  geom_sf(
    cent%>%filter(admin=="Germany"),
    mapping=aes(geometry=geometry)
  )+
  geom_sf(
    lns,mapping=aes(geometry=geometry)
  )
  #coord_sf(crs="ESRI:54030")

data <- read_delim('Data/Trade/coffee_2020.csv',delim=",")%>%
  dplyr::rename(
    # New name = Old name
    domain_code = 'Domain Code',
    reporter_country_code_M49 = 'Reporter Country Code (M49)',
    reporter_country = 'Reporter Countries',
    partner_country_code_M49 = 'Partner Country Code (M49)',
    partner_country = 'Partner Countries',
    element_code = 'Element Code',
    element = 'Element',
    item_code = 'Item Code (CPC)',
    item = 'Item',
    year_code = 'Year Code',
    flag_description = 'Flag Description',
  )%>%
  filter(item=="Coffee, green")%>%
  filter(Value>0)%>%
  drop_na(Value)%>%
  filter(partner_country!="China, Hong Kong SAR")%>%
  filter(partner_country!="China, Taiwan Province of")%>%
  filter(partner_country!="Malta")%>%
  filter(partner_country!="Singapore")


exps<-data%>%
  group_by(reporter_country)%>%
  summarize(sm=sum(Value))%>%
  ungroup()%>%
  arrange(-sm)%>%
  drop_na()%>%
  filter(sm>0)

exps_names <- exps%>%pull(reporter_country)

fun_coun <- function(nm){
  
  test<-data%>%
    filter(reporter_country==nm)%>%
    left_join(cent_with_coord,by=c("partner_country_code_M49"="un_a3"))

  cent_brazil<-cent_with_coord%>%
    filter(admin==nm)%>%
    select(X,Y)

  sf_braz <- cent_brazil%>%
    bind_rows(test[1,]%>%select(X,Y))%>%
    #st_coordinates()%>%
    sf_linestring()%>%
    st_set_crs(st_crs(cent))%>%
    bind_cols(Value=test$Value[1])
  
  sf_braz$id <- test$partner_country[1]

  for (i in 2:dim(test)[1]){
  
    temp<-cent_brazil%>%
      bind_rows(test[i,]%>%select(X,Y))%>%
      #st_coordinates()%>%
      sf_linestring()%>%
      st_set_crs(st_crs(cent))%>%
      bind_cols(Value=test$Value[i])
  
    temp$id <- test$partner_country[i]
  
    sf_braz <- sf_braz%>%
      bind_rows(temp)
  }
  
  #sf_out <- sf_braz%>%
  #  bind_cols(st_coordinates(sf_braz))%>%
  #  drop_na(X)

  return(sf_braz)

}

nrway<-cent%>%filter(name=="Vietnam")
exps$reporter_country[100]

tst<-fun_coun(nm=exps$reporter_country[2])
tst2<-st_zm(tst)


i=1

ggplot(
  st_zm(fun_coun(nm=exps$reporter_country[i])), 
  #sf_braz,
  aes(lwd=Value,alpha=Value))+
  geom_sf(
    world,
    mapping=aes(geometry=geometry),
    color=alpha("dimgrey",0.15),
    inherit.aes=FALSE
  )+
  geom_sf()+
  geom_sf(
    cent%>%filter(admin==exps$reporter_country[i]),
    mapping=aes(geometry=geometry),
    size=6,color="forestgreen",
    inherit.aes=FALSE
  )+
  scale_alpha(range=c(0.25,0.5))+
  #coord_sf(crs="ESRI:54030")+
  theme_light()





