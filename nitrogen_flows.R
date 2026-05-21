library(tidyverse)
library(rnaturalearth)
library(mapview)
library(lwgeom)
library(sf)
library(sfheaders)
library(camcorder)
library(patchwork)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21.6, 
  height = 27,
  units = "cm", 
  dpi = 300 
)

# Fixer le crs
cust_crs<-"ESRI:54030"

# Chargement des données
########################

# Matrice des échanges
data<-read_delim('data/trade_engrais_N.csv')

clean<-data%>%
  select(
    reporter_country_code=`Reporter Country Code (M49)`,
    reporter_country=`Reporter Countries`,
    partner_country_code=`Partner Country Code (M49)`,
    partner_country=`Partner Countries`,
    year=Year,
    value=Value
  )

# Table des productions
data_prod<-read_delim('data/nitrogen_production_2023.csv')%>%
  filter(Area!="China")

world <- ne_countries(scale = 110, type = "countries", returnclass = "sf")%>%
  sf::st_transform(crs="ESRI:54030")%>%
  mutate(un_a3=case_when(
    name=="Norway"~'578',
    name=='Malta'~'470',
    name=='Singapore'~'702',
    TRUE~un_a3
  ))%>%
  mutate(name=case_when(
    name=='Vietnam'~'Viet Nam',
    name=='Tanzania'~'United Republic of Tanzania',
    TRUE~name
  ))%>%
  filter(admin!="Antarctica")

# Préparation des données
#########################

# Extraction des centroïdes
cent <- world%>%
  st_centroid()
# Coordonnées des centroides
cent_with_coord<-cent%>%
  bind_cols(st_coordinates(cent))%>%
  st_drop_geometry()

# Extraction des 10 principaux producteurs
resume<-data_prod%>%
  arrange(-Value)%>%
  head(10)%>%
  left_join(cent,by=c('Area Code (M49)'='un_a3'))

sel_vec<-resume$`Area Code (M49)`

# Application à la table des flux
clean_sel<-clean%>%
  filter(reporter_country_code%in%sel_vec)

# Fonction pour créer les vecteurs d'exportations
fun_lines <- function(nm,data){
  
  test<-data%>%
    filter(reporter_country_code==nm)%>%
    left_join(cent_with_coord,by=c("partner_country_code"="un_a3"))
  
  cent_brazil<-cent_with_coord%>%
    filter(un_a3==nm)%>%
    select(X,Y)
  
  sf_braz <- cent_brazil%>%
    bind_rows(test[1,]%>%select(X,Y))%>%
    #st_coordinates()%>%
    sf_linestring()%>%
    st_set_crs(st_crs(cent))%>%
    bind_cols(value=test$value[1])
  
  sf_braz$id <- test$partner_country[1]
  
  for (i in 2:dim(test)[1]){
    
    temp<-cent_brazil%>%
      bind_rows(test[i,]%>%select(X,Y))%>%
      #st_coordinates()%>%
      sf_linestring()%>%
      st_set_crs(st_crs(cent))%>%
      bind_cols(value=test$value[i])
    
    temp$id <- test$partner_country[i]
    
    sf_braz <- sf_braz%>%
      bind_rows(temp)
  }
  
  sf_braz<-sf_braz%>%
    mutate(vld=st_is_valid(sf_braz))%>%
    filter(vld==TRUE)
  
  return(sf_braz)
  
}

# Application de la fonction aux dix principaux producteurs
flows_china<-fun_lines(nm='156',data=clean_sel)
flows_india<-fun_lines(nm='356',data=clean_sel)
flows_usa<-fun_lines(nm='840',data=clean_sel)
flows_russia<-fun_lines(nm='643',data=clean_sel)
flows_indonesia<-fun_lines(nm='360',data=clean_sel)
flows_egypt<-fun_lines(nm='818',data=clean_sel)
flows_arabia<-fun_lines(nm='682',data=clean_sel)
flows_pakistan<-fun_lines(nm='586',data=clean_sel)
flows_qatar<-fun_lines(nm='634',data=clean_sel)
flows_iran<-fun_lines(nm='364',data=clean_sel)

# Préparation des cartes
#########################

# Create graticule
grat <-  sf::st_graticule(lat = c(-89.9, seq(-90, 60, 30), 89.9))%>%
  sf::st_transform(crs=cust_crs)

col_grat='#293132'
col_n='#432371'
col_borders="#293132"
col_countries="#faae7b"

fun_plot<-function(nm, flw){

  plot<-ggplot()+
    geom_sf(
      world,
      mapping=aes(geometry=geometry),
      fill=col_countries,color=alpha(col_borders,0.15),linewidth=0.01
    )+
    geom_sf(
      flw,
      mapping=aes(linewidth=value,alpha=value,geometry=geometry),
      color=col_n
    )+
    geom_sf(
      resume%>%filter(Area==nm),
      mapping=aes(size=Value,geometry=geometry),
      pch=21, 
      fill=col_n
    )+
    geom_sf(
      grat,mapping=aes(geometry=geometry),
      alpha=1,color=col_grat,lwd=0.15,
      inherit.aes='F'
    )+
    scale_alpha(
      range=c(0.1,0.75),
      limits=c(min(clean_sel$value),max(clean_sel$value))
    )+
    scale_linewidth(
      range=c(1,10),
      limits=c(min(clean_sel$value),max(clean_sel$value))
    )+
    scale_size(
      range=c(5,15),
      limits=c(min(resume$Value),max(resume$Value))
    )+
    guides(alpha='none',size='none',linewidth='none',fill='none')+
    theme_void()
  
    return(plot)
  
}

fun_plot(nm="China, mainland",flw=flows_china)+
  fun_plot(nm="India",flw=flows_india)+
  fun_plot(nm="United States of America",flw=flows_usa)+
  fun_plot(nm="Russian Federation",flw=flows_russia)+
  fun_plot(nm="Indonesia",flw=flows_indonesia)+
  fun_plot(nm="Egypt",flw=flows_egypt)+
  fun_plot(nm="Saudi Arabia",flw=flows_arabia)+
  fun_plot(nm="Pakistan",flw=flows_pakistan)+
  fun_plot(nm="Qatar",flw=flows_qatar)+
  fun_plot(nm="Iran (Islamic Republic of)",flw=flows_iran)+
  plot_layout(ncol = 2)

sum(resume$Value)/sum(data_prod$Value)
sum(resume$Value[1:4])/sum(data_prod$Value)
