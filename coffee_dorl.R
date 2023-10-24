library(cartogram)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggtext)
library(camcorder)
library(rnaturalearth)
library(sf)
library(sfheaders)
library(showtext)
library(patchwork)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

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
  ))%>%
  mutate(name=case_when(
    name=='China'~'China, mainland',
    name=='Russia'~'Russian Federation',
    name=='Tanzania'~'United Republic of Tanzania',
    name=='United Kingdom'~'United Kingdom of Great Britain and Northern Ireland',
    name=='Vietnam'~'Viet Nam',
    TRUE~name
  ))%>%
  sf::st_transform(crs="ESRI:54030")


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
  #filter(item=="Coffee, green")%>%
  filter(item=="Tea leaves")%>%
  filter(Value>0)%>%
  drop_na(Value)%>%
  filter(partner_country!="China, Hong Kong SAR")%>%
  filter(partner_country!="China, Taiwan Province of")
#filter(partner_country!="Malta")%>%
#filter(partner_country!="Singapore")

exps<-data%>%
  group_by(reporter_country_code_M49)%>%
  summarize(sm=sum(Value))%>%
  ungroup()%>%
  arrange(-sm)%>%
  drop_na()

exps_names <- exps%>%pull(reporter_country_code_M49)

world_data <- world%>%left_join(exps,by=c('un_a3'='reporter_country_code_M49'))%>%
  mutate(sm_clean=case_when(
    sm>0~sm,
    TRUE~0
  ))

dorl<-cartogram::cartogram_dorling(
  world_data, weight='sm_clean', k = 5,
  m_weight = 1, itermax = 1000
)

ggplot(dorl)+
  geom_sf(world,mapping=aes(geometry=geometry),fill="grey96")+
  geom_sf(fill="dimgrey")+
  theme_void()

cent<-dorl%>%
  mutate(
    # Compute area
    ar=as.numeric(st_area(dorl)),
    # Compute radius based on area
    rad=as.numeric(sqrt(ar/pi))
  )%>%
  st_centroid()

cent_with_coord<-cent%>%
  bind_cols(st_coordinates(cent))%>%
  st_drop_geometry()%>%
  arrange(-sm)%>%
  mutate(
    rk=row_number(),
    sm_round = formatC(round(sm/1000),big.mark = ",")
  )%>%
  mutate(name = case_when(
    name=='China, mainland'~'China',
    name=='United Kingdom of Great Britain and Northern Ireland'~'United Kingdom',
    TRUE~name
  ))%>%
  mutate(lab=glue::glue('**{rk}. {name}**<br>{sm_round} kt exported'))

fun_coun <- function(nm){
  
  test<-data%>%
    filter(reporter_country_code_M49==nm)%>%
    left_join(cent_with_coord,by=c("partner_country_code_M49"="un_a3"))
  
  cent_brazil<-cent_with_coord%>%
    filter(un_a3==nm)%>%
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

i=1
tst<-fun_coun(nm=exps$reporter_country_code_M49[i])
ggplot(tst)+
  geom_sf()

i=1



buf<-cent_with_coord%>%
  filter(un_a3==exps$reporter_country_code_M49[i])%>%
  pull(rad)

tst<-fun_coun(nm=exps$reporter_country_code_M49[i])
cent_bra <- cent%>%filter(un_a3==exps$reporter_country_code_M49[i])%>%st_buffer(buf)

tst_ext<-tst%>%
  mutate(vld=st_is_valid(tst))%>%
  filter(vld==TRUE)

inter<-st_intersection(cent_bra,tst_ext)%>%
  select(id,value=Value)%>%
  left_join(world%>%st_drop_geometry(),by=c('id'='name'))

inter<- inter%>%
  mutate(vld=st_is_valid(inter))%>%
  filter(vld==TRUE)

for (i in 2:16){
  buf<-cent_with_coord%>%
    filter(un_a3==exps$reporter_country_code_M49[i])%>%
    pull(rad)
  
  tst<-fun_coun(nm=exps$reporter_country_code_M49[i])
  cent_bra <- cent%>%filter(un_a3==exps$reporter_country_code_M49[i])%>%st_buffer(buf)
  
  tst_ext<-tst%>%
    mutate(vld=st_is_valid(tst))%>%
    filter(vld==TRUE)
  
  inter_temp<-st_intersection(cent_bra,tst_ext)%>%
    select(id,value=Value)%>%
    left_join(world%>%st_drop_geometry(),by=c('id'='name'))
  
  inter_temp<- inter_temp%>%
    mutate(vld=st_is_valid(inter_temp))%>%
    filter(vld==TRUE)
  
  inter<-inter%>%bind_rows(inter_temp)
}


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 80, 
  height = 80, 
  units = "cm", 
  dpi = 300 
)

# Set colors
pal <- c(
  "Africa" = "#FFAD05", "Asia" = "#FF595E", "Europe" = "#8AC926",
  "South America" = "#1982C4", "North America" = "#6A4C93",
  "Oceania" = "#6B4D57"
)

col_world <- "grey20"
col_back <- "#1D201F"

# Create graticule
grat <-  sf::st_graticule(lat = c(-89.9, seq(-90, 60, 30), 89.9))%>%
  sf::st_transform(crs="ESRI:54030")

ggplot()+
  geom_sf(world,mapping=aes(geometry=geometry),fill=col_world)+
  geom_sf(
    dorl,
    mapping=aes(geometry=geometry),
    fill=alpha("white",0.75),color=alpha("white",0.2)
  )+
  geom_sf(inter,mapping=aes(
    lwd=value,color=continent,alpha=value,geometry=geometry
  ))+
  geom_sf(
    grat,mapping=aes(geometry=geometry),
    alpha=0.1,col="white",
    inherit.aes='F'
  )+
  geom_sf(
    dorl,
    mapping=aes(geometry=geometry),
    fill=NA,color=col_back
  )+
  geom_richtext(
    cent_with_coord%>%arrange(-sm_clean)%>%head(16),
    mapping=aes(
      #geometry=geometry,
      label=lab,size=sm_clean,x=X,y=Y
    ),
    family="ral",alpha=0.8,lineheight=0.15,
    fill = NA, label.color = NA
    
  )+
  scale_color_manual(values=pal)+
  scale_size(range=c(3,30))+
  scale_alpha(range=c(0.25,0.5))+
  theme_void()+
  theme(plot.background = element_rect(fill=col_back,color=NA))

