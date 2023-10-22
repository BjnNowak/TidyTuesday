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
library(geomtextpath)

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
  ))

cent <- world%>%
  # Convert WGS84 to projected crs (here Robinson)
  sf::st_transform(crs="ESRI:54030")%>%
  #sf::st_transform(crs="+proj=eck6")%>%
  st_centroid()
#sf::st_transform(crs="EPSG:4326")


cent_with_coord<-cent%>%
  bind_cols(st_coordinates(cent))%>%
  st_drop_geometry()



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
  #filter(item=="Tea leaves")%>%
  filter(Value>0)%>%
  drop_na(Value)%>%
  filter(partner_country!="China, Hong Kong SAR")%>%
  filter(partner_country!="China, Taiwan Province of")
#filter(partner_country!="Malta")%>%
#filter(partner_country!="Singapore")


exps<-data%>%
  group_by(reporter_country)%>%
  summarize(sm=sum(Value))%>%
  ungroup()%>%
  arrange(-sm)%>%
  drop_na()%>%
  filter(sm>0)%>%
  mutate(reporter_country_lab = case_when(
    reporter_country=='China, mainland'~'China',
    reporter_country=='United Kingdom of Great Britain and Northern Ireland'~'United Kingdom',
    TRUE~reporter_country
  ))%>%
  mutate(rk=row_number())%>%
  mutate(lab=glue::glue("{rk}. {reporter_country_lab}"))

exps_names <- exps%>%pull(reporter_country)

fun_coun <- function(nm){
  
  test<-data%>%
    filter(reporter_country==nm)%>%
    left_join(cent_with_coord,by=c("partner_country_code_M49"="un_a3"))
  
  cent_brazil<-cent_with_coord%>%
    filter(name==nm)%>%
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


fun_plot <- function(i){
  
  buf<-10000
  
  tst<-fun_coun(nm=exps$reporter_country[i])
  cent_bra <- cent%>%filter(name==exps$reporter_country[i])%>%st_buffer(buf)

  tst_ext<-tst%>%
    mutate(vld=st_is_valid(tst))%>%
    filter(vld==TRUE)

  inter<-st_intersection(cent_bra,tst_ext)%>%
    select(id,value=Value)%>%
    left_join(world%>%st_drop_geometry(),by=c('id'='name'))
  
  
  
  pl<-ggplot(inter%>%drop_na(continent),aes(lwd=value,color=continent,alpha=value))+

    geom_sf()+
    geom_textsf(
      inter%>%drop_na(continent)%>%arrange(-value)%>%head(1),
      mapping=aes(label=id,geometry=geometry),
      linecolour = NA,size=10,
      inherit.aes=F
    )+
    geom_sf(
      cent%>%filter(name==exps$reporter_country[i]),
      mapping=aes(geometry=geometry),
      inherit.aes=FALSE, size = 32,
      color='dimgrey'
    )+
    geom_sf(
      cent_bra%>%filter(name==exps$reporter_country[i]),
      mapping=aes(geometry=geometry),
      fill=NA,color="dimgrey",lwd=3,
      inherit.aes=FALSE
    )+
    scale_alpha(range=c(0.25,0.5),limits=c(1,441014))+
    scale_linewidth(range=c(2,32),limits=c(1,441014))+
    scale_color_manual(values=pal)+
    scale_fill_manual(values=pal)+
    guides(color='none',alpha='none',linewidth='none')+
    labs(
      title=exps$lab[i],
      subtitle=glue::glue(
        #'{round(exps%>%filter(reporter_country==exps$reporter_country[i])%>%pull(sm)/1000)} kt exported'
        '{formatC(round(exps%>%filter(reporter_country==exps$reporter_country[i])%>%pull(sm)/1000),big.mark = ",")} kt exported'
      )
    )+
    theme_void()+
    theme(
      plot.margin=margin(0.5,0.5,0.5,0.5,'cm'),
      plot.title=element_text(hjust=0.5,family='bit',size=80,face='bold',color=col_tit),
      plot.subtitle = element_text(hjust=0.5,family='ral',size=75,color=col_tit)
    )
  
  
  return(pl)
  
}


# Col 
fill_coun <- '#ffcdbc'
col_coun <- '#F5853F'
col_trade <- '#2D080A'
col_tit <- '#130303'


pal <- c(
  "Africa" = "#FFAD05", "Asia" = "#FF595E", "Europe" = "#8AC926",
  "South America" = "#1982C4", "North America" = "#6A4C93",
  "Oceania" = "#6B4D57"
)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21, 
  height = 21, 
  units = "cm", 
  dpi = 300 
)

fun_plot(1)


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21*4, 
  height = 21*4, 
  units = "cm", 
  dpi = 300 
)

fun_plot(1)+fun_plot(2)+fun_plot(3)+fun_plot(4)+
  fun_plot(5)+fun_plot(6)+fun_plot(7)+fun_plot(8)+
  fun_plot(9)+fun_plot(10)+fun_plot(11)+fun_plot(12)+
  fun_plot(13)+fun_plot(14)+fun_plot(15)+fun_plot(16)& 
  theme(plot.background = element_rect(fill=NA,color=NA))


