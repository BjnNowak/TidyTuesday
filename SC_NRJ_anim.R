
library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)

# Set fonts
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Fira Sans","fira")
font_add_google("Jost","jost")
font_add_google("Staatliches","sta")
font_add_google("Open Sans","open")
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

owid_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')


clean<-owid_energy%>%
  filter(year>2010)%>%
  mutate(iso_code=case_when(
    country=="Kosovo"~'KOS',
    TRUE~iso_code
  ))%>%
  drop_na(iso_code)%>%
  group_by(country)%>%
  summarize(
    iso_code=iso_code[1],
    biofuel=mean(na.omit(biofuel_elec_per_capita)),
    coal=mean(na.omit(coal_elec_per_capita)),
    gas=mean(na.omit(gas_elec_per_capita)),
    hydro=mean(na.omit(hydro_elec_per_capita)),
    nuke=mean(na.omit(nuclear_elec_per_capita)),
    oil=mean(na.omit(oil_elec_per_capita)),
    solar=mean(na.omit(solar_elec_per_capita)),
    wind=mean(na.omit(wind_elec_per_capita))
  )%>%
  ungroup()


fun_country<-function(nm){

fr<-clean%>%
  filter(iso_code==nm)%>%
  pivot_longer(
    !c(country,iso_code),names_to="type",values_to="value" 
  )%>%
  
  mutate(x=case_when(
    type%in%c("coal","nuke","hydro")~-1/3,
    type%in%c("gas","wind")~0,
    type%in%c("oil","biofuel","solar")~+1/3
  ))%>%
  mutate(y=case_when(
    type%in%c("coal","gas","oil")~-1/3,
    type%in%c("nuke","biofuel")~0,
    type%in%c("hydro","wind","solar")~+1/3
  ))

return(fr)

}

fun_country_type<-function(nm,tp){
  
  fr<-clean%>%
    filter(iso_code==nm)%>%
    pivot_longer(
      !c(country,iso_code),names_to="type",values_to="value" 
    )%>%
    
    mutate(x=case_when(
      type%in%c("coal","nuke","hydro")~-1/3,
      type%in%c("gas","wind")~0,
      type%in%c("oil","biofuel","solar")~+1/3
    ))%>%
    mutate(y=case_when(
      type%in%c("coal","gas","oil")~-1/3,
      type%in%c("nuke","biofuel")~0,
      type%in%c("hydro","wind","solar")~+1/3
    ))%>%
    
    group_by(country)%>%
    mutate(value=value[type==tp])%>%
    mutate(type=tp)
  
  return(fr)
  
}

# Country coordinates
coords<-tibble(
  iso_code=c(
    "FRA","ESP","PRT","MLT",
    "GBR","IRL","ISL","BEL",
    "LUX",'NLD','CHE','ITA',
    'SVN','NOR','DNK','DEU',
    'CZE','AUT','HRV',"BIH",
    'SWE','POL','SVK','HUN',
    'MNE','KOS','ALB','FIN',
    'EST','LTU','BLR','UKR',
    'ROU','SRB','MKD','GRC',
    'LVA','RUS','MDA','BGR',
    'GEO','TUR','CYP','AZE',
    'ARM'
  ),
  x_center=c(
    1,1,0,1,
    1,0,0,2,
    2,3,3,3,
    3,4,4,4,
    4,4,4,4,
    5,5,5,5,
    5,5,5,6,
    6,6,6,6,
    6,6,6,6,
    7,7,7,7,
    8,8,8,9,
    9
  ),
  y_center=c(
    -4,-5,-5,-7,
    -2,-2,-0,-3,
    -4,-3,-4,-5,
    -6,-0,-2,-3,
    -4,-5,-6,-7,
    -0,-3,-4,-5,
    -6,-7,-8,-0,
    -1,-2,-3,-4,
    -5,-6,-7,-8,
    -2,-3,-5,-6,
    -5,-6,-8,-5,
    -6
  )
)

fun_prep<-function(tp){
dt<-fun_country_type("FRA",tp)%>%
  bind_rows(fun_country_type("ESP",tp))%>%
  bind_rows(fun_country_type("PRT",tp))%>%
  bind_rows(fun_country_type("MLT",tp))%>%
  bind_rows(fun_country_type("GBR",tp))%>%
  bind_rows(fun_country_type("IRL",tp))%>%
  bind_rows(fun_country_type("ISL",tp))%>%
  bind_rows(fun_country_type("BEL",tp))%>%
  bind_rows(fun_country_type("LUX",tp))%>%
  bind_rows(fun_country_type("NLD",tp))%>%
  bind_rows(fun_country_type("CHE",tp))%>%
  bind_rows(fun_country_type("ITA",tp))%>%
  bind_rows(fun_country_type("SVN",tp))%>%
  bind_rows(fun_country_type("NOR",tp))%>%
  bind_rows(fun_country_type("DNK",tp))%>%
  bind_rows(fun_country_type("DEU",tp))%>%
  bind_rows(fun_country_type("CZE",tp))%>%
  bind_rows(fun_country_type("AUT",tp))%>%
  bind_rows(fun_country_type("HRV",tp))%>%
  bind_rows(fun_country_type("BIH",tp))%>%
  bind_rows(fun_country_type("SWE",tp))%>%
  bind_rows(fun_country_type("POL",tp))%>%
  bind_rows(fun_country_type("SVK",tp))%>%
  bind_rows(fun_country_type("HUN",tp))%>%
  bind_rows(fun_country_type("MNE",tp))%>%
  bind_rows(fun_country_type("KOS",tp))%>%
  bind_rows(fun_country_type("ALB",tp))%>%
  bind_rows(fun_country_type("FIN",tp))%>%
  bind_rows(fun_country_type("EST",tp))%>%
  bind_rows(fun_country_type("LTU",tp))%>%
  bind_rows(fun_country_type("BLR",tp))%>%
  bind_rows(fun_country_type("UKR",tp))%>%
  bind_rows(fun_country_type("ROU",tp))%>%
  bind_rows(fun_country_type("SRB",tp))%>%
  bind_rows(fun_country_type("MKD",tp))%>%
  bind_rows(fun_country_type("GRC",tp))%>%
  bind_rows(fun_country_type("LVA",tp))%>%
  bind_rows(fun_country_type("RUS",tp))%>%
  bind_rows(fun_country_type("MDA",tp))%>%
  bind_rows(fun_country_type("BGR",tp))%>%
  bind_rows(fun_country_type("GEO",tp))%>%
  bind_rows(fun_country_type("TUR",tp))%>%
  bind_rows(fun_country_type("CYP",tp))%>%
  bind_rows(fun_country_type("AZE",tp))%>%
  bind_rows(fun_country_type("ARM",tp))%>%
  left_join(coords)%>%
  mutate(
    x=x+x_center,
    y=y+y_center
  )%>%
  mutate(cl=case_when(
    type%in%c('oil','gas','coal')~'fossil',
    type%in%c('nuke','biofuel')~'low_carb',
    type%in%c('hydro','wind','solar')~'renew'
  ))

return(dt)

}

bordR<-coords%>%
  # horizontal lines
  mutate(
    y_low=y_center-0.5*1/3,
    y_high=y_center+0.5*1/3,
    x_min=x_center-0.5,
    x_max=x_center+0.5
  )%>%
  # vertical lines
  mutate(
    x_low=x_center-0.5*1/3,
    x_high=x_center+0.5*1/3,
    y_min=y_center-0.5,
    y_max=y_center+0.5
  )

col_line<-"grey50"
col_bord<-"white"

lwd_line<-0.1
lwd_bord<-0.5

pal<-c(
  'fossil'="#F21C8D",
  'low_carb'='#FEFF00',
  'renew'='#01A0C6'
)



col_back<-"grey25"


fun_plot<-function(tp,lab){

dt<-fun_prep(tp)

pl<-ggplot()+
  # Back
  geom_rect(
    bordR,
    mapping=aes(xmin=x_min,xmax=x_max,ymin=y_min,ymax=y_max),
    fill=col_back, col=NA
  )+
  # Horizontal lines
  geom_segment(
    bordR,
    mapping=aes(y=y_low,yend=y_low,x=x_min,xend=x_max),
    col=col_line,lwd=lwd_line
  )+
  geom_segment(
    bordR,
    mapping=aes(y=y_high,yend=y_high,x=x_min,xend=x_max),
    col=col_line,lwd=lwd_line
  )+
  # Vertical lines
  geom_segment(
    bordR,
    mapping=aes(x=x_low,xend=x_low,y=y_min,yend=y_max),
    col=col_line,lwd=lwd_line
  )+
  geom_segment(
    bordR,
    mapping=aes(x=x_high,xend=x_high,y=y_min,yend=y_max),
    col=col_line,lwd=lwd_line
  )+
  # Borders
  geom_rect(
    bordR,
    mapping=aes(xmin=x_min,xmax=x_max,ymin=y_min,ymax=y_max),
    fill=NA, col=col_bord,lwd=lwd_bord
  )+

  # Electricity per capita
  geom_point(
    dt%>%filter(value>0),
    mapping=aes(x=x,y=y,size=value,color=cl),
    pch=15
  )+
  
  # Country name
  geom_text(
    dt,
    mapping=aes(x=x_center,y=y_center,label=iso_code),
    family="bit",size=9,col="white"
  )+
  scale_color_manual(values=pal)+
  scale_size(
    limits=c(1,39000),
    range=c(1,7))+
  labs(
    title="Electricity consumption per capita in Europe",
    subtitle=lab,
    caption="**Data** OWID **| Plot** @BjnNowak"
  )+
  #scale_x_continuous(limits=c(-1,1))+
  #scale_y_continuous(limits=c(-1,1))+
  guides(color='none',size='none')+
  coord_fixed()+
  theme_void()+
  theme(
    plot.background=element_rect(fill="#1C2321",color=NA),
    plot.title=element_markdown(
      size=60,family="sta",color="white"
    ),
    plot.subtitle=element_markdown(
      size=45,family="open",margin=margin(0.25,0,0,0,"cm"),
      color="white"
    ),
    plot.margin=margin(1,1,1,1,"cm"),
    plot.caption=element_markdown(
      size=30,family="open",hjust=0,
      color="white"
    )
  )
  
return(pl)

}

fun_plot("coal","From **coal** sources")
fun_plot("oil","From **oil** sources")
fun_plot("gas","From **gas** sources")
fun_plot("biofuel","From **biofuel** sources")
fun_plot("nuke","From **nuclear** sources")
fun_plot("hydro","From **hydropower** sources")
fun_plot("solar","From **solar** sources")
fun_plot("wind","From **wind** sources")

