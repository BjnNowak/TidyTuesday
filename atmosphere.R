library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggtext)
library(showtext)
library(camcorder)
library(scico)

# Set fonts
font_add_google("Bebas Neue","beb")
font_add_google("Barlow Condensed","bar")
font_add_google("Staatliches","sta")
font_add_google("Bitter","bit")
font_add_google("Roboto","rob")
font_add_google("Open Sans","open")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10*0.5625, 
  units = "cm", 
  dpi = 300 
)


# Made different try for maps, so code is really messy this time !

# Load basemap
world <- ne_countries(scale = 110,type = "countries",returnclass = "sf")%>%
  mutate(iso_a3_eh=case_when(
    admin=='Norway'~'NOR',
    TRUE~iso_a3_eh
  ))
# Load CO2 data
# Source: https://ourworldindata.org/co2-emissions#cumulative-co2-emissions
capita<-read_csv('data/co-emissions-per-capita.csv')
annual<-read_csv('data/annual-co2-emissions-per-country.csv')
trade<-read_csv('data/share-co2-embedded-in-trade.csv')
cum<-read_csv('data/cumulative-co-emissions.csv')
change<-read_csv('data/change-co2-annual-pct.csv')



capita_clean<-capita%>%
  drop_na()%>%
  group_by(Entity)%>%
  filter(Year==max(Year))%>%
  ungroup()

colnames(capita_clean)[4]<-'capita'

annual_clean<-annual%>%
  drop_na()%>%
  group_by(Entity)%>%
  filter(Year==max(Year))%>%
  ungroup()

colnames(annual_clean)[4]<-'annual'

trade_clean<-trade%>%
  drop_na()%>%
  group_by(Entity)%>%
  filter(Year==max(Year))%>%
  ungroup()

colnames(trade_clean)[4]<-'trade'

colnames(cum)[4]<-'cumulative'
cum_clean<-cum%>%
  drop_na()%>%
  group_by(Code)%>%
  summarise(cum=sum(cumulative))%>%
  ungroup()



change_clean<-change%>%
  drop_na()%>%
  group_by(Entity)%>%
  filter(Year==max(Year))%>%
  ungroup()

colnames(change_clean)[4]<-'change'

world_capita<-world%>%
  left_join(capita_clean,by=c('iso_a3_eh'='Code'))

world_annual<-world%>%
  left_join(annual_clean,by=c('iso_a3_eh'='Code'))

world_trade<-world%>%
  left_join(trade_clean,by=c('iso_a3_eh'='Code'))

world_cum<-world%>%
  left_join(cum_clean,by=c('iso_a3_eh'='Code'))

world_change<-world%>%
  left_join(change_clean,by=c('iso_a3_eh'='Code'))%>%
  mutate(chg=case_when(
    change<0~"Decrease",
    change>0~"Increase",
    change==0~"Stable"
  ))

grat <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9))

theme_custom <- theme_void()+
  theme(
    plot.background = element_rect(fill="#03051A",color=NA),
    legend.position = "bottom",
    legend.title = element_markdown(color="white",family='rob',size=18,margin=margin(0.2,0,-0.2,0,'cm')),
    legend.text = element_markdown(color="white",family='open',size=16,margin=margin(-0.15,0,0,0,'cm')),
    legend.key = element_rect(color = NA)
                                   
  )

per_capita<-ggplot()+
  geom_sf(
    world_capita,
    mapping=aes(fill=capita,geometry=geometry),
    color=alpha("white",0.1)
  )+
  geom_sf(
    grat, mapping=aes(geometry=geometry),
    col=alpha("white",0.2),lwd=0.25
  )+
  guides(
    fill=guide_bins(
      title.position = 'top',
      title.hjust = 0.5,
      keywidth = unit(1,'cm'),
      keyheight = unit(0.25,'cm')
    )
  )+
  scale_fill_viridis_b(
    option='cividis',
    breaks=seq(3,12,3),
    na.value = "grey10",
    labels=function(x){paste(x, 't')}
    #nbreaks=5
  )+
  labs(fill="**Per capita annual** CO<sub>2</sub> emissions for year 2021")+
  coord_sf(crs='+proj=lask')+
  theme_custom



annual<-ggplot()+
  geom_sf(
    world_annual,
    mapping=aes(fill=annual/1000000000,geometry=geometry),
    color=alpha("white",0.1)
  )+
  geom_sf(
    grat, mapping=aes(geometry=geometry),
    col=alpha("white",0.2),lwd=0.25
  )+
  guides(
    fill=guide_bins(
      title.position = 'top',
      title.hjust = 0.5,
      keywidth = unit(1,'cm'),
      keyheight = unit(0.25,'cm')
    )
  )+
  scale_fill_viridis_b(
    option='cividis',
    breaks=seq(1,4,1),
    na.value = "grey10",
    labels=function(x){paste(x, 'Gt')}
    #nbreaks=5
  )+
  labs(fill="**Total annual** CO<sub>2</sub> emissions for year 2021")+
  coord_sf(crs='+proj=lask')+
  theme_custom

annual



trade<-ggplot()+
  geom_sf(
    world_trade,
    mapping=aes(fill=trade,geometry=geometry),
    color=alpha("white",0.1)
  )+
  geom_sf(
    grat, mapping=aes(geometry=geometry),
    col=alpha("white",0.2),lwd=0.25
  )+
  guides(
    fill=guide_bins(
      title.position = 'top',
      title.hjust = 0.5,
      keywidth = unit(1,'cm'),
      keyheight = unit(0.25,'cm')
    )
  )+
  scale_fill_viridis_b(
    option='cividis',
    breaks=seq(-60,60,30),
    na.value = "grey10",
    labels=function(x){paste(x, '%')}
    #nbreaks=5
  )+
  labs(fill="CO<sub>2</sub> emissions **embedded in trade** for year 2020")+
  coord_sf(crs='+proj=lask')+
  theme_custom

cum<-ggplot()+
  geom_sf(
    world_cum,
    mapping=aes(fill=cum/1000000000,geometry=geometry),
    color=alpha("white",0.1)
  )+
  geom_sf(
    grat, mapping=aes(geometry=geometry),
    col=alpha("white",0.2),lwd=0.25
  )+
  guides(
    fill=guide_bins(
      title.position = 'top',
      title.hjust = 0.5,
      keywidth = unit(1,'cm'),
      keyheight = unit(0.25,'cm')
    )
  )+
  scale_fill_viridis_b(
    option='cividis',
    breaks=seq(50,250,50),
    na.value = "grey10",
    labels=function(x){paste(x, 'Gt')}
    #nbreaks=5
  )+
  labs(fill="CO<sub>2</sub> emissions **embedded in trade** for year 2020")+
  coord_sf(crs='+proj=lask')+
  theme_custom

cum
trade

per_capita
annual


world_trade <- world_trade%>%
  mutate(trade_cl=case_when(
    trade<(-40)~'A',
    trade<(-20)~'B',
    trade<0~'C',
    trade<20~'D',
    trade<40~'E',
    trade>=40~'F'
  ))

pal<-scico(n=6,palette = 'imola', direction = 1)

col_pal<-c(
  'A'=pal[1],
  'B'=pal[2],
  'C'=pal[3],
  'D'=pal[4],
  'E'=pal[5],
  'F'=pal[6]
)

trade<-ggplot()+
  geom_sf(
    world_trade,
    mapping=aes(fill=trade_cl,geometry=geometry),
    color=alpha("white",0.1)
  )+
  geom_sf(
    grat, mapping=aes(geometry=geometry),
    col=alpha("white",0.2),lwd=0.25
  )+
  
  scale_fill_manual(
    values=col_pal,
    na.value = "grey10"
  )+
  guides(
    fill=guide_legend(
      title.position = 'top',
      title.hjust = 0.5,
      label.position = 'bottom',
      keywidth = unit(1,'cm'),
      keyheight = unit(0.25,'cm'),
      nrow = 1
    )
  )+
  labs(fill="CO<sub>2</sub> emissions **embedded in trade** for year 2020")+
  coord_sf(crs='+proj=lask')+
  theme_custom+
  theme(legend.spacing.x = unit(0, 'cm'))
trade




annual_dorl<-cartogram::cartogram_dorling(
  world_annual%>%drop_na(annual)%>%st_transform(crs='+proj=bertin1953'),
  weight='annual', 
  k = 5,m_weight = 0.05, itermax = 1000
)

ggplot(annual_dorl)+
  geom_sf(
    world_annual%>%st_transform(crs='+proj=bertin1953')%>%filter(admin!="Antarctica"),
    mapping=aes(geometry=geometry),
    fill="#5D3239",color=alpha("white",0.05)
  )+
  geom_sf(
    annual_dorl,
    mapping=aes(geometry=geometry),
    fill="#EF6F6C",color=alpha("white",0.1)
  )+
  geom_sf_text(
    annual_dorl%>%arrange(-annual)%>%head(5)%>%mutate(admin=case_when(admin=="United States of America"~"USA",TRUE~admin)),
    mapping=aes(label=admin,geometry=geometry),
    family='rob', color="white", size=4.5
  )+
  theme_custom

capita_carto<-cartogram::cartogram_ncont(
  world_capita%>%drop_na(capita)%>%st_transform(crs='+proj=bertin1953'),
  weight='capita'
)

ggplot(capita_carto)+
  geom_sf(
    capita_carto,
    mapping=aes(geometry=geometry),
    fill="#EF6F6C",color=alpha("white",0.1)
  )+
  theme_custom

capita_dorl<-cartogram::cartogram_dorling(
  world_capita%>%drop_na(capita)%>%st_transform(crs='+proj=bertin1953'),
  weight='capita', 
  k = 1.5,m_weight = 0.05, itermax = 1000
)

ggplot()+
  geom_sf(
    world_annual%>%st_transform(crs='+proj=bertin1953')%>%filter(admin!="Antarctica"),
    mapping=aes(geometry=geometry),
    fill="#5D3239",color=alpha("white",0.05)
  )+
  geom_sf(
    capita_dorl,
    mapping=aes(geometry=geometry),
    fill="#EF6F6C",color=alpha("white",0.1)
  )+
  geom_sf_text(
    capita_dorl%>%arrange(-capita)%>%head(100)%>%mutate(admin=case_when(admin=="United States of America"~"USA",TRUE~admin)),
    mapping=aes(label=admin,geometry=geometry),
    family='rob', color="white", size=4.5
  )+
  theme_custom

pal_chg <- c(
  #'Increase'='#FFCA3A',
  'Increase'='#7F2982',
  'Decrease'='#04E762',
  'Stable'='grey10'
)

pal<-scico(n=6,palette = 'imola', direction = 1)
pal_chg <- c(
  #'Increase'='#FFCA3A',
  'Increase'=pal[6],
  'Decrease'=pal[1],
  'Stable'='grey10'
)

ggplot()+
  geom_sf(
    world_change,mapping=aes(geometry=geometry,fill=chg),
    color=alpha("white",0.1)
  )+
  geom_sf(
    grat, mapping=aes(geometry=geometry),
    col=alpha("white",0.2),lwd=0.25
  )+
  scale_fill_manual(
    values=pal_chg,
    na.value='grey10'
  )+
  guides(fill='none')+
  #coord_sf(crs='+proj=goode')+
  coord_sf(crs='+proj=adams_ws1')+
  theme_custom

continent <- world_cum%>%
  group_by(continent)%>%
  mutate(sm=sum(cum,na.rm=TRUE))%>%
  ungroup()%>%
  mutate(vld=st_is_valid(world_cum))%>%
  filter(vld==TRUE)%>%
  group_by(continent)%>%
  summarize(sm=sum(sm))%>%
  ungroup()%>%
  st_centroid()

ggplot()+
  geom_sf(
    world_annual%>%filter(continent!="Antarctica")%>%filter(continent!="Seven seas (open ocean)"),
    mapping=aes(geometry=geometry),
    fill="#5D3239",color=alpha("white",0.05)
  )+
  geom_sf(
    grat, mapping=aes(geometry=geometry),
    col=alpha("white",0.2),lwd=0.25
  )+
  geom_sf(
    continent%>%filter(continent!="Antarctica")%>%filter(continent!="Seven seas (open ocean)"),
    mapping=aes(geometry=geometry, size=sm),
    pch=21,fill="#EF6F6C",color=alpha("white",0.1)
  )+
  guides(size='none')+
  scale_size(range=c(1,16))+
  coord_sf(crs='+proj=leac')+
  theme_custom

