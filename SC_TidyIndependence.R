
# Clear space #
###############

rm(list=ls())
gc()

# Libraries
library(sp)
library(extrafont)
library(sf)
library(plyr)
library(tidyverse)
library(showtext)
library(ggthemes)

# Load data
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

ind <- holidays%>%
  filter(name_of_holiday!="None")%>%
  filter(country!="NA")%>%
  group_by(country)%>% 
  summarise(
    min_year = max(year),
    independence = last(independence_from)
  )%>%
  filter(independence!='NA')%>%
  mutate(country=case_when(
    country=='North Macedonia' ~"The former Yugoslav Republic of Macedonia",
    country=='Bahamas, The'~"Bahamas",
    country=='Brunei'~"Brunei Darussalam",
    country=='Congo, Democratic Republic of the'~"Democratic Republic of the Congo",
    country=='Congo, Republic of the'~'Congo',
    country=='Eswatini'~"Swaziland",
    country=='Micronesia'~"Micronesia, Federated States of",
    country=='São Tomé and Príncipe'~'Sao Tome and Principe',
    country=='Moldova'~"Republic of Moldova",
    country=='Myanmar'~"Burma",
    country=='North Korea'~"Korea, Democratic People's Republic of",
    country=='South Korea'~"Korea, Republic of",
    country=='South Sudan'~"Sudan",
    country=='Syria'~"Syrian Arab Republic",
    country=='Vietnam'~"Viet Nam",
    TRUE~country
  ))%>%
  mutate(period=case_when(
    min_year<1900~'Before 1900',
    min_year<1925~'Between 1900 and 1925',
    min_year<1950~'Between 1925 and 1950',
    min_year<1975~'Between 1950 and 1975',
    TRUE~'After 1975'
  ))%>%
  mutate(period_fac=factor(
    period, levels = c(
      'Before 1900',
      'Between 1900 and 1925',
      'Between 1925 and 1950',
      'Between 1950 and 1975',
      'After 1975'
  )))%>%
  mutate(independence=case_when(
    # Russia
    independence=='Soviet Union[55]'~'Soviet Union',
    independence=='Soviet Union[80]'~'Soviet Union',
    independence=='Russian Soviet Federative Socialist Republic'~'Soviet Union',
    independence=='Russian Soviet Federative Socialist Republic and German Empire'~'Soviet Union',
    independence=='Soviet Union'~'Russia',
    # Spain
    independence=='Spanish Empire[72]'~'Spanish Empire',
    independence=='Spanish Empire'~'Spain',
    TRUE~independence
  ))%>%
  mutate(independence=factor(
    independence, levels = c(
      'United Kingdom',
      'France',
      'Spain',
      'Russia',
      'Portugal'
    )))

# Load map
# shp available for dl here: https://thematicmapping.org/downloads/world_borders.php
wld<-read_sf(dsn = "TM_WORLD_BORDERS_SIMPL-0.3", layer="TM_WORLD_BORDERS_SIMPL-0.3")%>%
  filter(NAME!="Antarctica",NAME!="Greenland")

# Merge map and data
wld_ind<-merge(
  wld,ind,
  by.x='NAME',
  by.y='country'
)

# Extract centroids of both former and new countries
# and store as data frame to use with geom_segment()
ind_bis<-wld_ind%>%filter(
    independence=="France"|
    independence=="United Kingdom"|
    independence=="Spain"|
    independence=="Portugal"|
    independence=="Russia"
  )%>%
  st_centroid()%>%
  select(NAME,LON,LAT,independence)

df_ind<-cbind.data.frame(
  NAME=ind_bis$NAME,
  LON_coun=ind_bis$LON,
  LAT_coun=ind_bis$LAT,
  indep=ind_bis$independence
)

centr_ind<-wld%>%
  filter(
    NAME=="France"|
      NAME=="United Kingdom"|
      NAME=="Spain"|
      NAME=="Portugal"|
      NAME=="Russia"
  )%>%
  st_centroid()%>%
  select(NAME,LON,LAT)

df_centr<-cbind.data.frame(
  indep=centr_ind$NAME,
  LON_ori=centr_ind$LON,
  LAT_ori=centr_ind$LAT
)

df<-merge(df_centr,df_ind,by="indep")



col_ax<-'#A5A58D'
col_pal<-c(
  "#ADB5BD","#6C757D","#495057","#343A40","#212529"
)
col_lin<-c("#e76f51","#264653","#2a9d8f","#FD96A9","#e9c46a")

back='white'
font_add_google(name = "Cabin", family = "Cabin")
showtext_auto()
font_add_google(name = "Playfair Display", family = "Playfair Display")
showtext_auto()

ft2<-'Playfair Display'
ft<-'Cabin'

theme_bare <- theme(
  text = element_text(family = ft),
  axis.line = element_blank(), 
  axis.text.x = element_text(size=11), 
  axis.text.y = element_text(size=11),
  axis.ticks = element_blank(), 
  axis.title.y = element_blank(), 
  axis.title.x = element_text(size=12),
  legend.text=element_text(size=9,family=ft),
  legend.title=element_text(size=10,family=ft  ),
  legend.key = element_blank(),
  legend.background=element_rect(fill =back),
  legend.position = "bottom",
  legend.spacing.x = unit(0.5, "cm"),
  plot.title=element_text(size=14,hjust=0.5),
  plot.subtitle=element_blank(),
  plot.background = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

df<-df%>%mutate(indep=factor(
  indep, levels = c(
    'Portugal',
    'Russia',
    'France',
    'Spain',
    'United Kingdom'
  )))

map<-ggplot()+ 
  geom_sf(data = wld,fill="white",color=col_ax)+                # All countries
  geom_sf(data = wld_ind, aes(fill=period_fac),color=col_ax)+   # Date of independence
  geom_segment(                                                 # Add past empire lines
    data=df,
    aes(
      x=LON_ori,y=LAT_ori,xend=LON_coun,yend=LAT_coun,
      color=indep
    ),
    alpha=0.8)+
  guides(color = FALSE, fill = FALSE)+
  theme_map()+
  scale_fill_manual(
    values=alpha(col_pal, 0.5)
  )+
  scale_color_manual(
    values=col_lin
  )

leg1<-ggplot(data=wld_ind, aes(x =period_fac ))+
  geom_bar(aes(fill=period_fac))+
  scale_fill_manual(values=alpha(col_pal, 0.5))+
  coord_flip()+
  guides(fill=FALSE)+
  theme_bare+
  labs(
    title="Date of independence",
    y="\nNumber of countries"
  )

leg2<-ggplot(data=df, aes(x =indep ))+
  geom_bar(aes(fill=indep))+
  scale_fill_manual(values=col_lin)+
  coord_flip()+
  guides(fill=FALSE)+
  theme_bare+
  labs(
    title="Former empire",
    y="\nNumber of countries"
  )

library(cowplot)
ggdraw() +
  draw_plot(map, x = 0, y = 0.05, width = 1, height = 1) +
  draw_plot(leg1, x = 0.05, y = 0.02, width = .45, height = .25) +
  draw_plot(leg2, x = 0.5, y = 0.02, width = 0.45, height = 0.25)+
  draw_plot_label(
    label = ' "World-wide, British Leaving Day is never out of season." ', 
    size = 18,family=ft2,hjust=0,color="#343a40",
    x = 0.05, y = 0.95)+
  draw_plot_label(
    label = ' Walter Russell Mead', 
    size = 14,family=ft2,hjust=0,color="#343a40",
    x = 0.06, y = 0.90)+
  draw_plot_label(
    label = "Data: TidyTuesday and I. Velasquez  |  Plot: @BjnNowak", 
    size = 10,family=ft,color="#343a40",hjust=0,vjust=0,
    x = 0.97, y = 0.8,angle=270)
