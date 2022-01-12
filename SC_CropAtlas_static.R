
# Effacement de la mémoire
rm(list=ls())
gc()

# Load libraries
library(tidyverse)
library(maps)
library(sf)
library(cowplot)
library(ggtext)
library(showtext)
library(camcorder)
library(viridis)
library(grid)


# Load fonts (for ggplot only)
font_add_google("Barlow Condensed","barlow")
font_add_google("Archivo Narrow","arch")
font_add_google("Kreon","kreon")
# Automatically use {showtext} for plots
showtext_auto()

crop_ft <- "arch"
head_ft <- "kreon"

# Loading data

data <- readr::read_csv(
  'https://raw.githubusercontent.com/BjnNowak/playground/main/data/cereal_area.csv')

# Preparing world map
states <- st_as_sf(maps::map(database="world", plot = FALSE, fill = TRUE))
country_to_remove <- c(
  'Antarctica','Greenland', 'French Southern and Antarctic Lands'
)
# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

states <- states %>%
  mutate(Area=ID)%>%
  select(-ID)%>%
  filter(Area %!in% country_to_remove)%>%
  mutate(Area=case_when(
    Area=='American Samoa'~'USA',
    Area=='Anguila'~'UK',
    Area=='Antigua'~'Antigua and Barbuda',
    Area=='Aruba'~'Netherlands',
    Area=='Barbuda'~'Antigua and Barbuda',
    Area=='Bermuda'~'UK',
    Area=='Canary Islands'~'Spain',
    Area=='Chagos Archipelago'~'UK',
    Area=='Christmas Island'~'Australia',
    Area=='Cocos Islands'~'Australia',
    Area=='Curacao'~'Netherlands',
    Area=='Falkland Islands'~'UK',
    Area=='French Guiana'~'France',
    Area=='Guadeloupe'~'France',
    Area=='Guam'~'USA',
    Area=='Guernsey'~'UK',
    Area=='Heard Island'~'Australia',
    Area=='Isle of Man'~'UK',
    Area=='Jersey'~'UK',
    Area=='Madeira Islands'~'Portugal',
    Area=='Martinique'~'France',
    Area=='Mayotte'~'France',
    Area=='Nevis'~'Saint Kitts and Nevis',
    Area=='Northern Mariana Islands'~'USA',
    Area=='Reunion'~'France',
    Area=='Saint Barthelemy'~'France',
    Area=='Saint Kitts'~'Saint Kitts and Nevis',
    Area=='Saint Martin'~'France',
    Area=='Saint Pierre and Miquelon'~'France',
    Area=='Siachen Glacier'~'India',
    Area=='South Sandwich Islands'~'UK',
    Area=='South Georgia'~'UK',
    Area=='Tobago'~'Trinidad and Tobago',
    Area=='Trinidad'~'Trinidad and Tobago',
    TRUE~Area
  ))%>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=270"))

####

# Cleaning data

#########

clean<-data%>%
  filter(Item=="Wheat")%>%
  filter(Element=='Yield')%>%
  filter(Area!='China')%>%
  mutate(Area=case_when(
    Area=='Bolivia (Plurinational State of)'~'Bolivia',
    Area=='Brunei Darussalam'~'Brunei',
    Area=='Cabo Verde'~'Cape Verde',
    Area=='China, Taiwan Province of'~'Taiwan',
    Area=='China, mainland'~'China',
    Area=='Congo'~'Republic of Congo',
    str_detect(Area,'Ivoire')~'Ivory Coast',
    Area=='Czechia'~'Czech Republic',
    Area=="Democratic People's Republic of Korea"~'North Korea',
    Area=='Iran (Islamic Republic of)'~'Iran',
    Area=="Lao People's Democratic Republic"~'Laos',
    Area=='Micronesia (Federated States of)'~'Micronesia',
    Area=='North Macedonia'~'Macedonia',
    Area=='Republic of Korea'~'South Korea',
    Area=='Republic of Moldova'~'Moldova',
    Area=='Russian Federation'~'Russia',
    Area=='United Kingdom of Great Britain and Northern Ireland'~'UK',
    Area=='United Republic of Tanzania'~'Tanzania',
    Area=='United States of America'~'USA',
    Area=='Venezuela (Bolivarian Republic of)'~'Venezuela',
    Area=='Viet Nam'~'Vietnam',
    TRUE~Area
  ))%>%
  mutate(Yield=round(Value/10000,2))%>%
  group_by(Year)%>%
  mutate(Yield_gap=(max(Yield)-Yield)/max(Yield)*100)%>%
  ungroup()%>%
  group_by(Area)%>%
  mutate(
    Yield_mean=round(mean(na.omit(Value)/10000),2)
  )%>%
  mutate(
    Yield_gap_mean=round(mean(na.omit(Yield_gap)),2)
  )%>%
  ungroup()

area<-data%>%
  filter(Item=="Wheat")%>%
  filter(Element=='Area harvested')%>%
  filter(Area!='China')%>%
  mutate(Area=case_when(
    Area=='Bolivia (Plurinational State of)'~'Bolivia',
    Area=='Brunei Darussalam'~'Brunei',
    Area=='Cabo Verde'~'Cape Verde',
    Area=='China, Taiwan Province of'~'Taiwan',
    Area=='China, mainland'~'China',
    Area=='Congo'~'Republic of Congo',
    str_detect(Area,'Ivoire')~'Ivory Coast',
    Area=='Czechia'~'Czech Republic',
    Area=="Democratic People's Republic of Korea"~'North Korea',
    Area=='Iran (Islamic Republic of)'~'Iran',
    Area=="Lao People's Democratic Republic"~'Laos',
    Area=='Micronesia (Federated States of)'~'Micronesia',
    Area=='North Macedonia'~'Macedonia',
    Area=='Republic of Korea'~'South Korea',
    Area=='Republic of Moldova'~'Moldova',
    Area=='Russian Federation'~'Russia',
    Area=='United Kingdom of Great Britain and Northern Ireland'~'UK',
    Area=='United Republic of Tanzania'~'Tanzania',
    Area=='United States of America'~'USA',
    Area=='Venezuela (Bolivarian Republic of)'~'Venezuela',
    Area=='Viet Nam'~'Vietnam',
    TRUE~Area
  ))%>%
  mutate(Area_harvested=round(Value/1000000,2))%>%
  group_by(Area)%>%
  mutate(
    Area_harvested_mean=round(
      mean(na.omit(Value)/10000),2))%>%
  ungroup()%>%
  select(Area,Area_harvested,Year)


test <- states%>%
  left_join(clean)



##########



test_2018<-test%>%filter(Year==2018)

gg<-ggplot(data=test_2018)+
  geom_sf(data=states,fill="lightgrey",size=0.05)+
  geom_sf(aes(fill=Yield_gap_mean),size=0.05)+
  guides(fill='none')+
  coord_sf(crs = "ESRI:54030")+
  cowplot::theme_minimal_grid()+
  scale_fill_viridis_b(
    option="G",
    breaks=seq(20, 80, by = 20),
    limits=c(0,100),
    labels = glue::glue("{seq(20, 80, by = 20)} %"),
    direction = 1
  )+
  guides(fill = guide_colorbar(
    barheight = unit(4, units = "mm"),
    barwidth = unit(100, units = "mm"),
    direction = "horizontal",
    ticks.colour = "white",
    ticks.linewidth = 1,
    title.position = "top",
    label.position = "bottom",
    title.hjust = 0.5)) +
  labs(
    title="Wheat yield gap",
    fill=""
  )+
  theme(
    plot.background = element_rect(fill=NA,color=NA),
    plot.title = element_text(hjust = 0.5,family = 'marker',size=50),
    plot.subtitle = element_markdown(size=20,family='open',lineheight =0.5),
    plot.caption = element_markdown(size=20,family='open'),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(color="grey80",size=0.1),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.justification = c(0.5, 0),
    #legend.title = element_markdown(family = "barlow", size = 30, hjust = 0.5,color="grey20"),
    #legend.text = element_markdown(family = "barlow", size = 30, hjust = 0.5,color="grey20"),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.spacing.x = unit(0, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  )

gg

legend <- cowplot::get_legend(gg)

gg_wheat<-ggplot(data=test_2018)+
  geom_sf(data=states,fill="lightgrey",size=0.05,color="white")+
  geom_sf(aes(fill=Yield_gap_mean),size=0.05,color="white")+
  guides(fill='none')+
  coord_sf(crs = "ESRI:54030")+
  cowplot::theme_minimal_grid()+
  scale_fill_viridis_b(
    option="G",
    breaks=seq(20, 80, by = 20),
    limits=c(0,100),
    labels = glue::glue("{seq(20, 80, by = 20)} %")  
  )+
  guides(fill = "none") +
  labs(
    title="Wheat",
    fill="**Yield gap:** difference between each country's yield and maximum yield"
  )+
  theme(
    plot.background = element_rect(fill=NA,color=NA),
    plot.title = element_text(hjust = 0.5,family = crop_ft,size=50,color="#061623"),
    plot.subtitle = element_markdown(size=20,family='open',lineheight =0.5),
    plot.caption = element_markdown(size=20,family='open'),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(color="grey80",size=0.1),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.justification = c(0.5, 0),
    legend.title = element_markdown(family = "barlow", size = 30, hjust = 0.5,color="grey20"),
    legend.text = element_markdown(family = "barlow", size = 30, hjust = 0.5,color="grey20"),
    legend.spacing.x = unit(0, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  )
gg_wheat



############


# Cleaning data

#########

clean_maize<-data%>%
  filter(Item=="Maize")%>%
  filter(Element=='Yield')%>%
  filter(Area!='China')%>%
  mutate(Area=case_when(
    Area=='Bolivia (Plurinational State of)'~'Bolivia',
    Area=='Brunei Darussalam'~'Brunei',
    Area=='Cabo Verde'~'Cape Verde',
    Area=='China, Taiwan Province of'~'Taiwan',
    Area=='China, mainland'~'China',
    Area=='Congo'~'Republic of Congo',
    str_detect(Area,'Ivoire')~'Ivory Coast',
    Area=='Czechia'~'Czech Republic',
    Area=="Democratic People's Republic of Korea"~'North Korea',
    Area=='Iran (Islamic Republic of)'~'Iran',
    Area=="Lao People's Democratic Republic"~'Laos',
    Area=='Micronesia (Federated States of)'~'Micronesia',
    Area=='North Macedonia'~'Macedonia',
    Area=='Republic of Korea'~'South Korea',
    Area=='Republic of Moldova'~'Moldova',
    Area=='Russian Federation'~'Russia',
    Area=='United Kingdom of Great Britain and Northern Ireland'~'UK',
    Area=='United Republic of Tanzania'~'Tanzania',
    Area=='United States of America'~'USA',
    Area=='Venezuela (Bolivarian Republic of)'~'Venezuela',
    Area=='Viet Nam'~'Vietnam',
    TRUE~Area
  ))%>%
  mutate(Yield=round(Value/10000,2))%>%
  filter(Yield<15)%>%
  group_by(Year)%>%
  mutate(Yield_gap=100-round(Yield/max(Yield)*100))%>%
  ungroup()%>%
  group_by(Area)%>%
  mutate(
    Yield_mean=round(mean(na.omit(Value)/10000),2)
  )%>%
  mutate(
    Yield_gap_mean=round(mean(na.omit(Yield_gap)),2)
  )%>%
  ungroup()

test_maize <- states%>%
  left_join(clean_maize)



##########




gg_maize<-ggplot(data=test_maize%>%filter(Year==2018))+
  geom_sf(data=states,fill="lightgrey",size=0.05,color="white")+
  geom_sf(aes(fill=Yield_gap_mean),size=0.05,color="white")+
  guides(fill='none')+
  coord_sf(crs = "ESRI:54030")+
  cowplot::theme_minimal_grid()+
  scale_fill_viridis_b(
    option="G",
    breaks=seq(20, 80, by = 20),
    limits=c(0,100),
    labels = glue::glue("{seq(20, 80, by = 20)} %")  
  )+
  guides(fill='none') +
  labs(
    title="Maize",

    fill="**Yield gap:** difference between each country's yield and maximum yield"
  )+
  theme(
    plot.background = element_rect(fill=NA,color=NA),
    plot.title = element_text(hjust = 0.5,family = crop_ft,size=50,color="#061623"),
    plot.subtitle = element_markdown(size=20,family='open',lineheight =0.5),
    plot.caption = element_markdown(size=20,family='open'),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(color="grey80",size=0.1),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.justification = c(0.5, 0),
    legend.title = element_markdown(family = "barlow", size = 30, hjust = 0.5,color="grey20"),
    legend.text = element_markdown(family = "barlow", size = 30, hjust = 0.5,color="grey20"),
    legend.spacing.x = unit(0, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  )


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10*1.618, 
  height = 22, 
  units = "cm", 
  dpi = 300 
)


rect <- rectGrob(
  x = 0,
  y = 0,
  width = 4,
  height = 4,
  gp = gpar(fill = "#FFF1EB", alpha = 1)
)

ggdraw()+
  draw_grob(rect)+
  draw_plot(gg_wheat,x=0.05,y=0.4,height=0.4,width=0.9)+
  draw_plot(gg_maize,x=0.05,y=0.08,height=0.4,width=0.9)+
  draw_plot(legend,x=0.0,y=0.05,height=1,width=1)+
  draw_label(
    "Close the gap",x=0.5,y=0.93,size=80,vjust=0,
    fontfamily=head_ft, lineheight = 0.3,fontface = 'bold',
    hjust=0.5,color="#061623"
  )+
  draw_label(
    "The yield gap is the difference between the yield achieved and maximum yield.\n
    Here, for each year, the yield gap was calculated as the difference between the yield\n
    of the best performing country and each country's yield, and expressed as a percentage\n
    of the maximum yield. The maps below show the averages for the period 2010 to 2020.",
    x=0.5,y=0.905,size=40,vjust=1,
    fontfamily="barlow", lineheight = 0.15,
    hjust=0.5
  )+
  draw_label(
    "Data: FAO | Map: @BjnNowak",x=0.5,y=0.80,size=32,vjust=1,hjust=0.5,
    fontfamily='barlow', lineheight = 0.3,angle=0,color="dimgrey"
  )+
  draw_label(
    "Yield gap:",x=0.22,y=0.1,size=32,vjust=0.5,hjust=0,
    fontfamily='barlow', lineheight = 0.3,fontface='bold'
  )+
  draw_label(
    "Difference between maximum yield and each country's yield",x=0.31,y=0.1,size=32,vjust=0.5,hjust=0,
    fontfamily='barlow', lineheight = 0.3
  )+
  draw_label(
    "20%",x=0.32,y=0.04,size=32,vjust=0.5,hjust=0.5,
    fontfamily='barlow', lineheight = 0.3
  )+
  draw_label(
    "40%",x=0.44,y=0.04,size=32,vjust=0.5,hjust=0.5,
    fontfamily='barlow', lineheight = 0.3
  )+
  draw_label(
    "60%",x=0.57,y=0.04,size=32,vjust=0.5,hjust=0.5,
    fontfamily='barlow', lineheight = 0.3
  )+
  draw_label(
    "80%",x=0.69,y=0.04,size=32,vjust=0.5,hjust=0.5,
    fontfamily='barlow', lineheight = 0.3
  )



