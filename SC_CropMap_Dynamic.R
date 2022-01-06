
# Clear space 
rm(list=ls())
gc()

# Load libraries
library(tidyverse)
library(camcorder)
library(ggtext)
library(showtext)
library(maps)
library(sf)
library(ggiraph)
library(cowplot)

# Load data
data <- readr::read_csv('https://raw.githubusercontent.com/BjnNowak/CropMap/main/Data/all_crops_area.csv')

# First
first <- data%>%
  filter(Year==2018)%>%
  filter(Element=='Area harvested')%>%
  filter(Unit=='ha')%>%
  group_by(Area)%>%
  slice_max(Value)


second <- data%>%
  filter(Year==2018)%>%
  filter(Element=='Area harvested')%>%
  filter(Unit=='ha')%>%
  arrange(-Value)%>%
  group_by(Area)%>%
  slice(1:2)%>%
  slice_min(Value)%>%
  select(
    Area,crop2=Item,surf2=Value
  )

third <- data%>%
  filter(Year==2018)%>%
  filter(Element=='Area harvested')%>%
  filter(Unit=='ha')%>%
  arrange(-Value)%>%
  group_by(Area)%>%
  slice(1:3)%>%
  slice_min(Value)%>%
  select(
    Area,crop3=Item,surf3=Value
  )


clean<-first %>%
  left_join(second)%>%
  left_join(third)

clean_names<-clean%>%
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
  ))


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
  ))




test <- states%>%
  left_join(clean_names)%>%
  select(Area,Item,Value,crop2,surf2,crop3,surf3)%>%
  mutate(Crop=case_when(
    Item %in% c('Maize','Soybeans','Wheat','Millet','Barley','Rice, paddy')~Item,
    TRUE~'Other'
  ))%>%
  mutate(
    Surface=round(Value/1000000,2),
    Surface2=round(surf2/1000000,2),
    Surface3=round(surf3/1000000,2)
  )%>%
  mutate(lab=paste0(Area,"\n",Item))%>%
  mutate(lab2=glue::glue(
    '<span style="font-weight: 900;">{Area}</span>
    <br>
    1. {Item} ({Surface} Mha)</body>
    <br>
    2. {crop2} ({Surface2} Mha)</body>
    <br>
    3. {crop3} ({Surface3} Mha)</body>
    '
  ))

pal <- c(
  'Maize' = '#419D78',
  'Wheat' = '#FFE066',
  'Millet'= '#A15856',
  'Barley' = '#F9A061',
  'Soybeans' = '#70C1B3',
  'Rice, paddy' = '#264653',
  'Other' = '#d4a373'
)



tooltip_css <- "background-color:gray;color:white;padding:10px;border-radius:5px;text-align:center;"

test_fixed <- test %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=270"))



gg<-ggplot(data=test_fixed)+
  geom_sf_interactive(aes(fill=Crop,tooltip=lab2),size=0.1)+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  coord_sf(crs = "ESRI:54030")+
  cowplot::theme_minimal_grid()+
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(hjust = 0.5,family = 'marker',size=50),
    plot.subtitle = element_markdown(size=20,family='open',lineheight =0.5),
    plot.caption = element_markdown(size=20,family='open'),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(color="grey80",size=0.1)
  )

x <- girafe(
  ggobj = gg,
  options = list(
    opts_tooltip(css = tooltip_css),
    opts_sizing(width = 1) 
  )
)

print(x)

