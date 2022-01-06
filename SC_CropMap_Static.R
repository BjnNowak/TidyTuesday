
# Clear space 
rm(list=ls())
gc()

library(tidyverse)
library(camcorder)
library(ggtext)
library(showtext)
library(maps)
library(sf)
library(ggiraph)
library(cowplot)

# Fonts
font_add_google("Permanent Marker","marker")
font_add_google("Open Sans","open")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10*1.618, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

# Load data
data <- readr::read_csv('https://raw.githubusercontent.com/BjnNowak/CropMap/main/Data/all_crops_area.csv')

# Get first crop per country
clean <- data%>%
  filter(Year==2018)%>%
  filter(Element=='Area harvested')%>%
  filter(Unit=='ha')%>%
  group_by(Area)%>%
  slice_max(Value)

# Clean names before merging with world map
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


# Load and clean world map
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

# Merge data
test <- states%>%
  left_join(clean_names)%>%
  select(Area,Item,Value)%>%
  mutate(Crop=case_when(
    Item %in% c('Maize','Soybeans','Wheat','Millet','Barley','Rice, paddy')~Item,
    TRUE~'Other'
  ))%>%
  mutate(Surface=round(Value/1000000,1))%>%
  mutate(lab=paste0(Area,"\n",Item))%>%
  mutate(lab2=glue::glue(
    '<span style="font-weight: 900;">{Area}</span>
    <br>
    {Item} ({Surface} Mha)</body>'
  ))

# Small fix for Robinson projection
test_fixed <- test %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=270"))


# Set color palette
pal <- c(
  'Maize' = '#419D78',
  'Wheat' = '#FFE066',
  'Millet'= '#A15856',
  'Barley' = '#F9A061',
  'Soybeans' = '#70C1B3',
  'Rice, paddy' = '#264653',
  'Other' = '#d4a373'
)

# Make plot
ggplot()+
  geom_sf(data=test_fixed,aes(fill=Crop),size=0.1)+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  coord_sf(crs = "ESRI:54030")+
  cowplot::theme_minimal_grid()+
  labs(
    title="Who grows what?",
    subtitle=
      "This map shows the most cultivated crop for each country (by harvested area). The most common crops are mainly cereals (<span style='color:#FFE066;'>**wheat**</span>, <span style='color:#419D78'>**maize**</span>, <span style='color:#264653'>**rice**</span>,<br>
      <span style='color:#F9A061;'>**barley**</span> and <span style='color:#A15856;'>**millet**</span>), with also a strong share of <span style='color:#70C1B3;'>**soybeans**</span> in the Americas. Beyond these main crops, some countries are specialized in <span style='color:#d4a373;'>**other crops**</span><br>
      (such as coffee or oil palm).",
    caption="**Data:** FAO (2018) | **Plot:** @BjnNowak"
  )+
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(hjust = 0.5,family = 'marker',size=50),
    plot.subtitle = element_markdown(size=20,family='open',lineheight =0.5),
    plot.caption = element_markdown(size=20,family='open'),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(color="grey80",size=0.1)
  )
