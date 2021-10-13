
# Clear space 
rm(list=ls())
gc()

library(tidyverse)
library(camcorder)
library(ggtext)
library(showtext)

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10*1.618, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

# Load fonts
font_add_google("Oswald","oswald")
font_add_google("Playfair Display","play")
font_add_google("Roboto Condensed","roboto")

# Fonnt for axis
ax<-'oswald'

# Automatically use {showtext} for plots
showtext_auto()

production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')

clean<-production%>%
  select(
    Country=Entity, Code, Year,
    Fish = 'Commodity Balances - Livestock and Fish Primary Equivalent - Freshwater Fish - 2761 - Production - 5510 - tonnes'
  )%>%
  drop_na()

test <- clean%>%
  filter(Year==2000)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))


top5 <- clean%>%
  filter(Country %!in% c('World','USSR'))%>%
  group_by(Country)%>%
  summarize(Total=sum(Fish))%>%
  arrange(-Total)%>%
  head(2)%>%
  pull(Country)
# (Finally only chose top2 countries to highlight difference between 
# China and India)

fished <- clean%>%
  filter(Country != 'World')%>%
  mutate(Cat = case_when(
   Country %in% top5 ~ Country,
   TRUE ~ 'Other'
  ))%>%
  group_by(Cat,Year)%>%
  summarize(Tot=sum(Fish))%>%
  ungroup()

fished%>%
  filter(Year=='2013')

#fished$Cat <- fct_relevel(fished$Cat,"China","India","Bangladesh","Indonesia","Vietnam")
fished$Cat <- fct_relevel(fished$Cat,"China","India")

Min <- fished%>%filter(Year==2013)%>%summarize(Final=-sum(Tot))%>%select(Final)

world <- clean%>%
  filter(Country == 'World')%>%
  mutate(Tot=Fish)

# Units: thousand-tonnes

# Color
water <- "#2a9d8f"
pal<-c("#EB886F","#E24D28","#A23216")

# Ax size
ax_size <- 12

# Labels
tit <- tibble(
  label="<span style='color:white'><b>Worlwide</b></span>, captures significantly increased since 1980.
  In 2013, <br><span style='color:#A23216'><b>China</b></span> fished **25 billions of tons**, far more than <span style='color:#E24D28'><b>India</b></span>,
  the second<br>biggest fishing country, and more than the rest of <span style='color:#EB886F'><b>the other countries.</b></span>",
  Year=1962,
  Tot=3.5e7)

source<-tibble(
  label="**Source:** Our World in Data | **Plot:** @BjnNowak",
  Year=1962,
  Tot=5e7)

# Plot
fished%>%
  ggplot(mapping=aes(x=Year,y=-Tot))+
  annotate(
    geom="rect",
    xmin=1961,xmax=2013,ymax=0,
    ymin=Min$Final-1000000,
    fill=water,alpha=0.65)+

  geom_area(mapping=aes(fill=Cat))+
  geom_line(data=world,size=2,color='white')+
  
  annotate(
    geom="text",
    x=1970,y=-2000000,
    label="1970",color='black',size=ax_size,family=ax)+
  annotate(
    geom="text",
    x=1990,y=-2000000,
    label="1990",color='black',size=ax_size,family=ax)+
  annotate(
    geom="text",
    x=2010,y=-2000000,
    label="2010",color='black',size=ax_size,family=ax)+
  
  annotate(
    geom="text",
    x=2012.5,y=-15e6,hjust=1,
    label="15Gt",color='black',size=ax_size,family=ax)+
  annotate(
    geom="text",
    x=2012.5,y=-30e6,hjust=1,
    label="30Gt",color='black',size=ax_size,family=ax)+
  
  annotate(
    geom='text',
    x=1962,y=-30.e6,hjust=0,family='play',
    size=20,fontface='bold',
    label='Freshwater fishes captures'
  )+
  
  geom_richtext(
    tit,mapping=aes(label=label),
    hjust=0,vjust=1,family='roboto',size=14,
    lineheight=0.5,
    fill = NA, label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    source,mapping=aes(label=label),
    hjust=0,vjust=1,family='roboto',size=10,
    lineheight=0.5,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  scale_fill_manual(values=rev(pal))+
  guides(
    fill='none'
  )+
  theme_void()
  

