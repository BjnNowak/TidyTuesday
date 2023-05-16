library(tidyverse)
library(showtext)
library(ggtext)
library(camcorder)

# Set fonts
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Fira Sans","fira")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 29.7, 
  height = 21, 
  units = "cm", 
  dpi = 300 
)

price <- read_delim('Data/Seed_price/seed_prices.csv',delim=";")
pmg <- read_delim('Data/Seed_price/seed_pmg.csv',delim=";")  

clean<-price%>%
  left_join(pmg)%>%
  mutate(
    price_per_seed = Prix_dose_euros/Nb_graine,
    seed_for_1kg = (1000*1000)/pmg_g_per_1000,
    price_per_kg = seed_for_1kg * price_per_seed
  )

res<-clean%>%
  group_by(Espece_eng)%>%
  summarise(
    mn=mean(price_per_kg),
    sd=sd(price_per_kg)
  )%>%
  ungroup()%>%
  mutate(
    mn_round=round(mn),
    label=glue::glue("**{Espece_eng} seeds**<br>{mn_round} € per kg")
  )

metals<-tibble(
  metal=c("Silver","Platinum","Palladium","Gold"),
  x=c(500,30000,45000,60000)
)

pal<-c(
  "Silver"="#8e9aaf",
  "Platinum"="#8e9aaf",
  "Palladium"="#8e9aaf",
  "Gold"="#f5cb5c"
)

col_crop <- "#2a9d8f"

ggplot(res,aes(y=fct_reorder(Espece_eng,mn),x=mn))+
  annotate(
    geom="segment",
    x=0,xend=0,y=-Inf,yend=Inf,lwd=1,
  )+
  geom_segment(
    data=metals%>%filter(metal!="Silver"),
    mapping=aes(x=x,xend=x,y=-Inf,yend=Inf,color=metal),
    lty="dashed",
    inherit.aes=FALSE
  )+
  geom_segment(
    aes(
      y=fct_reorder(Espece_eng,mn),yend=fct_reorder(Espece_eng,mn),
      x=0,xend=mn),
    color=col_crop
  )+
  ggtext::geom_richtext(
    aes(x=mn+2000,y=fct_reorder(Espece_eng,mn),label=label),
    hjust=0,size=20,lineheight=0.35,family="fira",
    color=col_crop,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_point(color=col_crop,size=3)+
  geom_jitter(
    data=clean,
    mapping=aes(y=Espece_eng,x=price_per_kg),
    alpha=0.25,size=2.5,
    color=col_crop
  )+
  scale_color_manual(values=pal)+
  guides(color="none")+
  theme_void()

