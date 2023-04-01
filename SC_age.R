library(tidyverse)
library(showtext)
library(ggtext)
library(camcorder)
library(patchwork)

# Set fonts

# Set fonts
font_add_google("Adamina","adam")
font_add_google("Fira Sans","fira")
font_add_google("Jost","jo")
font_add_google("Francois One","fran")
font_add_google("Raleway","ral")
font_add_google("Fira Sans","fira")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 32, 
  height = 16.6, 
  units = "cm", 
  dpi = 300 
)


data <- read_delim('Data/Chefs/chefs.csv',delim=";")


france_2020 <- data%>%
  filter(Region=="France"&Annee==2020)

france_2010 <- data%>%
  filter(Region=="France"&Annee==2010)

france_2020_total <- france_2020%>%
  filter(Age=="Total")%>%
  summarize(total=sum(Valeur))%>%
  pull(total)

france_2010_total <- france_2010%>%
  filter(Age=="Total")%>%
  summarize(total=sum(Valeur))%>%
  pull(total)

france_2020_per <- france_2020%>%
  filter(Age!="Total")%>%
  mutate(per=Valeur/france_2020_total*100)%>%
  group_by(Sexe)%>%
  mutate(nb = row_number())%>%
  mutate(lab_eng=case_when(
    Age=="Moins de 25 ans"~"Less than 25 yo",
    Age=="25 à 29 ans"~"25 to 29 yo",
    Age=="30 à 34 ans"~"30 to 34 yo",
    Age=="35 à 39 ans"~"35 to 39 yo",
    Age=="40 à 44 ans"~"40 to 44 yo",
    Age=="45 à 49 ans"~"45 to 49 yo",
    Age=="50 à 54 ans"~"50 to 54 yo",
    Age=="55 à 59 ans"~"55 to 59 yo",
    Age=="60 à 64 ans"~"60 to 64 yo",
    Age=="65 à 69 ans"~"65 to 69 yo",
    Age=="70 à 74 ans"~"70 to 74 yo",
    Age=="75 à 79 ans"~"75 to 79 yo",
    Age=="80 ans et plus"~"More than 80 yo"
    
  ))

france_2010_per <- france_2010%>%
  filter(Age!="Total")%>%
  mutate(per_2010=Valeur/france_2010_total*100)%>%
  group_by(Sexe)%>%
  mutate(nb = row_number())%>%
  ungroup()%>%
  select(Age,Sexe,per_2010,nb)

comp <- france_2020_per%>%
  left_join(france_2010_per)

data_2020 <- data%>%
  filter(Region!="France"&Annee!=2010&Age!="Total")%>%
  group_by(Region)%>%
  mutate(tot=sum(Valeur))%>%
  ungroup()%>%
  group_by(Region,Sexe)%>%
  mutate(nb = row_number())%>%
  ungroup()%>%
  mutate(per=Valeur/tot*100)


alpha_bar <- 0.2
alpha_line <- 0.6
alpha_point <- 0.3

col_femme <- "#4a9e48"
col_homme <- "#2b4584"
col_lab <- "#2D232E"
col_arrow <- "#26547C"


ggplot()+
  geom_rect(
    data=france_2020_per%>%filter(Sexe=="Homme"),
    aes(ymin=0,ymax=per,xmin=nb-0.4,xmax=nb+0.4),
    fill=col_homme,alpha=alpha_bar
  )+
  geom_rect(
    data=france_2020_per%>%filter(Sexe=="Femme"),
    aes(ymin=0,ymax=-per,xmin=nb-0.4,xmax=nb+0.4),
    fill=col_femme,alpha=alpha_bar
  )+
  
  geom_smooth(
    data=france_2020_per%>%filter(Sexe=="Homme"),
    aes(y=per,x=nb),
    color=alpha(col_homme,0.2),alpha=alpha_bar,
    se=FALSE
  )+
  geom_smooth(
    data=france_2010_per%>%filter(Sexe=="Homme"),
    aes(y=per_2010,x=nb),
    color=alpha(col_homme,0.2),alpha=alpha_bar,lty="dashed",
    se=FALSE
  )+
  geom_smooth(
    data=france_2020_per%>%filter(Sexe=="Femme"),
    mapping=aes(x = nb, y = -per),
    color=alpha(col_femme,0.2),
    se=FALSE
  )+
  geom_smooth(
    data=france_2010_per%>%filter(Sexe=="Femme"),
    aes(y=-per_2010,x=nb),
    color=alpha(col_femme,0.2),alpha=alpha_bar,lty="dashed",
    se=FALSE
  )+
  
  geom_segment(
    data=comp%>%filter(Sexe=="Femme"),
    aes(y=-per_2010,yend=-per_2010,x=nb-0.4,xend=nb+0.4),
    size=1,
    color=col_femme,alpha=alpha_line
  )+
  geom_segment(
    data=comp%>%filter(Sexe=="Homme"),
    aes(y=per_2010,yend=per_2010,x=nb-0.4,xend=nb+0.4),
    size=1,
    color=col_homme,alpha=alpha_line
  )+
  
  geom_segment(
    data=comp%>%filter(Sexe=="Homme"&Age=="60 à 64 ans"),
    aes(y=per_2010,yend=per,x=nb,xend=nb), 
    arrow = arrow(),
    size=2,
    color=col_arrow,alpha=1
  )+
  
  geom_jitter(
    data=data_2020%>%filter(Sexe=="Homme"),
    aes(y=per,x=nb),
    width=0.3,height=0,
    size=2,
    color=col_homme,alpha=alpha_point
  )+
  geom_jitter(
    data=data_2020%>%filter(Sexe=="Femme"),
    aes(y=-per,x=nb),
    width=0.3,height=0,
    size=2,
    color=col_femme,alpha=alpha_point
  )+
  
  geom_text(
    data=france_2020_per%>%filter(Sexe=="Homme"),
    aes(y=0,x=nb,label=lab_eng),
    hjust=0.5,vjust=0.5,
    size=13,family="jo",color=col_lab
  )+
  
  annotate(
    "text",
    y=-5,x=13.6,label="5%",
    family="fira",size=15,color=col_lab
  )+
  annotate(
    "text",
    y=5,x=13.6,label="5%",
    family="fira",size=15,color=col_lab
  )+
  annotate(
    "text",
    y=10,x=13.6,label="10%",
    family="fira",size=15,color=col_lab
  )+
  annotate(
    "text",
    y=15,x=13.6,label="15%",
    family="fira",size=15,color=col_lab
  )+
  
  annotate(
    "text",
    y=0,x=14,label="Percentage of farmers",
    family="jo",size=15,color=col_lab,fontface="bold"
  )+
  
  annotate(
    "text",
    y=-4,x=12,label="Women",
    family="ral",size=20,
    #alpha=0.5,
    color=col_femme,fontface="bold",
    hjust=0.5
  )+
  annotate(
    "text",
    y=5,x=12,label="Men",
    family="ral",size=20,
    #alpha=0.5,
    color=col_homme,fontface="bold",
    hjust=0.5
  )+
  
  coord_flip()+
  theme_void()
