library(tidyverse)
library(ggtext)
library(showtext)
library(camcorder)

# Set fonts
font_add_google("Jost","jo")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/cran_20230905.csv')

clean<-data%>%
  select(from=Imports,to=Package)%>%
  mutate(from = strsplit(from, ","))%>%
  unnest(from)%>%
  # Suppress version number
  mutate(from=gsub("\\s*\\([^\\)]+\\)","",from))%>%
  # Suppress whitespace
  mutate(from=str_replace_all(from, fixed(" "), ""))%>%
  mutate(n=1)%>%
  drop_na()

# extract top packages
top<-clean%>%
  group_by(from)%>%
  summarize(sm=sum(n))%>%
  ungroup()%>%
  arrange(-sm)%>%
  head(25)%>%
  mutate(
    rk=row_number(),
    nm=glue::glue("<b>{rk}.</b> {from}<br><span style='color:grey80'>{sm} times</span>")
  )%>%
  mutate(x=rep(c(1,2,3,4,5),5))%>%
  mutate(y=c(rep(5,5),rep(4,5),rep(3,5),rep(2,5),rep(1,5)))

drk<-"#0f4c5c"
bbl<-"#9e2a2b"
txt<-"#fff3b0"

cap<-tibble(cap="**Data** David Schoch **| Plot** Benjamin Nowak")

ggplot(top,aes(x=x,y=y))+
  geom_point(aes(size=sm),alpha=1,color=bbl)+
  geom_richtext(
    aes(label=nm),
    size=12,family="ral",lineheight=0.45,color=txt,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  annotate(
    "text",label="R packages most cited as dependencies on CRAN",
    y=6,x=0.5,family="jo",size=17,color=txt,hjust=0,
    fontface="bold"
  )+
  geom_richtext(
    data=cap,inherit.aes = FALSE,
    aes(label=cap,x=0.525,y=5.75),
    size=10,family="fira",lineheight=0.45,color=txt,hjust=0,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  guides(size='none')+
  scale_size(range=c(5,42))+
  scale_x_continuous(limits=c(0.25,5.75))+
  scale_y_continuous(limits=c(0.5,6.25))+
  #labs(title="R package most cited as dependence on CRAN")+
  theme_void()+
  theme(plot.background = element_rect(fill=drk,color=NA))
