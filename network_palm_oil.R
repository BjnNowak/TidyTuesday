library(tidyverse)
library(ggraph)
library(tidygraph)
library(igraph)
library(camcorder)
library(showtext)
library(ggtext)
library(scico)
library(MetBrewer)


# Set fonts
font_add_google("Anton","anton")
font_add_google("Londrina Solid","lon")
font_add_google("Quicksand","quick")
font_add_google("Open Sans","open")
font_add_google("Raleway","ral")
font_add_google("Staatliches","sta")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

data<-read_delim('Data/trade/palm_oil.csv')
data<-read_delim('Data/trade/cocoa_beans.csv')
data<-read_delim('Data/trade/vanilla.csv')
data<-read_delim('Data/trade/tomatoes.csv')
data<-read_delim('Data/trade/tea_leaves.csv')
data<-read_delim('Data/trade/wine.csv')
data<-read_delim('Data/trade/almond.csv')
data<-read_delim('Data/trade/coffee.csv')
data<-read_delim('Data/trade/kiwi.csv')

clean<-data%>%
  rename(
    from="Reporter Countries",
    to="Partner Countries",
    n=Value
  )%>%
  mutate(from=case_when(
    from=="China, mainland"~"China",
    from=="Netherlands (Kingdom of the)"~"Netherlands",
    from=="United Kingdom of Great Britain and Northern Ireland"~"UK",
    from=="United Republic of Tanzania"~"Tanzania",
    from=="United States of America"~"USA",
    from=="Venezuela (Bolivarian Republic of)"~"Venezuela",
    TRUE~from
  ))%>%
  mutate(to=case_when(
    to=="China, mainland"~"China",
    to=="Netherlands (Kingdom of the)"~"Netherlands",
    to=="United Kingdom of Great Britain and Northern Ireland"~"UK",
    to=="United Republic of Tanzania"~"Tanzania",
    to=="United States of America"~"USA",
    to=="Venezuela (Bolivarian Republic of)"~"Venezuela",
    TRUE~to
  ))%>%
  select(from,to,n)

sum(clean$n)/1000


tot<-clean%>%
  group_by(from)%>%
  summarize(tot=sum(n))%>%
  ungroup()

network<-as_tbl_graph(clean, directed=FALSE)%>%
  activate(nodes)%>%
  left_join(tot,by=join_by(name==from))%>% 
  mutate(tot = replace_na(tot,1))
  
#'star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
#'randomly', 'fr', 'kk', 'drl', 'lgl'

ggraph(
    graph=network,
    #layout = "stress"
    layout="grid"
    #layout = "circle"
    #layout = "graphopt"
  ) +
  #geom_node_point() +
  geom_edge_link(
    aes(width=n, alpha=n),
    color = "#A6808C"
  )+
  geom_node_text(
    aes(label=name, size=tot, alpha=tot),
    #aes(label = from), 
    family="ral",color="#D6CFCB",
    show.legend = FALSE, angle=45
  )+
  guides(edge_width='none',edge_alpha='none')+
  scale_edge_alpha(range=c(0.075,0.3))+
  scale_edge_width(range=c(0.1,5))+
  scale_size(range=c(3,12))+
  scale_alpha(range=c(0.3,1))+
  coord_fixed(clip='off')+
  theme_void()+
  theme(
    plot.margin = margin(1,1,1,1,"cm"),
    #plot.background = element_rect(fill="#222725",color=NA)
  )
  
