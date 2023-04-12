library(tidyverse)
library(showtext)
library(ggtext)
library(camcorder)
library(patchwork)
library(tidygraph)
library(ggraph)

# Set fonts

font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Fira Sans","fira")
font_add_google("Jost","jo")

showtext_auto()

# Plot size
# A4
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21, 
  height = 21, 
  units = "cm", 
  dpi = 300 
)

# full report :
# https://www.anses.fr/fr/system/files/LABORATOIRE2022AST0255Ra.pdf
# summary : 
# https://www.anses.fr/fr/content/polluants-emergents-dans-leau-potable-le-point-sur-les-principaux-resultats-de-la-derniere-2

# Dl data
data <- read_delim('Data/Anses/network.csv',delim=";")
node <- read_delim('Data/Anses/names.csv',delim=";")%>%
  mutate(prelev=case_when(
    freq_nb_total_per>0~100,
    TRUE~0
  ))%>%
  mutate(nms=case_when(
    short_name_eng=="Pesticide"~"",
    short_name_eng=="Fungicide"~"",
    short_name_eng=="Weedkiller"~"",
    short_name_eng=="Atrazine desethyl deisopropyl"~"Atrazine\ndesethyl\ndeisopropyl",
    short_name_eng=="Atrazine desethyl"~"Atrazine\ndesethyl",
    short_name_eng=="Chloridazone-desphenyl"~"Chloridazone\ndesphenyl",
    short_name_eng=="Chloridazone-methyl-desphenyl"~"Chloridazone\nmethyl\ndesphenyl",
    short_name_eng=="Terbumeton desethyl"~"Terbumeton\ndesethyl",
    short_name_eng=="Terbuthylazine desethyl"~"Terbuthylazine\ndesethyl",
    short_name_eng=="Dimethachlor OXA"~"Dimethachlor\nOXA",
    TRUE~short_name_eng
  ))%>%
  mutate(nms_sa_auth=case_when(
    Type=="SA"&Authorized=="A"~short_name_eng,
    TRUE~NA
  ))%>%
  mutate(nms_sa_nonauth=case_when(
    Type=="SA"&Authorized=="NAN"~short_name_eng,
    TRUE~NA
  ))%>%
  mutate(nms_met_pert=case_when(
    Type=="Metabolite"&pertinence=="P"~nms,
    TRUE~NA
  ))%>%
  mutate(circ=case_when(
    Type=="Metabolite"&pertinence=="P"~"circle",
    TRUE~"no_circle"
  ))%>%
  mutate(nms_met_ne=case_when(
    Type=="Metabolite"&pertinence=="NE"~nms,
    TRUE~NA
  ))

# Make network
graph <- as_tbl_graph(data, directed = TRUE)

# Add characteristics to nodes
graph<-graph%>%
  activate(nodes)%>%
  left_join(node,by=c("name"="short_name_eng"))


col_whole <- "#45CAFF"
col_above_thresh <- "#FF1B6B"
col_below_thresh <- "#FFADCA"
col_non_auth <- "#FF9000"
col_auth <- "#395B50"
col_auth <- "white"
col_pert <- "white"
col_circle <- "#FF1B6B"
#col_above_thresh <- "#A273B5"

pal <- c(
  "circle"=col_above_thresh,
  "no_circle"=alpha(col_whole,0)
)

ggraph(
  graph, 
  layout="dendrogram",
  circular=TRUE,
  height=-height
  
)+
  #geom_edge_link(
  geom_edge_elbow(color="grey30")+
  geom_node_point(
    aes(size=prelev),
    color=col_whole
  ) +
  geom_node_point(
    aes(size=freq_nb_total_per),
    color=col_below_thresh
  ) + 
  geom_node_point(
    aes(size=freq_above_threshold_per),
    color=col_above_thresh  
  ) +
  geom_node_point(
    aes(color=circ),
    pch=21,size=22
  ) +
  geom_node_text(
    aes(label=nms_sa_auth),
    size=8,family="ral",
    color=col_auth,
    #nudge_y = -0.05,
    angle=0,hjust=0.5,
    lineheight=0.25,
    fontface='bold',alpha=1
  )+
  geom_node_text(
    aes(label=nms_sa_nonauth),
    color=col_non_auth,
    size=8,family="ral",
    #nudge_y = -0.05,
    angle=0,hjust=0.5,
    lineheight=0.25,
    fontface='bold',alpha=1
  )+
  geom_node_text(
    aes(label=nms_met_pert),
    size=6,family="fira",
    #nudge_y = -0.05,
    angle=0,hjust=0.5,
    lineheight=0.25,
    fontface='bold.italic',alpha=0.75,
    color=col_pert
  )+
  geom_node_text(
    aes(label=nms_met_ne),
    size=6,family="fira",
    #nudge_y = -0.05,
    angle=0,hjust=0.5,
    lineheight=0.25,
    fontface='bold.italic',alpha=0.5,
    color=col_pert
  )+
  scale_size_continuous(
    range=c(0,20)
  )+
  scale_color_manual(values=pal)+
  guides(size='none',color='none')+
  coord_cartesian(clip = "off")+
  theme_void()+
  theme(
    #plot.background = element_rect(fill="#1D201F"),
    legend.position = 'none',
    plot.margin = margin(2,2,2,2,"cm")
  )






