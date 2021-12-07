
# Clear space 
rm(list=ls())
gc()

library(tidyverse)
library(camcorder)
library(ggtext)
library(showtext)
library(tidygraph)
library(ggraph)
library(igraph)
library(ggtext)

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

# Load fonts
font_add_google("Oswald","oswald")
font_add_google("Playfair Display","play")
font_add_google("Roboto Condensed","roboto")

# Automatically use {showtext} for plots
showtext_auto()

set.seed(1)

spiders <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv'
  )%>%
  mutate(uniq_spec=paste(genus,species))

t<-spiders%>%
  filter(str_detect(uniq_spec,'Latrodectus geometricus'))

sp_us%>%
  filter(str_detect(uniq_spec,'Cheiracanthium inclusum'))

# https://www.britannica.com/list/9-of-the-worlds-deadliest-spiders
deadly_spiders <- c(
  'Loxosceles reclusa',      # Brown recluse
  'Phoneutria fera','Phoneutria nigriventer', # Brazilian Wandering Spiders
  'Cheiracanthium inclusum', # Yellow sac spider
  'Latrodectus mactans',     # Black widow
  'Latrodectus geometricus', # Brown widow
  'Latrodectus bishopi',     # Red widow
  'Latrodectus hasselti',    # Redback spider
  'Atrax robustus'           # Sidney funnel-web spider
)

pal <- c(
  'Latrodectus mactans'='#264653',     # Black widow
  
  
  'Phoneutria fera' = '#287271',
  'Phoneutria nigriventer'='#287271', # Brazilian Wandering Spiders
  
  'Loxosceles reclusa' = '#2A9D8F'  , # Brown recluse
  
  'Cheiracanthium inclusum'='#E9C46A', # Yellow sac spider
  
  
  
  'Latrodectus geometricus'="#EFB366", # Brown widow
  'Atrax robustus'="#F4A261", # Sidney funnel-web spider
  'Latrodectus bishopi'="#EE8959",     # Red widow
  'Latrodectus hasselti'="#E76F51",    # Redback spider
  
  'other'='black' 
)

sp_us<-spiders%>%
  filter(str_detect(distribution,"USA")|str_detect(distribution,"North America"))

sp_fr<-spiders%>%
  filter(str_detect(distribution,"France"))

sp_au<-spiders%>%
  filter(str_detect(distribution,"Australia"))

sp_ch<-spiders%>%
  filter(str_detect(distribution,"China"))

sp_br<-spiders%>%
  filter(str_detect(distribution,"Brazil")|str_detect(distribution,"South America"))

sp_fi<-spiders%>%
  filter(str_detect(distribution,"Finland"))

test<-sp_au

data_0<-test%>%
  mutate(ct=1)%>%
  mutate(origin='spider')%>%
  group_by(origin,family)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=origin,
    to=family
  )%>%
  mutate(type="family")

data_1<-test%>%
  mutate(ct=1)%>%
  group_by(family,genus)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=family,
    to=genus
  )%>%
  mutate(type="genus")

data_2<-test%>%
  mutate(ct=1)%>%
  group_by(genus,uniq_spec)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=genus,
    to=uniq_spec
  )%>%
  mutate(type="species")


data<-data_0%>%
  bind_rows(data_1)%>%
  bind_rows(data_2)

network <- as_tbl_graph(data, directed = TRUE)

network<-network%>%
  activate(nodes)%>%
  mutate(col_ind=case_when(
    name %in% deadly_spiders~name,
    TRUE~'other'
  ))%>%
  mutate(other_ind=case_when(
    name %in% deadly_spiders~'red',
    TRUE~'black'
  ))



au<-ggraph(network, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  geom_node_point(aes(color=col_ind,filter=leaf,size=other_ind,alpha=other_ind))+
  scale_size_manual(values=c(2,15))+
  scale_alpha_manual(values=c(0.05,0.8))+
  scale_color_manual(values=pal)+
  theme_graph()+
  coord_fixed()+
  #guides(color='none',size='none',alpha='none')+
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))+
  annotate(geom='text',x=0,y=1.3,label="Australia",size=30,family='oswald')

test<-sp_br

data_0<-test%>%
  mutate(ct=1)%>%
  mutate(origin='spider')%>%
  group_by(origin,family)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=origin,
    to=family
  )%>%
  mutate(type="family")

data_1<-test%>%
  mutate(ct=1)%>%
  group_by(family,genus)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=family,
    to=genus
  )%>%
  mutate(type="genus")

data_2<-test%>%
  mutate(ct=1)%>%
  group_by(genus,uniq_spec)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=genus,
    to=uniq_spec
  )%>%
  mutate(type="species")


data<-data_0%>%
  bind_rows(data_1)%>%
  bind_rows(data_2)

network <- as_tbl_graph(data, directed = TRUE)

network<-network%>%
  activate(nodes)%>%
  mutate(col_ind=case_when(
    name %in% deadly_spiders~name,
    TRUE~'other'
  ))%>%
  mutate(other_ind=case_when(
    name %in% deadly_spiders~'red',
    TRUE~'black'
  ))



br<-ggraph(network, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  geom_node_point(aes(color=col_ind,filter=leaf,size=other_ind,alpha=other_ind))+
  scale_size_manual(values=c(2,15))+
  scale_alpha_manual(values=c(0.05,0.8))+
  scale_color_manual(values=pal)+
  theme_graph()+
  coord_fixed()+
  #guides(color='none',size='none',alpha='none')+
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))+
  annotate(geom='text',x=0,y=1.3,label="Brazil",size=30,family='oswald')


test<-sp_fi

data_0<-test%>%
  mutate(ct=1)%>%
  mutate(origin='spider')%>%
  group_by(origin,family)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=origin,
    to=family
  )%>%
  mutate(type="family")

data_1<-test%>%
  mutate(ct=1)%>%
  group_by(family,genus)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=family,
    to=genus
  )%>%
  mutate(type="genus")

data_2<-test%>%
  mutate(ct=1)%>%
  group_by(genus,uniq_spec)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=genus,
    to=uniq_spec
  )%>%
  mutate(type="species")


data<-data_0%>%
  bind_rows(data_1)%>%
  bind_rows(data_2)

network <- as_tbl_graph(data, directed = TRUE)

network<-network%>%
  activate(nodes)%>%
  mutate(col_ind=case_when(
    name %in% deadly_spiders~name,
    TRUE~'other'
  ))%>%
  mutate(other_ind=case_when(
    name %in% deadly_spiders~'red',
    TRUE~'black'
  ))



fi<-ggraph(network, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  geom_node_point(aes(filter=leaf),color='black',size=2,alpha=1)+
  #scale_size_manual(values=c(2,15))+
  #scale_alpha_manual(values=c(0.05,0.8))+
  #scale_color_manual(values=pal)+
  theme_graph()+
  coord_fixed()+
  #guides(color='none',size='none',alpha='none')+
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))+
  annotate(geom='text',x=0,y=1.3,label="Finland",size=30,family='oswald')


test<-sp_us

data_0<-test%>%
  mutate(ct=1)%>%
  mutate(origin='spider')%>%
  group_by(origin,family)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=origin,
    to=family
  )%>%
  mutate(type="family")

data_1<-test%>%
  mutate(ct=1)%>%
  group_by(family,genus)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=family,
    to=genus
  )%>%
  mutate(type="genus")

data_2<-test%>%
  mutate(ct=1)%>%
  group_by(genus,uniq_spec)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=genus,
    to=uniq_spec
  )%>%
  mutate(type="species")


data<-data_0%>%
  bind_rows(data_1)%>%
  bind_rows(data_2)

network <- as_tbl_graph(data, directed = TRUE)

network<-network%>%
  activate(nodes)%>%
  mutate(col_ind=case_when(
    name %in% deadly_spiders~name,
    TRUE~'other'
  ))%>%
  mutate(other_ind=case_when(
    name %in% deadly_spiders~'red',
    TRUE~'black'
  ))



us<-ggraph(network, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  geom_node_point(aes(color=col_ind,filter=leaf,size=other_ind,alpha=other_ind))+
  scale_size_manual(values=c(2,15))+
  scale_alpha_manual(values=c(0.05,0.8))+
  scale_color_manual(values=pal)+
  theme_graph()+
  coord_fixed()+
  #guides(color='none',size='none',alpha='none')+
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))+
  annotate(geom='text',x=0,y=1.3,label="United States",size=30,family='oswald')

test<-sp_ch

data_0<-test%>%
  mutate(ct=1)%>%
  mutate(origin='spider')%>%
  group_by(origin,family)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=origin,
    to=family
  )%>%
  mutate(type="family")

data_1<-test%>%
  mutate(ct=1)%>%
  group_by(family,genus)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=family,
    to=genus
  )%>%
  mutate(type="genus")

data_2<-test%>%
  mutate(ct=1)%>%
  group_by(genus,uniq_spec)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=genus,
    to=uniq_spec
  )%>%
  mutate(type="species")


data<-data_0%>%
  bind_rows(data_1)%>%
  bind_rows(data_2)

network <- as_tbl_graph(data, directed = TRUE)

network<-network%>%
  activate(nodes)%>%
  mutate(col_ind=case_when(
    name %in% deadly_spiders~name,
    TRUE~'other'
  ))%>%
  mutate(other_ind=case_when(
    name %in% deadly_spiders~'red',
    TRUE~'black'
  ))



ch<-ggraph(network, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  geom_node_point(aes(color=col_ind,filter=leaf,size=other_ind,alpha=other_ind))+
  scale_size_manual(values=c(2,15))+
  scale_alpha_manual(values=c(0.05,0.8))+
  scale_color_manual(values=pal)+
  theme_graph()+
  coord_fixed()+
  #guides(color='none',size='none',alpha='none')+
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))+
  annotate(geom='text',x=0,y=1.3,label="China",size=30,family='oswald')


test<-sp_fr

data_0<-test%>%
  mutate(ct=1)%>%
  mutate(origin='spider')%>%
  group_by(origin,family)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=origin,
    to=family
  )%>%
  mutate(type="family")

data_1<-test%>%
  mutate(ct=1)%>%
  group_by(family,genus)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=family,
    to=genus
  )%>%
  mutate(type="genus")

data_2<-test%>%
  mutate(ct=1)%>%
  group_by(genus,uniq_spec)%>%
  summarize(n=sum(ct))%>%
  rename(
    from=genus,
    to=uniq_spec
  )%>%
  mutate(type="species")


data<-data_0%>%
  bind_rows(data_1)%>%
  bind_rows(data_2)

network <- as_tbl_graph(data, directed = TRUE)

network<-network%>%
  activate(nodes)%>%
  mutate(col_ind=case_when(
    name %in% deadly_spiders~name,
    TRUE~'other'
  ))%>%
  mutate(other_ind=case_when(
    name %in% deadly_spiders~'red',
    TRUE~'black'
  ))



fr<-ggraph(network, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  geom_node_point(aes(filter=leaf),color='black',size=2,alpha=1)+
  #scale_size_manual(values=c(2,15))+
  #scale_alpha_manual(values=c(0.05,0.8))+
  #scale_color_manual(values=pal)+
  theme_graph()+
  coord_fixed()+
  #guides(color='none',size='none',alpha='none')+
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))+
  annotate(geom='text',x=0,y=1.3,label="France (mainland)",size=30,family='oswald')

fr

library(patchwork)
library(glue)

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 60, 
  height = 40, 
  units = "cm", 
  dpi = 300 
)

au+br+ch+fr+fi+us+
  #plot_layout(design=layout)+
  plot_annotation(
    title = "Diversity of spider species by country",
    subtitle = glue(
      "Each dendrogram shows the families, genus and species of spiders present in each country. <br>
      Most venomous species are colored (<span style='color:#264653;'>**black widow**, </span><span style='color:#287271;'>**Brazilian wandering spiders**,</span><span style='color:#2A9D8F;'> **brown recluse**,</span>
      <br><span style='color:#E9C46A;'>**yellow sac spider**, </span><span style='color:#EFB366;'>**brown widow**, </span><span style='color:#F4A261;'>**Sidney funnel web spider**</span> and <span style='color:#E76F51;'>**redback spider**</span>.)"),
    caption="Data: TidyTuesday & World Spider Database | Plot: @BjnNowak",
    theme = theme(
      plot.title = element_text(size=140,family = 'play',color = "grey15", lineheight=1),
      plot.subtitle = element_markdown(size=100,family = 'roboto', lineheight=0.4,margin=margin(t = 1, r = 0, b = 1, l = 0, unit = "cm")),
      plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"),
      plot.caption.position = "panel",
      plot.caption = element_text(family='roboto',size=80)
    ))

