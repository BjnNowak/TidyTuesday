library(tidyverse)
library(tidygraph)
library(ggraph)
library(camcorder)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21, 
  height = 21, 
  units = "cm", 
  dpi = 300 
)

data <- read_delim('Data/Trade/soybean_cake_export_2020.csv',delim=",")%>%
  dplyr::rename(
    # New name = Old name
    domain_code = 'Domain Code',
    reporter_country_code_M49 = 'Reporter Country Code (M49)',
    reporter_country = 'Reporter Countries',
    partner_country_code_M49 = 'Partner Country Code (M49)',
    partner_country = 'Partner Countries',
    element_code = 'Element Code',
    element = 'Element',
    item_code = 'Item Code (CPC)',
    item = 'Item',
    year_code = 'Year Code',
    flag_description = 'Flag Description',
  )%>%
  mutate(reporter_country_code_M49 = str_remove(reporter_country_code_M49, "^0+"))%>%
  mutate(partner_country_code_M49 = str_remove(partner_country_code_M49, "^0+"))

mat<-read_delim('Data/Trade/mat_change.csv',delim=";")%>%
  dplyr::rename(
    # New name = Old name
    global_code = 'Global Code',
    global_name = 'Global Name',
    region_code = 'Region Code',
    region_name = 'Region Name',
    subregion_code = 'Sub-region Code',
    subregion_name = 'Sub-region Name',
    inter_region_code = 'Intermediate Region Code',
    inter_region_name = 'Intermediate Region Name',
    area = "Country or Area",
    M49_code = "M49 Code",
    iso_alpha2_code = "ISO-alpha2 Code",
    iso_alpha3_code = "ISO-alpha3 Code"
  )%>%
  mutate(M49_code = as.character(M49_code))%>%
  mutate(region_pers_name = case_when(
    (subregion_name=="Northern America")|(subregion_name=="Latin America and the Caribbean")~subregion_name,
      TRUE ~ region_name
  ))%>%
  select(M49_code,region_pers_name,area)%>%
  bind_rows(tibble(M49_code="158",region_pers_name="Asia",area="Taiwan"))


clean<-data%>%
  select(reporter_country,partner_country,Value)

edges_list <- 
  data%>%
  filter(Value>3000)%>%
  rename(
    from=reporter_country_code_M49,
    to=partner_country_code_M49,
    n=Value
  )%>%
  
  # Select columns of interest
  dplyr::select(from,to,n)


network <- as_tbl_graph(edges_list, directed = TRUE)

network<-network%>%
  activate(nodes)%>%
  left_join(mat,by,by=c("name"="M49_code"))

network<-network%>%
  activate(edges)%>%
  mutate(n_cl=case_when(
    n<10000~1,
    n<100000~2,
    n<200000~3,
    TRUE~4
  ))

cust_arrows <- arrow(
  type='closed',
  length = unit(2, 'mm')
)

pal_new <- c(
  "Africa" = "#FFC43D", "Asia" = "#F0426B", "Europe" = "#5A4FCF",
  "Latin America and the Caribbean" = "#06D6A0", "Northern America" = "#059E75",
  "Oceania" = "#F68EA6",
  'Z'=alpha('black',0)
)

net<-ggraph(
  graph= network, 
  layout = "kk"
  ) +
  geom_edge_fan(
    aes(width=as.factor(n_cl),alpha=as.factor(n_cl)),
    alpha=0.25
  )+
  geom_node_point(
    aes(color=region_pers_name)
  )+
  geom_node_text(aes(label=area),size=3)+
  scale_color_manual(values=pal_new)+
  scale_edge_width_manual(
    values = c(0.1,0.5,1,2.5)
  )+
  scale_alpha_manual(
    values=c(0.1,0.2,0.25,0.3)
  )+
  guides(color="none")+
  theme_void()

net

igraph_layouts <- c(
  'star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
  'randomly', 'fr', 'kk', 'drl', 'lgl')

for(i in 1:length(igraph_layouts)){

  
  
net<-ggraph(
  graph= network, 
  layout = igraph_layouts[i],
) +
  geom_edge_fan(
    aes(width=as.factor(n_cl),alpha=as.factor(n_cl)),
    alpha=0.25
  )+
  geom_node_point(
    aes(color=region_pers_name)
  )+
  scale_color_manual(values=pal_new)+
  scale_edge_width_manual(
    values = c(0.1,0.5,1,3)
  )+
  scale_alpha_manual(
    values=c(0.1,0.2,0.3,0.4)
  )+
  guides(color="none",alpha="none",edge_width="none")+
  theme_void()+
  theme(plot.background = element_rect(fill="white",color=NA))

print(net)

}

# Make animation
gg_playback(
  name = file.path(tempdir(), "recording", "soybean.gif"),
  first_image_duration = 1,
  last_image_duration = 1,
  frame_duration = 2,
  image_resize = 1200
)
