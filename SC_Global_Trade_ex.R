library(tidyverse)
library(tidygraph)
library(ggraph)
library(camcorder)
library(showtext)

# Set fonts
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

# Set color palette
pal_new <- c(
  "Africa" = "#E9C46A", "Asia" = "#E76F51", "Europe" = "#2A9D8F",
  "Latin America and the Caribbean" = "#20A4F3", "Northern America" = "#264653",
  "Oceania" = "#9C7A97"
)

data<-read_csv('Data/Trade/export_values_2020_all.csv')%>%
  mutate(reporter_country_code_M49 = str_remove(reporter_country_code_M49, "^'+"))%>%
  mutate(partner_country_code_M49 = str_remove(partner_country_code_M49, "^'+"))%>%
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

rpt<-mat%>%
  select(reporter_country_code_M49=M49_code,reporter_reg=region_pers_name)
prt<-mat%>%
  select(partner_country_code_M49=M49_code,partner_reg=region_pers_name)

clean<-data%>%
  filter(item%in%c("Soya beans","Cake of  soya beans","Soya bean oil"))%>%
  select(reporter_country_code_M49,partner_country_code_M49,value)%>%
  left_join(rpt)%>%
  left_join(prt)%>%
  group_by(reporter_reg,partner_reg)%>%
  summarize(n=sum(na.omit(value)))%>%
  ungroup()%>%
  drop_na()%>%
  select(from=reporter_reg,to=partner_reg,n)%>%
  as_tbl_graph(directed = TRUE)%>%
  activate(edges)%>%
  left_join(vec_count)%>%
  mutate(n_cl=case_when(
    n<1000000~1,
    n<3000000~2,
    n<5000000~3,
    n<7000000~4,
    TRUE~5
  ))




net<-ggraph(graph= clean, layout = "circle")+
  geom_edge_fan(aes(width=as.factor(n_cl),color=name),alpha=0.75)+
  geom_node_point(aes(fill=name),color="white",size=7,pch=21)+
  scale_fill_manual(values=pal_new)+
  scale_edge_colour_manual(values=pal_new)+
  scale_edge_width_manual(values = c(0.5,3,5,7,9))+
  guides(color="none",alpha="none",edge_width="none",fill="none",edge_color="none")+
  annotate(geom="text",x=0,y=0,label="Cheese",size=50,alpha=0.25,family="sta",lineheight=0.25)+
  theme_void()

net

