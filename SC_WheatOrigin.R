library(tidyverse)
library(sf)

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-18/founder_crops.csv')
worldmap <- st_as_sf(maps::map(database="world", plot = FALSE, fill = TRUE))

# Soft wheat (="Child"), hexaploids
soft <- c("Triticum aestivum")
# Hard wheat (="Parent 1"), tetraploids
durum <- c("Triticum turgidum","Triticum dicoccum", "Triticum durum")
# Wild grass: Aegilops tauschii (="Parent 2"), diploids
wild <- c("Aegilops")

# Add column with a "type" element
# (soft wheat, hard wheat or aegilops)
clean<-data%>%
  mutate(
    type=case_when(
      str_detect(taxon_detail,paste(soft, collapse = "|"))~"Soft_wheat",
      str_detect(taxon_detail,paste(durum, collapse = "|"))~"Durum_wheat",
      str_detect(taxon_detail,paste(wild, collapse = "|"))~"Aegilops"
    )
  )%>%
  drop_na(type)

# Select all sites with one, two or three crops present
all_sites<-clean%>%pull(site_name)

# Select only sites with the three crops present
venn<-clean%>%
  select(site_name,age_start,type)%>%
  group_by(site_name,type)%>%
  filter(age_start==min(age_start))%>%
  ungroup()%>%
  distinct()%>%
  select(site_name,type)%>%
  mutate(ct=1)%>%
  pivot_wider(names_from=type,values_from=ct,values_fill=0)%>%
  mutate(cl=case_when(
    # Single class
    (Aegilops==1)&(Durum_wheat==0)&(Soft_wheat==0)~"A",
    (Aegilops==0)&(Durum_wheat==1)&(Soft_wheat==0)~"B",
    (Aegilops==0)&(Durum_wheat==0)&(Soft_wheat==1)~"C",
    # Double class
    (Aegilops==1)&(Durum_wheat==1)&(Soft_wheat==0)~"D",
    (Aegilops==0)&(Durum_wheat==1)&(Soft_wheat==1)~"E",
    (Aegilops==1)&(Durum_wheat==0)&(Soft_wheat==1)~"F",
    # Triple class
    (Aegilops==1)&(Durum_wheat==1)&(Soft_wheat==1)~"G"
  ))

res <- venn%>%
  mutate(ct=1)%>%
  group_by(cl)%>%
  summarize(sm=sum(ct))

# Select only sites with the three crops present
sel_sites<-clean%>%
  select(site_name,age_start,type)%>%
  group_by(type,site_name)%>%
  filter(age_start==min(age_start))%>%
  ungroup()%>%
  distinct()%>%
  mutate(ct=1)%>%
  group_by(site_name)%>%
  summarize(sm=sum(ct))%>%
  filter(sm==3)%>%
  pull(site_name)

# Only two sites with the three crops
sel_sites

rar<-clean%>%
  filter(site_name%in%sel_sites)



coords_all<-clean%>%
  select(site_name,latitude,longitude)%>%
  #filter(site_name%in%all_sites)%>%
  distinct()%>%
  left_join(venn)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
coords_sf <- st_as_sf(
  x = coords_all,                         
  coords = c("longitude", "latitude"),
  crs = projcrs
)

bbox_x = c(28,60)
bbox_y = c(29,41)

back <- "#1D201F"
land <- "grey20"

pal<-c(
  "A"= "#E09B33",
  "B"= "#EA3D32",
  "D"= "#E05A33",
  "E"= "#6A45A2",
  "G"= "white"
)

ggplot()+
  geom_sf(worldmap,mapping=aes(geometry=geom),fill=land,color=NA)+
  geom_sf(
    coords_sf%>%filter(cl!="G"),
    mapping=aes(geometry=geometry,color=cl),
    alpha=0.8,size=2
  )+
  geom_sf(
    coords_sf%>%filter(cl=="G"),
    mapping=aes(geometry=geometry),
    pch=21,color="black",fill="white",
    alpha=1,size=4
  )+
  geom_sf_text(
    coords_sf%>%filter(cl=="G"),
    mapping=aes(geometry=geometry,label=site_name),
    pch=21,color="black",fill="white",
    alpha=1,size=4
  )+
  guides(color="none")+
  #geom_sf(coords_sf%>%filter(site_name%in%sel_sites),mapping=aes(geometry=geometry))+
  #geom_sf(coords_sf%>%filter(site_name%in%sel_sites[1]),mapping=aes(geometry=geometry))+
  scale_x_continuous(limits=bbox_x)+
  scale_y_continuous(limits=bbox_y)+
  scale_color_manual(values=pal)+
  theme_void()+
  theme(
    plot.background = element_rect(fill=back,color=NA)
  )

ggsave(
  filename="wheat_map.png",
  width=25,
  height=15,
  unit="cm",
  dpi=320
)
