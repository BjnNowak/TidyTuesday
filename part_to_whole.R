library(tidyverse)
library(rnaturalearth)
library(sf)
library(camcorder)
library(showtext)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Cabin","cab")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 23.3, 
  height = 32, 
  units = "cm", 
  dpi = 300 
)

world <- ne_countries(returnclass = "sf")%>%
  st_drop_geometry()

mat<-read_delim('data/trade/mat_change.csv',delim=";")%>%
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
    iso_a3_eh = "ISO-alpha3 Code"
  )


data <- read_csv('data/wheat/wheat_prod_2010_to_2020.csv')%>%
  rename(iso_a3_eh = "Area Code (ISO3)")%>%
  group_by(iso_a3_eh)%>%
  summarize(prod=mean(Value))%>%
  ungroup()%>%
  left_join(mat)%>%
  drop_na(region_name)


continent<-data%>%
  group_by(region_name)%>%
  summarize(prod=sum(prod))%>%
  ungroup()%>%
  arrange(-prod)%>%
  mutate(
    prod_end=cumsum(prod),
    prod_start=prod_end-prod,
    rank_continent=row_number()
  )%>%
  drop_na()

continent_name <-  continent%>%
  mutate(
    xpos=prod_start+prod/2,
    #xpos=prod_start,
  # Use (id-0.5), not just id, to center label on each item
    angle=90-360*(xpos-0.5)/max(xpos)
  )%>%
  mutate(
    hjust=case_when(
      angle<=-90~1,
      TRUE~0
    )
  )%>%
  # Flip left side labels
  mutate(
    angle=case_when(
      angle<=-90~angle+180,
      TRUE~angle
    )
  )

rk_cont<-continent%>%
  select(region_name,rank_continent)

subcont<-data%>%
  group_by(subregion_name)%>%
  summarize(
    region_name=region_name[1],
    prod=sum(prod)
  )%>%
  ungroup()%>%
  left_join(rk_cont)%>%
  arrange(rank_continent,-prod)%>%
  #arrange(region_name)%>%
  mutate(
    prod_end=cumsum(prod),
    prod_start=prod_end-prod,
    rank_subcontinent=row_number()
  )%>%
  drop_na()%>%
  mutate(ct=1)%>%
  group_by(region_name)%>%
  mutate(rank=-cumsum(ct)/sum(ct))%>%
  ungroup()

subcont_name <-  subcont%>%
  mutate(
    xpos=prod_start+prod/2,
    #xpos=prod_start,
    # Use (id-0.5), not just id, to center label on each item
    angle=90-360*(xpos-0.5)/max(xpos)
  )%>%
  mutate(
    hjust=case_when(
      angle<=-90~1,
      TRUE~0
    )
  )%>%
  # Flip left side labels
  mutate(
    angle=case_when(
      angle<=-90~angle+180,
      TRUE~angle
    )
  )

rk_sub<-subcont%>%
  select(subregion_name,rank_subcontinent)

countries<-data%>%
  group_by(area)%>%
  summarize(
    region_name=region_name[1],
    subregion_name=subregion_name[1],
    prod=sum(prod)
  )%>%
  ungroup()%>%
  left_join(rk_cont)%>%
  left_join(rk_sub)%>%
  arrange(rank_continent,rank_subcontinent,-prod)%>%
  #arrange(rank)%>%
  #arrange(region_name)%>%
  mutate(
    prod_end=cumsum(prod),
    prod_start=prod_end-prod
  )%>%
  drop_na()%>%
  mutate(ct=1)%>%
  group_by(subregion_name)%>%
  mutate(rank=-1-cumsum(ct)/sum(ct))%>%
  ungroup()

countries_name <-  countries%>%
  mutate(
    xpos=prod_start+prod/2,
    #xpos=prod_start,
    # Use (id-0.5), not just id, to center label on each item
    angle=90-360*(xpos-0.5)/max(xpos)
  )%>%
  mutate(
    hjust=case_when(
      angle<=-90~1,
      TRUE~0
    )
  )%>%
  # Flip left side labels
  mutate(
    angle=case_when(
      angle<=-90~angle+180,
      TRUE~angle
    )
  )

pal<-c(
  "Africa"="#FFCA3A",
  "Asia"="#6A4C93",
  "Europe"="#8AC926",
  "Americas"="#1982C4",
  "Oceania"="#FF595E"
)

ggplot()+
  geom_rect(
    data=continent,
    aes(xmin=prod_start,xmax=prod_end,ymin=1,ymax=2.8,fill=region_name)
  )+
  geom_rect(
    data=subcont,
    aes(xmin=prod_start,xmax=prod_end,ymin=3,ymax=4.8,fill=region_name,alpha=rank)
    #color=alpha("dimgrey",0.1)
  )+
  geom_rect(
    data=countries,
    aes(xmin=prod_start,xmax=prod_end,ymin=5,ymax=6.8,fill=region_name,alpha=rank)
    #color="black"
  )+
  geom_text(
    data=continent_name%>%filter(region_name%in%c("Asia","Europe","Americas")),
    aes(x=xpos,y=1.15,angle=angle,label=region_name,hjust=hjust),
    color="white",size=11,family="cab",fontface='bold'
  )+
  geom_text(
    data=subcont_name%>%filter(prod>500000),
    aes(x=xpos,y=3.1,angle=angle,label=subregion_name,hjust=hjust),
    color="white",size=5,family="cab",vjust=0.5
  )+
  geom_text(
    data=countries_name%>%filter(prod>5000000),
    aes(x=xpos,y=5.1,angle=angle,label=area,hjust=hjust),
    color="white",size=5,family="cab",vjust=0.5
  )+
  scale_y_continuous(limits=c(-1,7))+
  scale_alpha(range=c(0.1,0.9))+
  scale_fill_manual(values=pal)+
  guides(fill='none',alpha='none')+
  coord_polar()+
  theme_void()




