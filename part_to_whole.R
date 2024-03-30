library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)

# Set fonts
font_add_google("Catamaran","cata")
font_add_google("Cabin","cab")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Roboto Slab","rob")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 24, 
  height = 26, 
  units = "cm", 
  dpi = 300 
)

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
    #angle=90-360*(xpos-0.5)/max(xpos)
    angle=90-360*(xpos-0.5)/max(prod_end)
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
    angle=90-360*(xpos-0.5)/max(prod_end)
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
  )%>%
  mutate(subregion_name=case_when(
    subregion_name=="Latin America and the Caribbean"~"Latin America",
    subregion_name=="Australia and New Zealand"~"Australia and NZ",
    TRUE~subregion_name
  ))

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
  mutate(
    prod_end=cumsum(prod),
    prod_start=prod_end-prod
  )%>%
  drop_na()%>%
  mutate(ct=1)%>%
  group_by(subregion_name)%>%
  mutate(rank=-0.5-cumsum(ct)/sum(ct))%>%
  ungroup()

countries_name <-  countries%>%
  mutate(
    xpos=prod_start+prod/2,
    angle=90-360*(xpos-0.5)/max(prod_end)
  )%>%
  mutate(
    hjust=case_when(
      angle<=-90~1,
      TRUE~0
    )
  )%>%
  mutate(
    angle=case_when(
      angle<=-90~angle+180,
      TRUE~angle
    )
  )%>%
  mutate(area=case_when(
    area=="United Kingdom of Great Britain and Northern Ireland"~"UK",
    area=="Iran (Islamic Republic of)"~"Iran",
    area=="United States of America"~"USA",
    TRUE~area
  ))

pal<-c(
  "Africa"="#FFD23F",
  "Asia"="#EE4266",
  "Europe"="#0EAD69",
  "Americas"="#540D6E",
  "Oceania"="#47A8BD"
)


ggplot()+
  #annotate(geom='rect',xmin=-Inf,xmax=+Inf,ymin=1,ymax=2.8,fill="white",color=alpha("black",0))+
  annotate(geom='rect',xmin=-Inf,xmax=+Inf,ymin=3,ymax=4.8,fill="white",color=alpha("black",0))+
  annotate(geom='rect',xmin=-Inf,xmax=+Inf,ymin=5,ymax=6.8,fill="white",color=alpha("black",0))+
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
  geom_segment(
    data=continent,
    aes(x=prod_start,xend=prod_start,y=1,yend=7),
    color="white"
  )+
  geom_segment(
    data=subcont,
    aes(x=prod_start,xend=prod_start,y=3,yend=6.9),
    color=alpha("white",0.5)
  )+
  geom_text(
    #data=continent_name%>%filter(region_name%in%c("Asia","Europe","Americas")),
    data=continent_name,
    aes(x=xpos,y=1.15,angle=angle,label=region_name,hjust=hjust),
    color="white",size=11,family="cab",fontface='bold'
  )+
  geom_text(
    data=subcont_name%>%filter(prod>500000),
    aes(x=xpos,y=3.1,angle=angle,label=subregion_name,hjust=hjust),
    color="white",size=6,family="cab",vjust=0.5
  )+
  geom_text(
    data=countries_name%>%filter(prod>5000000),
    aes(x=xpos,y=5.1,angle=angle,label=area,hjust=hjust),
    color="white",size=6,family="cab",vjust=0.5
  )+
  scale_y_continuous(limits=c(-1,7))+
  scale_alpha(range=c(0.1,0.9))+
  scale_fill_manual(values=pal)+
  guides(fill='none',alpha='none')+
  coord_polar()+
  labs(
    title="Wheat production in the world",
    subtitle = "average from 2010 to 2020",
    caption = "**Data** FAOStat **| Plot** Benjamin Nowak"
  )+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#222725",color=NA),
    plot.title = element_text(family="cata",size=75,hjust=0.5,color="white",face='bold',margin=margin(1.5,0,0,0,'cm')),
    plot.subtitle = element_text(family="ral",size=50,hjust=0.5,color="white",margin=margin(0.25,0,-1.5,0,'cm')),
    plot.caption = element_markdown(family="ral",size=40,hjust=0.5,color="white",margin=margin(-1.25,0,0.5,0,'cm'))
  )




