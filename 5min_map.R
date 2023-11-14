# Load packages:
library(rnaturalearth) # for basemap
library(sf)            # for vectors processing
library(tidyverse)
library(cartogram)
library(ggtext)

# 1. Load basemap
world <- ne_countries(
  scale = 110,
  type = "countries",
  returnclass = "sf"
)

# 2. Compute variable of interest
world<-world%>%
  # Compute GDP per capita
  mutate(gdp_per_capita=gdp_md_est/as.numeric(pop_est)*1000000)%>%
  # Remove value for Antarctica
  mutate(gdp_per_capita=case_when(
    name!='Antarctica'~gdp_per_capita,
    TRUE~NA_real_
  ))%>%
  st_transform(crs='+proj=robin')

# 3. Make Dorling carto
dorl<-cartogram::cartogram_dorling(
  world%>%drop_na(gdp_per_capita)%>%filter(continent!="Seven seas (open ocean)"), 
  weight='gdp_per_capita', k = 4,
  m_weight = 0.05, itermax = 1000
)

# 4. Make map
col_back<-'#EEE2DF'
col_pal <- scico::scico(6, palette = 'bam')

theme_map<-theme_void()+
  theme(
    plot.margin=margin(1,1,1,1,'cm'),
    plot.background = element_rect(
      fill=col_back,color=NA
    ),
    legend.title = element_text(
      face='bold',
      margin=margin(0,0,0,0,'cm')
    ),
    plot.title=element_markdown()
  )

p1<-ggplot(dorl,aes(fill=continent))+
  geom_sf()+
  geom_sf_text(
    dorl%>%arrange(-gdp_per_capita)%>%head(10),
    mapping=aes(label=name),
    inherit.aes='FALSE'
  )+
  scale_fill_manual(values=col_pal)+
  labs(title="**Gross Domestic Product per capita**",subtitle="With name for top10 countries")+
  theme_map
p1

ggsave(filename='5min_map.png',plot=p1,width=20,height=16,unit='cm',dpi=300)


