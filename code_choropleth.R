# Load packages:
library(rnaturalearth) # for basemap
library(sf)            # for vectors processing
library(tidyverse)

# 1. Load basemap
world <- ne_countries(
  scale = 110,
  type = "countries",
  returnclass = "sf"
)

# Plot map
ggplot(world)+
  geom_sf()

# 2. Plot variable of interest
world<-world%>%
  # Compute GDP per capita
  mutate(gdp_per_capita=gdp_md_est/as.numeric(pop_est)*1000000)%>%
  # Remove value for Antarctica
  mutate(gdp_per_capita=case_when(
    name!='Antarctica'~gdp_per_capita,
    TRUE~NA_real_
  ))

# Make choropleth
p1<-ggplot(world)+
  geom_sf(aes(fill=gdp_per_capita))+
  scale_fill_viridis_b(
    limits=c(100,70000),
    n.breaks=6,
    labels = function(x) paste(x, '$'),
    option='plasma',
    na.value = "grey90"
  )+
  labs(fill="GDP per capita")

p1
  

# 3. Customize map
col_back<-'#EEE2DF'

theme_map<-theme_void()+
  theme(
    plot.margin=margin(1,1,1,1,'cm'),
    plot.background = element_rect(
      fill=col_back,color=NA
    ),
    legend.title = element_text(
      face='bold',
      margin=margin(0,0,0.25,0,'cm')
    )
  )

p2<-p1+theme_map
p2

# 4. Change projection

# Check world crs
st_crs(world)

p3<-p2+
  # Change CRS to Robinson
  coord_sf(crs='+proj=robin')

p3

# 5. Create graticule

grat <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9))

p3b<-p2+
  geom_sf(
    grat,
    mapping=aes(geometry=geometry),
    alpha=0.5
  )+
  # Change CRS to Robinson
  coord_sf(crs='+proj=robin')

p3b




