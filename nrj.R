library(cartogram)
library(sf)
library(tidyverse)
library(rnaturalearth)
library(camcorder)
library(MoMAColors)
library(glue)
library(ggtext)
library(ggforce)
library(showtext)

# Set fonts
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Fira Sans","fira")
font_add_google("Jost","jost")
showtext_auto()

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 24, 
  height = 6.75*2, 
  units = "cm", 
  dpi = 600 
)

mp_with_countries<-ne_countries(scale = 50, type = "countries", returnclass = "sf")%>%
  #st_transform(crs="EPSG:3035")%>%
  st_transform(crs="+proj=robin")

######################################
# Dorling cartogram for Africa

# Load data
owid_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')


clean<-owid_energy%>%
  filter(year>2010)%>%
  mutate(iso_code=case_when(
    country=="Kosovo"~'KOS',
    TRUE~iso_code
  ))%>%
  drop_na(iso_code)%>%
  group_by(country)%>%
  summarize(
    iso_code=iso_code[1],
    biofuel=mean(na.omit(biofuel_elec_per_capita)),
    coal=mean(na.omit(coal_elec_per_capita)),
    gas=mean(na.omit(gas_elec_per_capita)),
    hydro=mean(na.omit(hydro_elec_per_capita)),
    nuke=mean(na.omit(nuclear_elec_per_capita)),
    oil=mean(na.omit(oil_elec_per_capita)),
    solar=mean(na.omit(solar_elec_per_capita)),
    wind=mean(na.omit(wind_elec_per_capita)),
    total=biofuel+coal+gas+hydro+nuke+oil+solar+wind
  )%>%
  ungroup()

# Make cartograms
#################

# Join data and map
world<-mp_with_countries%>%
  left_join(clean,by=c("iso_a3"="iso_code"))%>%
  #filter(continent=="Europe")%>%
  drop_na(total)

# Making Dorling cartogram based on total waking hours
dorl<-cartogram_dorling(world, weight="total", k = 1, m_weight = 0.25, itermax = 1000)

ggplot()+
  geom_sf(world,mapping=aes(geometry=geometry))+
  geom_sf(dorl,mapping=aes(geometry=geometry))

# Compute area and radius for each circus
d2<-dorl%>%
  mutate(
    ar=st_area(dorl),
    rad=sqrt(ar/pi)
  )

# Extract centroids for each circle
centr <- dorl%>%
  st_centroid()%>%
  st_coordinates()

# Merge area and centroids for total time
# and compute radius for different activities 
d3 <- tibble(d2,X=centr[,1],Y=centr[,2])%>%
  mutate(rad=as.numeric(rad))%>%
  mutate(
    # Renewable
    ratio_renew = (biofuel+hydro+solar+wind)/total,
    ratio_fossil = (oil+gas+coal)/total,
    ratio_nuke = nuke/total
  )%>%
  mutate(
    rad_renew=sqrt(rad*rad*ratio_renew),
    rad_fossil=sqrt(rad*rad*ratio_fossil),
    rad_nuke=sqrt(rad*rad*ratio_nuke)
  )

# Function to draw circles
# Inspired by post on StackOverflow to define function to draw (half) circle:
# https://stackoverflow.com/questions/28185743/draw-a-half-circle-with-ggplot2

circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2)
{
  tt <- seq(start*pi, end*pi, length.out=npoints)
  df<-data.frame(
    x = center[1] + diameter / 2 * cos(tt), 
    y = center[2] + diameter / 2 * sin(tt)
  )
  df2<-bind_rows(
    df,
    tibble(x = center[1],y=center[2]),
  )
  
  return(df2)
}


# Apply function to all countries for arable land and grassland
dord<-d3%>%
  arrange(iso_a3)

t1 <- tibble(
  iso = dord$iso_a3[1],
  x = dord$X[1],
  y = dord$Y[1]
)


c_renew <- c(0,2*1/3)
c_fossil <- c(2*1/3,2*2/3)
c_nuke <- c(2*2/3,2)

renew<-tibble(iso=c(),x=c(),y=c())
nuke<-tibble(iso=c(),x=c(),y=c())
fossil<-tibble(iso=c(),x=c(),y=c())

# Draw for renew
for (i in 1:dim(dord)[1]){
  
  # compared to the version of the tutorial created for R Graph Gallery, 
  # the only difference here is that I need to add a point for the center 
  # (as I don't work with half circles).
  t1 <- tibble(
    iso = dord$iso_a3[i],
    x = dord$X[i],
    y = dord$Y[i]
  )
  
  temp_renew <- 
    tibble(
      iso=dord$iso_a3[i],
      circleFun(
        c(dord$X[i],dord$Y[i]),diameter=dord$rad_renew[i]*2, start=c_renew[1], end=c_renew[2]
      )
    )%>%
    bind_rows(t1)
  
  temp_nuke <- 
    tibble(
      iso=dord$iso_a3[i],
      circleFun(
        c(dord$X[i],dord$Y[i]),diameter=dord$rad_nuke[i]*2, start=c_nuke[1], end=c_nuke[2]
      )
    )%>%
    bind_rows(t1)
  
  temp_fossil <- 
    tibble(
      iso=dord$iso_a3[i],
      circleFun(
        c(dord$X[i],dord$Y[i]),diameter=dord$rad_fossil[i]*2, start=c_fossil[1], end=c_fossil[2]
      )
    )%>%
    bind_rows(t1)
  
  renew<-renew%>%
    bind_rows(temp_renew)
  nuke<-nuke%>%
    bind_rows(temp_nuke)
  fossil<-fossil%>%
    bind_rows(temp_fossil)
}


col_world <- "#073B4C"
col_borders <- "grey80"



pal_dis<-moma.colors("Lupi" , n=3, type="continuous")
pal_dis<-moma.colors("Klein" , n=10, type="discrete")
pal_dis<-moma.colors("Doughton" , n=9, type="continuous")
pal_dis

col_fossil <- pal_dis[6]
col_nuke <- "#FFA630"
col_renew <- pal_dis[4]
col_coun<-"grey90"
col_back <- "white"


grat <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9))

ggplot()+
  #geom_sf(mp,mapping=aes(geometry=geometry),fill=pal[22],color=alpha("white",0))+
  geom_sf(mp,mapping=aes(geometry=geometry),fill=col_coun,color=alpha("white",0.15),lwd=0.1)+
  #geom_sf(mp,mapping=aes(geometry=geometry),fill=NA,color=alpha("white",1),lwd=0.25)+
  
  
  geom_sf(mp_with_countries,mapping=aes(geometry=geometry),fill=NA,color=alpha("white",0.05),lwd=0.15)+
  
  
  # Add dorling cartogram
  geom_circle(
    data = d3,
    aes(x0 = X, y0 = Y, r = rad),
    color=alpha("white",0.25),
    fill="#6C809A",alpha=0.5,
    linewidth=0.05
  )+
  geom_polygon(
    renew,
    mapping=aes(x,y,group=iso),
    fill=col_renew,color=NA
  )+ 
  geom_polygon(
    nuke,
    mapping=aes(x,y,group=iso),
    fill=col_nuke,color=NA
  )+
  geom_polygon(
    fossil,
    mapping=aes(x,y,group=iso),
    fill=col_fossil,color=NA
  )+
  
  # Add graticule
  geom_sf(
    grat, mapping=aes(geometry=geometry),
    color=alpha("black",0.05)
  )+
  theme_void()+
  theme(plot.background = element_rect(fill=NA,color=NA))


