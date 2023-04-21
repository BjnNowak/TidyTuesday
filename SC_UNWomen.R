library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(sf)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 32, 
  height = 16.6, 
  units = "cm", 
  dpi = 300 
)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load data
data <- read_delim('Data/Labor/pop_agri_en.csv',
  #locale=locale(encoding="latin1"),
  delim=',')



test<-data%>%filter(Area=="Namibia")

test<-data%>%filter(Area=="Angola")%>%
  filter(Indicator=="Employment in agriculture, forestry and fishing - ILO modelled estimates")%>%
  filter(Sex=="Female"|Sex=="Total")%>%
  select(Area,Year,M49="Area Code (M49)",Sex,Value)%>%
  pivot_wider(names_from=Sex,values_from=Value)%>%
  mutate(fem_share=Female/Total*100)

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
  mutate(M49 = M49_code)

######
# clean data

fem_share<- data%>%
  filter(Indicator=="Share of females in total employment in agriculture, forestry and fishing")%>%
  group_by(Area)%>%
  filter(Year==max(Year))%>%
  ungroup()%>%
  select(Area,M49="Area Code (M49)",fem_share=Value)

# Second way to estimate share of female : less missing data
fem_share<-data%>%
  #filter(Area=="Angola")%>%
  filter(Indicator=="Employment in agriculture, forestry and fishing - ILO modelled estimates")%>%
  filter(Sex=="Female"|Sex=="Total")%>%
  select(Area,Year,M49="Area Code (M49)",Sex,Value)%>%
  pivot_wider(names_from=Sex,values_from=Value)%>%
  mutate(fem_share=Female/Total*100)%>%
  group_by(Area)%>%
  filter(Year==max(Year))%>%
  ungroup()%>%
  select(Area,M49,fem_share)
fem_share

agri_share<- data%>%
  filter(
    Indicator=="Share of employment in agriculture, forestry and fishing in total employment - ILO Modelled Estimates",
    Sex=="Total"
  )%>%
  group_by(Area)%>%
  filter(Year==max(Year))%>%
  ungroup()%>%
  select(Area,M49="Area Code (M49)",agri_share=Value)

clean_mrg <- fem_share%>%
  inner_join(agri_share)

# Get quantiles for both variables
qfem <- quantile(clean_mrg$fem_share,probs=c(0.33,0.66))
qagri <- quantile(clean_mrg$agri_share,probs=c(0.33,0.66))

clean<-clean_mrg%>%
  mutate(fem_class=case_when(
    fem_share < qfem[1] ~ "A1",
    fem_share <= qfem[2] ~ "B1",
    fem_share > qfem[2] ~ "C1"
  ))%>%
  mutate(
    agri_class=case_when(
      agri_share < qagri[1] ~ "A2",
      agri_share <= qagri[2] ~ "B2",
      agri_share > qagri[2] ~ "C2"
  ))%>%
  mutate(full=glue::glue("{agri_class}_{fem_class}"))%>%
  mutate(M49=as.numeric(M49))


summary(as.factor(clean$full))

quantile(clean$fem_share,probs=c(0.33,0.66))
quantile(clean$agri_share,probs=c(0.33,0.66))

clean$M49[clean$Area=="China"]<-156

mrg <- clean%>%
  left_join(mat)

mrg$iso_alpha2_code[mrg$Area=="Namibia"]<-"NAM"

# open map
world_ne <- sf::read_sf("Data/Erosion/data/world_map/ne_110m_admin_0_countries_lakes.shp")
  
world_ne$ISO_A2_EH[world_ne$ADMIN=="Namibia"]<-"NAM"

map_ne <- world_ne%>%
  left_join(mrg,by=c("ISO_A2_EH"="iso_alpha2_code"))

map_ne$full[map_ne$ADMIN=="Greenland"]<-NA


# Bivariate color palette
pal<-c(
  "A2_A1"="#f3f3f3",
  "A2_B1"="#ebc5de",
  "A2_C1"="#e6a2d1",
  "B2_A1"="#c2efce", 
  "B2_B1"="#9ec5d4",
  "B2_C1"="#bb9fcf",
  "C2_A1"="#8ae1ae",
  "C2_B1"="#7cc4b2", 
  "C2_C1"="#7890aa"
)





test_map <- map_ne%>%filter(ADMIN=="Namibia")


ggplot()+
  geom_sf(
    map_ne,
    mapping=aes(geometry=geometry,fill=full),
    size=0.1,color="white")+
  #coord_sf(crs = "ESRI:54030")+
  coord_sf(crs= "+proj=eck4")+
  scale_fill_manual(values=pal,na.value = "grey95")+
  guides(fill="none")+
  theme_void()+
  theme(
    legend.position="bottom",
    legend.title=element_markdown(
      size=34,family = "bit",face="bold",
      hjust=0.5,lineheight = 0.5,margin=margin(0.5,0,-0.25,0,"cm")
    ),
    legend.text=element_markdown(
      size=32,family = "cond",hjust=0.5,
      color="#6F8695",
      lineheight = 0.5,margin=margin(-0.25,0,0,0,"cm")),
    plot.margin = margin(1,1,1,1,"cm"),
    plot.background = element_rect(
      #fill="#1D201F",
      fill=NA,
      color=NA),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_line(color=alpha("grey90",0.5),linewidth=0.01)
  )


#####################
# Making the legend

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

tib<-tibble(
  x1 = rep(c("A","B","C"),3),
  y2 = c(rep("A",3),rep("B",3),rep("C",3)),
  value = glue::glue("{y2}2_{x1}1")
)

ggplot(data=tib,aes(x=y2,y=x1,fill=value))+
  geom_point(pch=21,size=40,color="grey90")+
  #geom_tile()+
  #geom_text(aes(label=value),size=10)+
  scale_fill_manual(values=pal)+
  guides(fill="none")+
  theme_void()


# another try...

ggplot(clean,aes(x=fem_share,y=agri_share,fill=full))+
  annotate("segment",x=qfem[1],xend=qfem[1],y=-Inf,yend=Inf)+
  annotate("segment",x=qfem[2],xend=qfem[2],y=-Inf,yend=Inf)+
  annotate("segment",y=qagri[1],yend=qagri[1],x=-Inf,xend=Inf)+
  annotate("segment",y=qagri[2],yend=qagri[2],x=-Inf,xend=Inf)+
  geom_point(size=4,pch=21,color="grey50")+
  guides(fill="none")+
  scale_fill_manual(values=pal)+
  theme_void()
#theme(plot.background = element_rect(fill="#1D201F"))

