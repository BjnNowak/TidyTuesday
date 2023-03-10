library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(tidygraph)
library(ggraph)
library(igraph)
library(waffle)


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
  width = 5, 
  height = 16.6, 
  units = "cm", 
  dpi = 300 
)

# Load data
###########

data<-read_delim('Data/Water/data/aquastat_bulk.csv',delim=",")
pop <- read_delim('Data/Chicken/data/fao_pop.csv',delim=',')
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

clean<-data%>%
  left_join(mat,by=c("M49"))%>%
  filter(Variable%in%c("Municipal water withdrawal","Industrial water withdrawal","Agricultural water withdrawal"))



pop2<-pop%>%
  rename(M49="Area Code (M49)")%>%
  mutate(M49=as.numeric(M49))%>%
  select(Area,M49,Year,Pop=Value)

short<-clean%>%
  select(Country,M49,Variable,Year,Value,region_name,subregion_name)%>%
  group_by(Country)%>%
  filter(Year==max(Year))%>%
  ungroup()%>%
  left_join(pop2)%>%
  mutate(cs=(Value*1000000)/Pop)

res <- short%>%
  group_by(subregion_name,Variable)%>%
  summarize(
    cs_mn=mean(cs),
    water=sum(Value),
    pop=sum(Pop)
  )%>%
  mutate(
    conso=(water*1000000)/pop, # in m3
    conso_10 = round(conso/10)
  ) 
  
total <- short%>%
  group_by(subregion_name)%>%
  summarize(
    cs_mn=mean(cs),
    water=sum(Value),
    pop=sum(Pop)/3
  )%>%
  mutate(
    conso=(water*1000000)/pop, # in m3
    conso_10 = round(conso/10)
  ) 


total

pal<-c(
  "Agricultural water withdrawal"="#04A777",
  "Industrial water withdrawal"="#F0803C",
  "Municipal water withdrawal"='#133C55'
)



pal<-c(
  "Agricultural water withdrawal"='#00AF54',
  "Industrial water withdrawal"="#FF006E",
  "Municipal water withdrawal"="#FBAF00"
)

ggplot(
  data=res%>%filter(subregion_name=="Sub-Saharan Africa"),
  aes(fill = Variable, values = conso_10)) +
  geom_waffle(n_rows = 5, size = 0.33, colour = "white", flip = TRUE) +
  scale_fill_manual(values = pal) +
  guides(fill='none')+
  scale_y_continuous(limits=c(0,40))+
  coord_equal() +
  theme_void()
  


