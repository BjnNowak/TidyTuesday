library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(tm)
library(sf)
library(geomtextpath)

# install.packages("remotes")
#remotes::install_github("AllanCameron/geomtextpath")

# Set fonts
font_add_google("Open Sans","open")
font_add_google("Source Sans Pro","pro")
font_add_google("Outfit","out")
font_add_google("Playfair Display","title")
font_add_google("Noto Serif Ethiopic","amh")


showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 15, 
  units = "cm", 
  dpi = 300 
)

# Load data
afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')

res<-afrisenti%>%
  mutate(ct=1)%>%
  group_by(language_iso_code)%>%
  summarize(sm=sum(ct))

# Load map
states <- sf::st_as_sf(maps::map(database="world", plot = FALSE, fill = TRUE))
world <- sf::read_sf("Data/Erosion/data/world_map/ne_110m_admin_0_countries_lakes_guy.shp")

africa<-world%>%filter(REGION_UN=="Africa")



amh_neg<-afrisenti%>%
  filter(language_iso_code=="amh")%>%
  filter(label=="negative")

amh_pos<-afrisenti%>%
  filter(language_iso_code=="amh")%>%
  filter(label=="positive")

arq_neg<-afrisenti%>%
  filter(language_iso_code=="arq")%>%
  filter(label=="negative")

arq_pos<-afrisenti%>%
  filter(language_iso_code=="arq")%>%
  filter(label=="positive")

pcm_neg<-afrisenti%>%
  filter(language_iso_code=="pcm")%>%
  filter(label=="negative")

pcm_pos<-afrisenti%>%
  filter(language_iso_code=="pcm")%>%
  filter(label=="positive")

ha_neg<-afrisenti%>%
  filter(language_iso_code=="hau")%>%
  filter(label=="negative")

ha_pos<-afrisenti%>%
  filter(language_iso_code=="hau")%>%
  filter(label=="positive")

corp <- function(samp){

  tx<-paste(samp$tweet,collapse="")
  # Create a corpus  
  docs <- Corpus(VectorSource(tx))
  # Clean text
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs<-tm_map(docs,toSpace, "'")
  docs <- docs %>%
    tm_map(removeNumbers)%>%
    tm_map(removePunctuation)
  # Create a document-term-matrix
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- tibble(word = names(words),freq=words)
  
  return(df)
}


df_neg<-corp(amh_neg)%>%mutate(negative=freq)%>%select(word,negative)
df_pos<-corp(amh_pos)%>%mutate(positive=freq)%>%select(word,positive)

df_neg_arq<-corp(arq_neg)%>%mutate(negative=freq)%>%select(word,negative)
df_pos_arq<-corp(arq_pos)%>%mutate(positive=freq)%>%select(word,positive)

df_neg_pcm<-corp(pcm_neg)%>%mutate(negative=freq)%>%select(word,negative)
df_pos_pcm<-corp(pcm_pos)%>%mutate(positive=freq)%>%select(word,positive)

df_neg_ha<-corp(ha_neg)%>%mutate(negative=freq)%>%select(word,negative)
df_pos_ha<-corp(ha_pos)%>%mutate(positive=freq)%>%select(word,positive)

clean<-df_neg%>%
  full_join(df_pos)%>%
  filter(word!="user")%>%
  mutate(
    freq_pos=positive/sum(positive,na.rm=TRUE)*100,
    freq_neg=negative/sum(negative,na.rm=TRUE)*100,
    gap=freq_pos-freq_neg
  )%>%
  mutate(eng=case_when(
    # keep
    word=="ሰው"~"man",
    word=="ብቻ"~"alone",
    word=="ሴቶች"~"woman",
    word=="መንግስት"~"government",
    word=="መልካም"~"good",
    word=="ህዝብ"~"people",
    word=="ደስ"~"happy",
    word=="አፍሪካ"~"africa",
    word=="መሪ"~"leader",  
    word=="ኢትዮጵያ"~"ethiopia", 
  ))%>%
  mutate(eng2=glue::glue("{eng}   {eng}"))%>%
  mutate(id=case_when(
    # keep
    eng=="people"~1,
    eng=="man"~2,
    eng=="woman"~3,
    eng=="good"~4,
    eng=="happy"~5,
    eng=="ethiopia"~6,
    eng=="government"~7,
    eng=="leader"~8,
    eng=="africa"~9,
    eng=="alone"~10
  ))

clean_arq<-df_neg_arq%>%
  full_join(df_pos_arq)%>%
  filter(word!="user")%>%
  mutate(
    freq_pos=positive/sum(positive,na.rm=TRUE)*100,
    freq_neg=negative/sum(negative,na.rm=TRUE)*100,
    gap=freq_pos-freq_neg
  )%>%
  mutate(eng=case_when(
    # keep
    word=="راجل"~"man",
    word=="وحداني"~"alone",
    word=="ሴቶች"~"woman",
    word=="بايلٌك"~"government",
    word=="مليح"~"good",
    word=="هم"~"people",
    word=="فرحان"~"happy",
    word=="فريقيا"~"africa",
    word=="معـلـّم"~"leader",  
    word=="الجزائر"~"algeria", 
  ))%>%
  
  mutate(eng2=glue::glue("{eng}   {eng}"))

clean_ha<-df_neg_pcm%>%
  full_join(df_pos_pcm)%>%
  filter(word!="user")%>%
  mutate(
    freq_pos=positive/sum(positive,na.rm=TRUE)*100,
    freq_neg=negative/sum(negative,na.rm=TRUE)*100,
    gap=freq_pos-freq_neg
  )%>%
  mutate(eng=case_when(
    # keep
    word=="man"~"man",
    word=="alone"~"alone",
    word=="woman"~"woman",
    word=="government"~"government",
    word=="good"~"good",
    word=="people"~"people",
    word=="happy"~"happy",
    word=="africa"~"africa",
    word=="leader"~"leader",  
    word=="nigeria"~"niger or nigeria", 
  ))%>%
  mutate(eng2=glue::glue("{eng}   {eng}"))%>%
  mutate(id=case_when(
    # keep
    eng=="people"~10,
    eng=="man"~9,
    eng=="woman"~8,
    eng=="good"~7,
    eng=="happy"~6,
    eng=="niger or nigeria"~5,
    eng=="government"~4,
    eng=="leader"~3,
    eng=="africa"~2,
    eng=="alone"~1
  ))


clean_pcm<-df_neg_pcm%>%
  full_join(df_pos_pcm)%>%
  filter(word!="user")%>%
  mutate(
    freq_pos=positive/sum(positive,na.rm=TRUE)*100,
    freq_neg=negative/sum(negative,na.rm=TRUE)*100,
    gap=freq_pos-freq_neg
  )%>%
  mutate(eng=case_when(
    # keep
    word=="man"~"man",
    word=="alone"~"alone",
    word=="woman"~"woman",
    word=="government"~"government",
    word=="beta"~"good",
    word=="pipo"~"people",
    word=="happy"~"happy",
    word=="africa"~"africa",
    word=="oga"~"leader",  
    word=="naija"~"nigeria", 
  ))%>%
  mutate(eng2=glue::glue("{eng}   {eng}"))%>%
  mutate(id=case_when(
    # keep
    eng=="people"~10,
    eng=="man"~9,
    eng=="woman"~8,
    eng=="good"~7,
    eng=="happy"~6,
    eng=="nigeria"~5,
    eng=="government"~4,
    eng=="leader"~3,
    eng=="africa"~2,
    eng=="alone"~1
  ))


df_amh<-clean%>%drop_na()
df_pcm<-clean_pcm%>%drop_na()
df_ha<-clean_ha%>%drop_na()

col_ps <- "#1dbde6"
col_ps_lg <-"#DAF4FB"
col_ng <- "#f1515e"
col_ng_lg <- "#FDECEE"

leg_pos <- tibble(
  x=0.55,
  y=6,
  label="**positive** tweets"
)

leg_neg <- tibble(
  x=0.55,
  y=6,
  label="**negative** tweets"
)

eth<-ggplot(df_amh)+
  geom_segment(aes(y=id,yend=id,x=0,xend=0.5),color="grey80",lwd=0.1)+
  geom_segment(aes(y=id,yend=id,x=1.5,xend=2),color="grey80",lwd=0.1)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_pos),fill=col_ps)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=2-freq_neg,xmax=2),fill=col_ng)+
  geom_textpath(
    aes(y=id,x=0,label=eng2),
    upright=TRUE,
    size=7,family="out",hjust=0.5,vjust=0.5,spacing=-200)+

  coord_polar(start=pi/4)+
  scale_y_continuous(limits=c(-4,10.5))+
  scale_x_continuous(limits=c(0,2))+
  theme_void()


eth

corr_pcm <- 0.8
nig<-ggplot(df_pcm)+
  geom_segment(aes(y=id,yend=id,x=0,xend=0.5*corr_pcm),color="grey80",lwd=0.1)+
  geom_segment(aes(y=id,yend=id,x=1.5*corr_pcm,xend=2*corr_pcm),color="grey80",lwd=0.1)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_neg**corr_pcm),fill=col_ng)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=2*corr_pcm-freq_pos**corr_pcm,xmax=2*corr_pcm),fill=col_ps)+
  geom_textpath(
    aes(y=id,x=0,label=eng2),
    upright=TRUE,
    size=7,family="out",hjust=0.5,vjust=0.5,spacing=-200)+
  geom_textpath(
    aes(y=id,x=1.5*corr_pcm,label=eng),
    upright=TRUE,
    size=7,family="out",hjust=0.5,vjust=0.5,spacing=-200)+

  coord_polar(start=pi+pi/4)+
  scale_y_continuous(limits=c(-4,10.5))+
  scale_x_continuous(limits=c(0,2*corr_pcm))+
  theme_void()
nig

ha<-ggplot(df_ha)+
  geom_segment(aes(y=id,yend=id,x=0,xend=0.5),color="grey80",lwd=0.1)+
  geom_segment(aes(y=id,yend=id,x=1.5,xend=2),color="grey80",lwd=0.1)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_neg),fill=col_ng)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=2-freq_pos,xmax=2),fill=col_ps)+
  geom_textpath(
    aes(y=id,x=0,label=eng2),
    upright=TRUE,
    size=7,family="out",hjust=0.5,vjust=0.5,spacing=-200)+
  geom_textpath(
    aes(y=id,x=0.5,label=eng),
    upright=TRUE,
    size=7,family="out",hjust=0.5,vjust=0.5,spacing=-200)+
  
  coord_polar(start=pi+pi/4)+
  scale_y_continuous(limits=c(-4,10.5))+
  scale_x_continuous(limits=c(0,2))+
  theme_void()
ha


eth_map <- ggplot()+
  geom_sf(africa,mapping=aes(geometry=geometry),fill=alpha("#26547C",0.5),color=alpha("white",0.25))+
  geom_sf(africa%>%filter(ADMIN=="Ethiopia"),mapping=aes(geometry=geometry),fill="#FF7D00",color=alpha("white",0.25))+
  theme_void()

nig_map <- ggplot()+
  geom_sf(africa,mapping=aes(geometry=geometry),fill=alpha("#26547C",0.5),color=alpha("white",0.25))+
  geom_sf(africa%>%filter(ADMIN=="Nigeria"),mapping=aes(geometry=geometry),fill="#FF7D00",color=alpha("white",0.25))+
  theme_void()

ha_map <- ggplot()+
  geom_sf(africa,mapping=aes(geometry=geometry),fill=alpha("#26547C",0.5),color=alpha("white",0.25))+
  geom_sf(africa%>%filter(ADMIN=="Nigeria"|ADMIN=="Niger"),mapping=aes(geometry=geometry),fill="#FF7D00",color=alpha("white",0.25))+
  theme_void()

ha_map

# Make plots

design <- c(
  area(1, 1, 100,100),
  area(41,41,65,65)
)

ha + ha_map + plot_layout(design = design)&
  theme(plot.background = element_rect(fill=NA,color=NA))

eth + eth_map + plot_layout(design = design)&
  theme(plot.background = element_rect(fill=NA,color=NA))

nig + nig_map + plot_layout(design = design)&
  theme(plot.background = element_rect(fill=NA,color=NA))

