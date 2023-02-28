library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(tm)
library(sf)


# Set fonts
font_add_google("Open Sans","open")
font_add_google("Playfair Display","title")
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

# Load map
states <- sf::st_as_sf(maps::map(database="world", plot = FALSE, fill = TRUE))
world <- sf::read_sf("Data/Erosion/data/world_map/ne_110m_admin_0_countries_lakes_guy.shp")

africa<-world%>%filter(REGION_UN=="Africa")



samp<-afrisenti%>%head(10)

amh_neg<-afrisenti%>%
  filter(language_iso_code=="amh")%>%
  filter(label=="negative")

amh_pos<-afrisenti%>%
  filter(language_iso_code=="amh")%>%
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

clean<-df_neg%>%
  full_join(df_pos)%>%
  filter(word!="user")%>%
  mutate(
    freq_pos=positive/sum(positive,na.rm=TRUE)*100,
    freq_neg=negative/sum(negative,na.rm=TRUE)*100,
    gap=freq_pos-freq_neg
  )%>%
  mutate(eng=case_when(
    # mostly positive
    word=="መልካም"~"good",
    word=="እግዚአብሔር"~"god",
    word=="በሰላም"~"peacefully",
    word=="አሜን"~"amen",
    word=="ደስ"~"happy",
    word=="በዓል"~"holiday",
    word=="ጋር"~"with",
    word=="እንኳን"~"even",
    word=="ሰላም"~"hello",
    word=="ኢትዮጵያ"~"ethiopia",
    # mostly negative
    word=="ምን"~"what",
    word=="ህዝብ"~"people",
    word=="ብሎ"~"said",
    word=="እንዴ"~"sure",
    word=="መንግስት"~"government",
    word=="አሁን"~"right now",
    word=="ብሎ"~"is sit",
    word=="እኮ"~"after all",
    word=="አንተ"~"hey you",
    word=="አይነት"~"sort of",
    word=="ነበር"~"has been",
    ))

ps<-clean%>%drop_na()%>%filter(gap>0)%>%
  arrange(freq_pos)%>%
  mutate(id=row_number())

ng<-clean%>%drop_na()%>%filter(gap<0)%>%
  arrange(freq_neg)%>%
  mutate(id=row_number())


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

p2b<-ggplot(ps)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_pos),fill=col_ps)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=1.8-freq_neg,xmax=1.8),fill=col_ng)+
  geom_text(aes(y=id,x=-0.075,label=eng),size=7,family="open")+
  geom_richtext(
    data=leg_pos,
    aes(x=x,y=y,label=label),hjust=0.5,
    size=11,family="open",color=col_ps,lineheight=0.45,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  coord_polar(start=pi/6)+
  scale_y_continuous(limits=c(-4,10.5))+
  scale_x_continuous(limits=c(-0.1,1.85))+
  theme_void()

p2b

p1b<-ggplot(ng)+
  geom_segment(aes(x=-0.1,xend=1.85,y=id,yend=id),alpha=0.05)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_neg),fill=col_ng)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=1.8-freq_pos,xmax=1.8),fill=col_ps)+
  geom_text(aes(y=id,x=0-0.075,label=eng),size=7,family="open",color="black")+
  geom_richtext(
    data=leg_neg,
    aes(x=x,y=y,label=label),hjust=0.5,
    size=11,family="open",color=col_ng,lineheight=0.45,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  coord_polar(start=pi+pi/6)+
  scale_y_continuous(limits=c(-4,10.5))+
  scale_x_continuous(limits=c(-0.1,1.85))+
  theme_void()
p1b


afri_map <- ggplot()+
  geom_sf(africa,mapping=aes(geometry=geometry),fill=alpha("#26547C",0.5),color=alpha("white",0.25))+
  geom_sf(africa%>%filter(ADMIN=="Ethiopia"),mapping=aes(geometry=geometry),fill="#FF7D00",color=alpha("white",0.25))+
  theme_void()

design <- c(
  area(1, 1, 100,100),
  area(1, 1, 100,100),
  area(41,41,65,65)
)

p1b + p2b + afri_map + plot_layout(design = design)&
  theme(plot.background = element_rect(fill=NA,color=NA))

p1b + p2b + afri_map + plot_layout(design = design) +
plot_annotation(
    title = 'Ethiopian Language Sentiment',
    subtitle = 'bars show **frequency of word** for tweets **written in Amaharic**',
    caption = '**Data** @shmuhammad2004 | **Plot** @BjnNowak',
    theme = theme(
      plot.title = element_markdown(size=60,hjust=0.5,family="title",face="bold",margin=margin(0.5,0,0,0,"cm")),
      plot.subtitle = element_markdown(size=35,hjust=0.5,family="open",margin=margin(0.5,0,0,0,"cm")),
      plot.caption = element_markdown(size=25,hjust=0.5,family="open",margin=margin(0,0,0.1,0,"cm"))
    ))

# Now some highlights

gov<-ggplot(ng)+
  geom_segment(aes(x=-0.1,xend=1.85,y=id,yend=id),alpha=0.025)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_neg),fill=col_ng_lg)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=1.8-freq_pos,xmax=1.8),fill=col_ps_lg)+
  geom_text(aes(y=id,x=0-0.075,label=eng),size=7,family="open",color="grey80")+
  geom_rect(
    data=ng%>%filter(eng=="government"),
    aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_neg),fill=col_ng
  )+
  geom_rect(
    data=ng%>%filter(eng=="government"),
    aes(ymin=id-0.25,ymax=id+0.25,xmin=1.8-freq_pos,xmax=1.8),fill=col_ps
  )+
  geom_text(
    data=ng%>%filter(eng=="government"),
    aes(y=id,x=0-0.075,label=eng),size=7,family="open",color="black"
  )+
  coord_polar(start=pi+pi/6)+
  scale_y_continuous(limits=c(-4,10.5))+
  scale_x_continuous(limits=c(-0.1,1.85))+
  theme_void()
gov

eth<-ggplot(ps)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_pos),fill=col_ps_lg)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=1.8-freq_neg,xmax=1.8),fill=col_ng_lg)+
  geom_text(aes(y=id,x=-0.075,label=eng),size=7,family="open",color="grey80")+

  geom_rect(
    data=ps%>%filter(eng=="ethiopia"),
    aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_pos),fill=col_ps
  )+
  geom_rect(
    data=ps%>%filter(eng=="ethiopia"),
    aes(ymin=id-0.25,ymax=id+0.25,xmin=1.8-freq_neg,xmax=1.8),fill=col_ng
  )+
  geom_text(
    data=ps%>%filter(eng=="ethiopia"),
    aes(y=id,x=-0.075,label=eng),size=7,family="open",color="black"
  )+
  
  coord_polar(start=pi/6)+
  scale_y_continuous(limits=c(-4,10.5))+
  scale_x_continuous(limits=c(-0.1,1.85))+
  theme_void()

design2 <- c(
  area(1, 1, 100,100),
  area(1, 1, 100,100)
)

gov+eth+plot_layout(design = design2)&
  theme(plot.background = element_rect(fill=NA,color=NA))

peo<-ggplot(ng)+
  geom_segment(aes(x=-0.1,xend=1.85,y=id,yend=id),alpha=0.025)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_neg),fill=col_ng_lg)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=1.8-freq_pos,xmax=1.8),fill=col_ps_lg)+
  geom_text(aes(y=id,x=0-0.075,label=eng),size=7,family="open",color="grey80")+
  geom_rect(
    data=ng%>%filter(eng=="what"|eng=="people"|eng=="said"),
    aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_neg),fill=col_ng
  )+
  geom_rect(
    data=ng%>%filter(eng=="what"|eng=="people"|eng=="said"),
    aes(ymin=id-0.25,ymax=id+0.25,xmin=1.8-freq_pos,xmax=1.8),fill=col_ps
  )+
  geom_text(
    data=ng%>%filter(eng=="what"|eng=="people"|eng=="said"),
    aes(y=id,x=0-0.075,label=eng),size=7,family="open",color="black"
  )+
  coord_polar(start=pi+pi/6)+
  scale_y_continuous(limits=c(-4,10.5))+
  scale_x_continuous(limits=c(-0.1,1.85))+
  theme_void()
peo

god<-ggplot(ps)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_pos),fill=col_ps_lg)+
  geom_rect(aes(ymin=id-0.25,ymax=id+0.25,xmin=1.8-freq_neg,xmax=1.8),fill=col_ng_lg)+
  geom_text(aes(y=id,x=-0.075,label=eng),size=7,family="open",color="grey80")+
  
  geom_rect(
    data=ps%>%filter(eng=="god"|eng=="amen"),
    aes(ymin=id-0.25,ymax=id+0.25,xmin=0,xmax=freq_pos),fill=col_ps
  )+
  geom_rect(
    data=ps%>%filter(eng=="god"|eng=="amen"),
    aes(ymin=id-0.25,ymax=id+0.25,xmin=1.8-freq_neg,xmax=1.8),fill=col_ng
  )+
  geom_text(
    data=ps%>%filter(eng=="god"|eng=="amen"),
    aes(y=id,x=-0.075,label=eng),size=7,family="open",color="black"
  )+
  
  coord_polar(start=pi/6)+
  scale_y_continuous(limits=c(-4,10.5))+
  scale_x_continuous(limits=c(-0.1,1.85))+
  theme_void()

design2 <- c(
  area(1, 1, 100,100),
  area(1, 1, 100,100)
)

peo+god+plot_layout(design = design2)&
  theme(plot.background = element_rect(fill=NA,color=NA))

