# Load libraries
library(tidyverse)
library(camcorder)
library(ggtext)
library(showtext)
library(glue)

# Load fonts
font_add_google("Oswald","oswald")
font_add_google("Abril Fatface","abril")
font_add_google("Balsamiq Sans","balsamiq")
font_add_google("Lobster","lobster")
font_add_google("Anton","anton")
font_add_google("Pacifico","pacifico")
font_add_google("Roboto Slab","slab")
font_add_google("Permanent Marker","marker")
font_add_google("Secular One","secular")
# Automatically use {showtext} for plots
showtext_auto()

# Set fonts
fts<-tibble(fts=c(
  "oswald","abril",'lobster',
  'anton','pacifico','slab',
  'balsamiq','marker','secular'))

# Set colors
clr<-tibble(cl=c(
  "#f94144","#f3722c","#f8961e","#f9c74f",
  "#90be6d","#43aa8b","#577590"
))

# Load data : all Spice Girl lyrics
lyrics<-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/lyrics.csv')


# Get all words associations in Spice girls lyrics
test<-str_replace_all(lyrics$line, "[\\[\\{}(),!?]", "")%>%
  str_to_lower()%>%
  str_split(" ")

ly<-tibble(
  from=character(),
  to=character()
)

for (i in 1:length(test)){

  for (j in 1:(length(test[[i]])-1)){
  
    temp<-tibble(
      from=test[[i]][j],
      to=test[[i]][j+1]
    )
  
    ly<-ly%>%
      bind_rows(temp)
  
}}

ly<-ly%>%
  drop_na()

# Create new song!

# Choose first word
word <- 'baby'

new <- tibble(
  new=rep(word,60)
)

i=1


for(i in 1:59){
  if (new$new[i] %in% ly$from){
    nw<-ly%>%
      dplyr::filter(from==new$new[i])%>%
      slice_sample()%>%
      pull(to)
    #new<-new%>%
    #  bind_rows(tibble(new=nw))
    new$new[i+1]<-nw
  } else {
    new$new[i+1]<-word
    #new<-new%>%
    #  bind_rows(tibble(new=nw))
  }
}

# "Draw" song

col_draw<-tibble(cl=character())

for(i in 1:60){

  col_draw<-col_draw%>%
    bind_rows(clr%>%slice_sample())
  
}


song <- new%>%
  bind_cols(col_draw)



sg<-song%>%
  mutate(name=glue("<span style='color:{cl};'>{new}</span>"))%>%
  mutate(y=rev(c(
    rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),
    rep(6,6),rep(7,6),rep(8,6),rep(9,6),rep(10,6)
  )))%>%
  mutate(
    x=rep(seq(1,6,1),10),
    hjust=rep(c(0,0,0,0,0,0,1,1,1,1,1,1),5))

row<-tibble(row=as.character())
fts_draw<-tibble(fts=character())

for(i in 1:10){
  temp<-sg%>%
    filter(y==i)%>%
    pull(name)
  row<-row%>%
    bind_rows(
      tibble(row=paste(temp,collapse=" "))
    )
  
  fts_draw<-fts_draw%>%
    bind_rows(fts%>%slice_sample())
}


data<-tibble(
    x=0,
    y=seq(1,10,1)
  )%>%
  bind_cols(row)%>%
  bind_cols(fts_draw)


gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

ggplot(data=data)+
  geom_richtext(
    aes(x=x,y=y,label=row,family=fts),
    size=12,lineheight = 0.1,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding# remove padding
  )+
  theme_void()+
  theme(
    plot.margin = margin(t = 0.5, r = 1, b = 0.5, l = 1, unit = "cm"),
  )


