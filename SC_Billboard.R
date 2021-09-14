# Clear space 
###############
rm(list=ls())
gc()

# Load data
billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')
features <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

# Load packages
library(tidyverse)
library(broom)
library(ggtext)
library(glue)
library(showtext)
library(camcorder)
library(patchwork)
library(ggrepel)

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21*1.618, 
  height = 21, 
  #width = 10*1.618, 
  #height = 10, 
  units = "cm", 
  dpi = 300 
)

# Load fonts
font_add_google("Acme", "acme")
font_add_google("Raleway", "raleway")
font_add_google("Open Condensed", "open")
font_add_google("Cabin Condensed","cabin")
font_add_google("Barlow Condensed","barlow")
font_add_google("Stint Ultra Condensed","stint")
font_add_google("Poppins","poppins")
font_add_google("Pacifico","pacifico")
font_add_google("Oswald","oswald")

# Automatically use {showtext} for plots
showtext_auto()


# Add decade
bill_dec<-billboard%>%
  mutate(year=format(
    as.Date(week_id,"%m/%d/%Y"),format="%Y"))%>%
  mutate(decade=case_when(
    year<1970~1960,
    year<1980~1970,
    year<1990~1980,
    year<2000~1990,
    year<2010~2000,
    TRUE~2010
  ))

# Max weeks on chart
bill_top<-billboard%>%
  group_by(song_id)%>%
  summarize(max=max(weeks_on_chart))

# Unique song
bill_clean<-bill_dec%>%
  filter(
    (weeks_on_chart==1)&(instance==1)
  )%>%
  left_join(bill_top,by="song_id")

# duration removed coz no effect
PCA_prep<-features%>%
  select(
    c(song_id,danceability,energy,instrumentalness,key,acousticness,
      mode,valence,tempo,time_signature,speechiness,loudness,liveness
      #spotify_track_duration_ms
    )
  )%>%
  drop_na()

# PCA
PCA <-PCA_prep%>%
  select(-song_id)%>%
  prcomp(scale = TRUE)

# Show eigenvalues
PCA%>%
  tidy(matrix = "eigenvalues")

# Individual plots
PCA_indiv<-PCA%>%
  broom::augment(PCA_prep)%>%
  left_join(bill_clean,by='song_id')

# Keep only a few songs
keepSongs=c(
  "Unchained Melody","Why Me", "You And Me", "Radioactive",
  "What About Me","Need You Now","Before He Cheats", "I'm So Excited",
  "I'm Yours","Sail","Rockin' Around The Christmas Tree", "Red Red Wine",
  "How Deep Is Your Love", "Somebody That I Used To Know", "Smooth", "I Don't Want To Wait", 
  "The Twist", "Blinding Lights", "Bohemian Rhapsody", "Use Someboby",
  "With You I'm Born Again", "Ho Hey", "Another Ones Bites The Dust",
  "Disco Inferno", "1999", "Celebration", "Gloria","Rolling In The Deep",
  "In My Dreams", "Wipe Out", "All Of Me", "Truly Madly Deeply", "Use Somebody")

top3<- PCA_indiv%>%
  group_by(decade)%>% 
  slice_max(order_by = max, n = 10)%>%
  mutate(
    fullName = glue("<b style='font-family: cabin;'>{song}</b><br> <i style='font-family: open'>({performer})</i> ")
  )%>%
  ungroup()%>%
  distinct(song,.keep_all=TRUE)%>%
  filter(song %in% keepSongs)


sub <- cbind.data.frame(
  x = -5, y = 3.45,
  label = glue("<span>Principal component analysis of songs' features from </span><b style='color:#E76F51'> 1960s </b><span>to</span><b style='color:#43AA8B'> 2010s </b><span>decades.<br>Each dot stands for a song, with title and performer for some popular tracks.</span> ")
)

feat <- cbind.data.frame(
  x= -5, 
  y=seq(-0.75,-0.75-11*0.20,-0.2),
  label = c(
    glue("<b style='font-family:barlow'>Acousticness:</b><span style='font-family:cabin'> 1 represents high confidence the track is acoustic</span>"),
    glue("<b style='font-family:barlow'>Danceability:</b><span style='font-family:cabin'> How suitable a track is for dancing based on a combination of elements</span>"),
    glue("<b style='font-family:barlow'>Energy:</b><span style='font-family:cabin'> Perceptual measure of intensity and activity</span>"),
    glue("<b style='font-family:barlow'>Instrumentalness:</b><span style='font-family:cabin'> Higher instrumentalness indicates lower vocal content</span>"),
    glue("<b style='font-family:barlow'>Key:</b><span style='font-family:cabin'> Key using pitch class notation (e.g. 0=C, 2=D and so on) </span>"),
    glue("<b style='font-family:barlow'>Liveness:</b><span style='font-family:cabin'> Higher liveness means higher probability that the track was performed live</span>"),
    glue("<b style='font-family:barlow'>Loudness:</b><span style='font-family:cabin'> Overall loudness of a track in decibels</span>"),
    glue("<b style='font-family:barlow'>Mode:</b><span style='font-family:cabin'> Modality of a track (0 for minor, 1 for major)</span>"),
    glue("<b style='font-family:barlow'>Speechiness:</b><span style='font-family:cabin'> Frequence of spoken words in a track</span>"),
    glue("<b style='font-family:barlow'>Tempo:</b><span style='font-family:cabin'> Pace in beats per minute</span>"),
    glue("<b style='font-family:barlow'>Time signature:</b><span style='font-family:cabin'> How many beats are in each bar (or measure)</span>"),
    glue("<b style='font-family:barlow'>Valence:</b><span style='font-family:cabin'> Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric)</span>")
))

col_pal <- c("#E76F51","#CC795B","#B08364","#958D6E","#7A9678","#5EA081","#43AA8B","white")

arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(4, "pt")
)

arrow_style_ind <- arrow(
  angle = 45, 
  length = grid::unit(8, "pt")
)

indiv<-ggplot(PCA_indiv,aes(.fittedPC1, .fittedPC2,color=as.factor(decade))) +
  geom_point(alpha=0.15,size=1)+
  scale_color_manual(values=col_pal)+
  scale_x_continuous(limits=c(-5,4))+
  scale_y_continuous(limits=c(-3,4))+
  annotate(
    geom="segment",x=-5,xend=4,y=0,yend=0,
    lty="dashed",
    col="grey60",
    size=0.5)+
  annotate(
    geom="segment",x=3.9,xend=4,y=0,yend=0,
    col="grey60",
    size=0.5,arrow = arrow_style_ind, lineend='round',linejoin='round')+
  annotate(
    geom="text",x=3,y=0.15,label="Principal component 1 (22%)",
    col="dimgrey",size=10,family="open",fontface="italic")+
  annotate(
    geom="text",x=-0.1,y=-3,label="Principal component 2 (11%)",
    col="dimgrey",size=10,family="open",angle=90,vjust=0,hjust=0,fontface="italic")+
  annotate(
    geom="segment",x=0,xend=0,y=-3,yend=4,col="grey60",lty="dashed",
    size=0.5)+
  annotate(
    geom="segment",x=0,xend=0,y=3.9,yend=4,col="grey60",
    size=0.5,arrow = arrow_style_ind, lineend='round',linejoin='round')+
  annotate(
    geom="text",x=-5,y=4,vjust=1,hjust=0,
    label="Billboard hits are getting louder",size=30,color='#203B46',
    family="pacifico")+
  annotate(
    geom="text",x=3.8,y=1.5,hjust=1,label="Most recent hits are\n'loud' and 'energetic'",
    size=16,family="cabin",lineheight=0.4
  )+
  annotate(
    geom="text",x=3.8,y=-3,hjust=1,color="dimgrey",
    label="Data: data.world",
    size=13,family="cabin",lineheight=0.4
  )+
  geom_richtext(
    data=sub,
    aes(x=x,y=y,label=label),
    inherit.aes = FALSE,lineheight=0.5,
    alpha=1,size=17,family='cabin',hjust=0,vjust=1,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+ 
  geom_richtext(
    data=feat,
    aes(x=x,y=y,label=label),
    inherit.aes = FALSE,
    alpha=1,size=10,
    lineheight=0.4,
    hjust=0,fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  annotate(
    geom="text",x=-5,y=-0.4,hjust=0,vjust=1,
    label="Features (based on Spotify API)",
    family="cabin",fontface='bold',
    size=15
  )+

  geom_richtext(
    data=top3,aes(label=fullName),size=10,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt"),
    lineheight=0.4
  )+
  theme_minimal()+
  guides(
    color=FALSE
  )+

  theme(
    panel.grid=element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
  )


# Variable plot

PCA_var<-PCA %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value")%>%
  mutate(column=stringr::str_to_title(column))%>%
  mutate(column=stringr::str_replace_all(column,"_"," "))

var<-ggplot(data=PCA_var,aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style,size=0.2,color="dimgrey") +
  geom_text_repel(
    aes(label = column),
    size=10,
    hjust = 1, 
    nudge_x = -0.01,
    nudge_y = -0.01,
    min.segment.length = Inf,
    color = "grey30",family="barlow"
  ) +
  annotate(
    geom="segment",x=0,xend=0,y=-Inf,yend=Inf,col="grey60",lty="dashed",
    size=0.5)+
  annotate(
    geom="segment",x=Inf,xend=-Inf,y=0,yend=0,col="grey60",lty="dashed",
    size=0.5)+
  coord_fixed()+
  theme_minimal()+
  theme(
    panel.grid=element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
  )

# Make plot with patchwork
layout=c(
  area(t=1,l=1,b=10,r=10),
  area(t=1.5,l=8,b=3.5,r=10)
)
indiv+var+plot_layout(design=layout)



