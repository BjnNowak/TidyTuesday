
library(tidyverse)
library(plyr)
library(stringr)
library(camcorder)
library(showtext)

font_add_google(name = "Graduate", family = "Graduate")
font_add_google(name = "Lobster", family = "Lobster")
font_add_google(name = "Ubuntu", family = "Ubuntu")
showtext_auto()

tit<-"Graduate"
tex<-"Ubuntu"

athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')

data<-athletes%>%
  filter(type=="Athletics")%>%
  filter(medal=="Gold")%>%
  filter(str_detect(event,"100 m"))%>%
  filter(grp_id=="1"|is.na(grp_id))      # Count only one medal for relay runs

team_ranking<-plyr::ddply(
  data,
  .(abb),
  summarize,
  nb_medal=length(year)
  )%>%
  arrange(nb_medal)%>%
  tail(8)%>%
  pull(abb)

team_ranking_whole<-plyr::ddply(
  data,
  .(abb),
  summarize,
  nb_medal=length(year)
)%>%
  arrange(nb_medal)%>%
  tail(8)%>%
  mutate(xpos=seq(1.5,8.5,1))

team_medals<-plyr::ddply(
  data,
  .(abb,gender),
  summarize,
  nb_medal=length(year)
  )%>%
  mutate(
    most_teams=case_when(
      abb%in%team_ranking~abb,
      TRUE~'Others'
    )
  )%>%
  filter(most_teams!='Others')%>%
  mutate(clean_rank = fct_recode(abb,
    "Italy" = "ITA",
    "Spain" = "ESP",
    "Germany" = "FRG",
    "China" = "CHN",
    "Great Britain" = "GBR",
    "Canada" = "CAN",
    "Australia" = "AUS",
    "USA" = "USA"
  ))

team_medals$clean_rank<-fct_relevel(
  team_medals$clean_rank, 
  "Italy",
  "Spain",
  "Germany",
  "China",
  "Great Britain",
  "Canada",
  "Australia",
  "USA"
)

# Tartan color:
tartan<-"#BE524B"
lines<-"#EBFAF2"

# Gender colors
#pal_gender<-c("#909ED4","#6C9270")
pal_gender<-c("#0E79B2","#4CB944")

# Text color
text_col<-"#1E1014"


theme_tracksNfields<-theme(
  panel.background = element_rect(
    fill=tartan
  ),
  panel.grid = element_line(
    color=lines
  ),
  axis.text=element_blank(),
  axis.title=element_blank(),
  axis.ticks=element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)
 
country_names<-cbind.data.frame(
  label = levels(team_medals$clean_rank),
  ypos= 2,
  xpos = seq(1.5,8.5,1)
)
  
trk_nb <-cbind.data.frame(
  label = rev(seq(1,8,1)),
  ypos= -2.5,
  xpos = seq(1.5,8.5,1)
)


ggplot()+
  scale_fill_manual(
    values = pal_gender
  )+
  geom_col(
    data=team_medals,
    aes(y=nb_medal,x=as.numeric(clean_rank)+0.5,fill=gender),
    width=0.7
    #position="stack"
    #position_nudge(y = 0.5)
    )+
  expand_limits(y = -2.7)+
  scale_x_continuous(
    name = "Country",
    #limits = range(as.numeric(team_medals$clean_rank)),
    limits = c(1,9),
    breaks = unique(sort(as.numeric(team_medals$clean_rank))),
    labels = levels(team_medals$clean_rank)
  )+
  coord_flip()+
  geom_text(
    data=country_names,aes(x=xpos,y=ypos,label=label),
    hjust=0,alpha=1,color=lines,
    family=tit, size=15)+
  geom_text(
    data=trk_nb,aes(x=xpos,y=ypos,label=label),
    hjust=0.5,alpha=1,color=lines,
    family=tit, size=20,angle=270)+
  geom_text(
    data=team_ranking_whole,aes(y=nb_medal+1,x=xpos,label=nb_medal),
    hjust=0,alpha=1,color=lines,
    family=tex, size=15)+
  annotate(
    geom='segment',
    x=-Inf,xend=Inf,
    y=0,yend=0,color=lines
  )+
  annotate(
    geom='segment',
    x=9,xend=9,
    y=-Inf,yend=Inf,color=lines
    
  )+
  annotate(
  'text',
    x=4.5,
    y=45,label="Queens",family="Lobster",color=pal_gender[2],
    size=30,hjust=1
  )+
  annotate(
    'text',
    x=4.5,
    y=47.5,label="&",family="Lobster",color=lines,
    size=25,hjust=1
  )+
  annotate(
    'text',
    x=4.5,
    y=55,label="Kings",family="Lobster",color=pal_gender[1],
    size=30,hjust=1
  )+
  annotate(
    'text',
    x=3.5,
    y=55,label="of the straight line",family="Lobster",color=lines,
    size=25,hjust=1
  )+
  annotate(
    'text',
    x=2.5,
    y=55,label="Number of gold medals for 100m at the paralympics",family=tex,color=lines,
    size=14,hjust=1
  )+
  annotate(
    'text',
    x=1.5,
    y=55,label="Data: International Paralympic Committee | Plot: @BjnNowak",family=tex,color=lines,
    size=12,hjust=1
  )+
  guides(
    fill = FALSE
  )+
  theme_tracksNfields
