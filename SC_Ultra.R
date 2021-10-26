
library(tidyverse)
library(waffle)
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
  units = "cm", 
  dpi = 300 
)

# Load fonts
font_add_google("Roboto Condensed","roboto")
font_add_google("Oswald","oswald")
font_add_google("Anton","anton")

# Automatically use {showtext} for plots
showtext_auto()

# Finally load tables straight from my git:
#ranking <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
ranking<- readr::read_csv('https://raw.githubusercontent.com/BjnNowak/UltraTrailRunning/main/ranking.csv')
features <- readr::read_csv('https://raw.githubusercontent.com/BjnNowak/UltraTrailRunning/main/features.csv')


# Data processing

# If interested, this is how you can correct age in the data 
# (to get age at time of race, not age in 2021)
ranking_full<-ranking%>%
  # Merging features to get date of race
  left_join(features)%>%
  # Get year from date of race
  mutate(year=lubridate::year(Date))%>%
  # Compute AgeAtRace
  mutate(AgeAtRace=Age-(2021-year))%>%
  # Set to NA if age is not available
  mutate(AgeAtRace=case_when(
    (AgeAtRace<=10)&(AgeAtRace>100)~NA_real_,
    TRUE~AgeAtRace)
  )

data<-ranking_full%>%
  mutate(finish=case_when(
    is.na(Rank)~FALSE,
    TRUE~TRUE
  ))%>%
  # Keep only finishers
  filter(finish==TRUE)%>%
  filter(str_detect(Race,'UTMB'))%>%
  filter(year==2021)%>%
  # Add count variable
  mutate(ct=1)

# Keep only hour to make waffle plot
hours <- str_split_fixed(data$Time, ":", 3)[,1]

data<-data%>%
  add_column(hr=hours)%>%
  group_by(Gender,hr)%>%
  summarise(val=sum(ct))

ranking_full%>%
  mutate(finish=case_when(
    is.na(Rank)~FALSE,
    TRUE~TRUE
  ))%>%
  # Keep only finishers
  filter(finish==TRUE)%>%
  filter(str_detect(Race,'UTMB'))%>%
  filter(year==2021)%>%
  # Add count variable
  mutate(ct=1)%>%
  group_by(Gender)%>%
  summarise(val=sum(ct))

facet_labeller <- function(var){
  c('20h',
    '','','','',
    '25h',
    '','','','',
    '30h',
    '','','','',
    '35h',
    '','','','',
    '40h',
    '','','','',
    '45h',
    '',''
    )
}

col_men<-'#00a091'
col_women<-'#f18f01'

p<-ggplot(data,aes(fill=Gender,values=val))+
  geom_waffle(size=1,n_rows=4,color='white',flip=TRUE)+
  facet_wrap(
    ~hr,nrow=1,strip.position = 'bottom',
    labeller=labeller(hr=as_labeller(facet_labeller))
  )+
  scale_x_discrete()+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual(values=c(col_men,col_women))+
  coord_equal()+
  labs(
    x='**Finishing time** (hours)'
  )+
  theme_minimal()+
  theme(
    panel.spacing.x = unit(-0.25,'cm'),
    panel.grid=element_blank(),
    strip.text=element_text(size=60,family='oswald',color='grey40',hjust=0),
    axis.text = element_blank(),
    axis.title.x=element_markdown(size=80,family='roboto',margin=margin(0.5, 0,0.5 , 0, "cm")),
    plot.caption = element_markdown(size=50,family='roboto')
  )+
  guides(fill='none')

p

# Empty plot for title
tit<- ggplot() + 
  theme_void() +
  labs(
    title = "All finishers of UTMB 2021",
    subtitle = 
    "Each dot represents one finisher of the 171km race (10,000m elevation gain) around<br>
    the Mont-Blanc (French Alps). In 2021, <span style='color:#f18f01'>**110 women**</span> and <span style='color:#00a091'>**1,414 men**</span> finish the race.<br>
    <br>
    <span style='color:#00a091'>François D’Haene</span> (*France*) won this race **for the fourth time**, in 20h45min.<br>
    On the women side, <span style='color:#f18f01'>Courtney Dauwalter</span> (*USA*) win the race (seventh overall),<br>
    **breaking the women’s record** in 22h30min.
    <br>
    <br>
    <span style='font-size:80px'>**Plot:** @BjnNowak</span>"
  )+
  theme(
    plot.title=element_markdown(size=100,family='anton',margin=margin(0, 0,0.5 , 0, "cm")),
    plot.subtitle=element_markdown(size=70,family='roboto',lineheight = 0.4),
  )



# Make plot with patchwork
layout=c(
  area(t=1,l=1,b=10,r=10),
  area(t=3,l=1.5,b=6,r=5)
)

p+tit+plot_layout(design=layout)

