
library(tidyverse)
library(waffle)
library(ggtext)
library(glue)
library(showtext)
library(camcorder)
library(grid)
library(cowplot)

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21, 
  height = 21*1.6, 
  units = "cm", 
  dpi = 300 
)

# Load fonts
font_add_google("Roboto Condensed","roboto")
font_add_google("Oswald","oswald")
font_add_google("Anton","anton")
font_add_google("Bebas Neue","bebas")
font_add_google("Francois One","fran")
font_add_google("Acme","acme")
font_add_google("Staatliches","staat")
# Automatically use {showtext} for plots
showtext_auto()

# Load tables straight from my git:
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

finish<-ranking_full%>%
  mutate(finish=case_when(
    is.na(Rank)~FALSE,
    TRUE~TRUE
  ))%>%
  # Keep only finishers
  filter(finish==TRUE)
  #filter(year==2019|year==2021)

# Qucik exploration of finishres per race
expl<-finish%>%
  mutate(ct=1)%>%
  group_by(Race)%>%
  summarize(finishers=sum(ct))

# Select some races
data<-finish%>%
  filter(
    (str_detect(Race,'Western States Endurance Run')&(year==2021))|
    (str_detect(Race,'UTMB')&(year==2021))|
    (Race=='La Diagonale Des Fous'&(year==2019))|
    (Race=='Leadville Trial 100 Run'&(year==2019))
    #(Race=='HARDROCK 100'&(year==2018))
  )%>%
  mutate(ct=1)

# Keep only hour to make waffle plot
hours <- str_split_fixed(data$Time, ":", 3)[,1]

# Add some combinations to have only complete cases
# (z added as a dummy variable so all combinations are available for all races)
clean<-data%>%
  add_column(hr=hours)%>%
  group_by(Gender,hr,Race)%>%
  summarise(val=sum(ct))%>%
  ungroup()%>%
  add_row(Gender='M',Race='Western States Endurance Run',hr='15',val=0)%>%
  complete(Gender,Race,hr,fill=list(val=0))%>%
  add_row(Gender='Z',Race='Western States Endurance Run',hr='15',val=1)%>%
  complete(Gender,Race,hr,fill=list(val=1))%>%
  drop_na()

# Make main plot
################

# To add hours as facet in ggplot
facet_labeller <- function(var){
  c('',
    '15',
    '','','','',
    '20',
    '','','','',
    '25',
    '','','','',
    '30',
    '','','','',
    '35',
    '','','','',
    '40',
    '','','','',
    '45',
    '','','','',
    '50',
    '','','','',
    '55',
    '','','','',
    '60',
    '','','','',
    '65',
    ''
  )
}

# Set fill and color palette
# (alpha = 0 to hide dummy z variable)
col_men<-'#00a091'
col_women<-'#f18f01'

pal <- c(
  'M' = col_men,
  'W' = col_women,
  'Z' = alpha('black', 0)
)

pal_col<- c(
  'M'='white',
  'W'='white',
  'Z'=alpha('black',0)
)

# Order race names
clean$Race <- as.factor(clean$Race)
clean$Race<-fct_relevel(clean$Race,"Western States Endurance Run",'Leadville Trial 100 Run', 'UTMB®','La Diagonale Des Fous')

# Make plot
whole<-ggplot(
  clean,
  aes(fill=Gender,color=Gender,values=val))+
  geom_waffle(size=0.2,n_rows=3,flip=TRUE)+
  facet_grid(
    Race~hr,
    switch = "x",
    labeller=labeller(hr=as_labeller(facet_labeller))
  )+
  scale_x_discrete()+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=pal_col)+
  coord_equal()+
  labs(
    x= "Finishing time (hours)"
  )+
  theme_minimal()+
  theme(
    plot.margin = margin(0,1,0,1,"cm"),
    plot.background = element_rect(fill=NA,color=NA),
    panel.spacing.x = unit(-0.05,'cm'),
    panel.grid=element_blank(),
    strip.text.x=element_text(size=30,family='oswald',color='grey40',hjust=0.5),
    strip.text.y=element_blank(),
    axis.text = element_blank(),
    axis.title.x=element_text(size=45,family='roboto',face='bold',margin=margin(0, 0,0.5 , 0, "cm"))
  )+
  guides(fill='none',color='none')

# Now with labels and background
################################

# Prep labels
name_races <- tibble(
  name = c('Western States 2021','Leadville 2019','UTMB 2021','Diagonale des Fous 2019'),
  x=rep(0.07,4),
  y=c(0.89,0.69,0.48,0.27)
)

details <- tibble(
  lab = c(
    'Olympic Valley, California, USA\nElevation gain: 5,600m', 'Leadville, Colorado, USA\nElevation gain: 4,800m', 'Chamonix, France\nElevation gain: 10,000m', 'Saint-Piere, La Réunion, France\nElevation gain: 10,210m')
)

# Prep background colors
col_day1 <- '#FDFFD6'
col_day2 <- '#FAFF97'
col_day3 <- '#F7FF58'

x1 <- 21/4.175
x2 <- 21/2.825

rect1 <- rectGrob(
  x = unit(0, "cm"),
  y = unit(0, "cm"),
  width = unit(x1, "cm"),
  height = unit(21*1.6, "cm"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = col_day1, alpha = 1,col=NA)
)

rect2 <- rectGrob(
  x = unit(x1, "cm"),
  y = unit(0, "cm"),
  width = unit(21-x2-x1, "cm"),
  height = unit(21*1.6, "cm"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = col_day2, alpha = 1,col=NA)
)

rect3 <- rectGrob(
  x = unit(21, "cm"),
  y = unit(0, "cm"),
  width = unit(x2, "cm"),
  height = unit(21*1.6, "cm"),
  hjust = 1, vjust = 0,
  gp = gpar(fill = col_day3, alpha = 1,col=NA)
)

ggdraw()+
  draw_grob(rect1)+
  draw_grob(rect2)+
  draw_grob(rect3)+
  draw_plot(whole,y=0.02,height=0.95)+
  draw_text(
    "How long to\nrun 100 miles?", 
    x=0.92,y=name_races$y[1],
    hjust=1,vjust=1,lineheight=0.35,
    color = "black", size = 110, 
    family = 'anton')+
  draw_text(
    "One, two or three days.", 
    x=0.92,y=name_races$y[1]-0.1,
    hjust=1,vjust=1,lineheight=0.35,
    color = "black", size = 90, 
    family = 'oswald')+
  draw_text(
"The most iconic races of ultratrail running\n
are 100 miles races (about 161 km). In this\n
chart, each dot represents one finisher for\n
four of these races. Generally organized in\n
mountainous areas, ultratrail running races\n
have important elevation changes and\n
finishing times depend on the elevation \n
gain and type of trails of each race. 
\n
On flat roads, world\n
record for the distance\n
is 10h51min and belongs\n
to Aleksandr Sorokin.\n", 
    x=0.92,y=name_races$y[1]-0.15,
    hjust=1,vjust=1,lineheight=0.20,
    color = "black", size = 50, 
    family = 'roboto')+
  draw_text(
"Data: ITRA\n
Plot: @BjnNowak", 
    x=0.92,y=0.41,
    hjust=1,vjust=1,lineheight=0.20,
    color = "black", size = 40,alpha=0.8, 
    family = 'roboto')+
  draw_text(
    name_races$name, 
    x=name_races$x,y=name_races$y,
    hjust=0,vjust=1,lineheight=0.35,
    color = "black", size = 60, 
    family = 'fran',fontface='bold')+
  draw_text(
    "Women", 
    x=0.56,y=0.525,
    hjust=1,vjust=1,lineheight=0.35,
    color = col_women, size = 55, 
    family = 'oswald',fontface='bold')+
  draw_text(
    "Men", 
    x=0.56,y=0.49,
    hjust=1,vjust=1,lineheight=0.35,
    color = col_men, size = 55, 
    family = 'oswald',fontface='bold')+
  draw_text(
    details$lab, 
    x=name_races$x,y=name_races$y-0.025,
    hjust=0,vjust=1,lineheight=0.35,
    color = "black", size = 40, 
    family = 'roboto')+
  draw_text(
    c('Day 1','Day 2','Day 3'), 
    x=c(0.13,0.44,0.81),y=0.95,
    hjust=0.5,vjust=1,lineheight=0.35,
    color = "#54494B", 
    size=100,alpha=0.2,
    family = 'staat',fontface='bold')+
  draw_line(x=c(0.1975,0.1975),y=c(0.3175,0.365),size=0.5,color="#54494B",alpha=0.8)+
  draw_text(
"Courtney Dauwalter
22h30 (Women race record)", 
    x=0.1975,y=0.375,
    hjust=0,vjust=0,lineheight=0.35,
    color = col_women, size = 35, 
    family = 'oswald')+
  draw_line(x=c(0.0572,0.0572),y=c(0.743,0.77),size=0.5,color="#54494B",alpha=0.8)+
  draw_text(
"Jim Walmsley
14h46 (Third win in the race)", 
      x=0.0572,y=0.78,
      hjust=0,vjust=0,lineheight=0.35,
      color = col_men, size = 35, 
      family = 'oswald')+
  
  draw_text(
    "Hot and wet climate of Reunion Island\nis an additional difficulty\nfor this race", 
    x=name_races$x[4],y=name_races$y[4]-0.085,
    hjust=0,vjust=1,lineheight=0.35,
    color = "black", size = 40,fontface='italic', 
    family = 'roboto')


