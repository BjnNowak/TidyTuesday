library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 15, 
  units = "cm", 
  dpi = 300 
)

# Fonts
font_add_google("Open Sans","open")
font_add_google("Bebas Neue","bebas")
font_add_google("League Spartan","spart")

showtext_auto()

# Load data
#bioc <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv')
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

# Select pkg
pk <- c(
  'ggplot2','dplyr','tidyverse',
  'purrr','forcats','magrittr',
  'lubridate','tidytuesdayR',
  'tibble','stringr','readr')

# Prep data
data<-cran%>%
  filter(package %in% pk)%>%
  mutate(
    ct=case_when(
      package=='stringr'~1,
      package=='ggplot2'~2,
      package=='lubridate'~3,
      package=='dplyr'~4,
      package=='magrittr'~5,
      package=='readr'~6,
      package=='purrr'~7,
      package=='tibble'~8,
      package=='forcats'~9,
      package=='tidyverse'~10,
      package=='tidytuesdayR'~11
  ))%>%
  mutate(date_simpl=as.Date(date, "%Y-%m-%d"))%>%
  drop_na()

# Prep labels for pk names
lab_pk <- data%>%
  group_by(package)%>%
  summarize(
    ct=mean(ct),
    date_min=as.Date(min(date_simpl),"%Y-%m-%d")
  )

# Prep lab for x axis 
year_lab <- as.Date(as.character(seq(2010,2022,2)),'%Y')

caption <- tibble(
  x=as.Date('2009','%Y'),
  y=7.5,
  lab="**Data:** R. Flight **| Plot:** @BjnNowak ",
  color="black"
)

# Make plot
ggplot(data,aes(x=date_simpl,y=ct))+
  geom_point(color='#11D0CC',alpha=0.7)+
  geom_text(
    data=lab_pk,aes(x=date_min,y=ct+0.3,label=package),
    hjust=0,inherit.aes = FALSE,family='spart',size=15,color='#EF745C')+
  scale_x_date(
    date_labels = "%Y",
    limits=c(as.Date('2009','%Y'),as.Date('2023','%Y')),
    breaks=year_lab
  )+
  annotate(
    geom = 'text',x=as.Date('2009','%Y'),y=11.5,
    label="The Evolution\nof the Tidyverse",size=25,
    color='#EF745C',
    family='bebas',
    hjust=0,vjust=1,lineheight=0.35
  )+
  annotate(
    geom = 'text',x=as.Date('2009','%Y'),y=9.5,
    label=
"Each point shows the release\n
of a new version of one of the\n
few packages represented here",
    size=10,
    family='open',color='#FFF8E8',
    hjust=0,vjust=1,lineheight=0.25
  )+
  geom_richtext(
    caption,mapping=aes(x=x,y=y,label=lab),
    inherit.aes = FALSE,
    size=9, family='open',hjust=0,lineheight=0.45,vjust=1,
    color='#FFF8E8',
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  theme_minimal()+
  theme(
    plot.margin = margin(1,1,1,1, unit = "cm"),
    plot.background = element_rect(fill="#070E0C",color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y=element_blank(),
    axis.text.x = element_text(size=40,family='open',color='#FFF8E8'),
    axis.text.y=element_blank()
  )

