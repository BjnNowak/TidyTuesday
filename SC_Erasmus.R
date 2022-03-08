library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21, 
  height = 29.7, 
  units = "cm", 
  dpi = 300 
)

# Fonts
font_add_google("Oswald","oswald")
font_add_google("Open Sans","open")
font_add_google("Bree Serif","bree")
font_add_google("Patua One","patua")
font_add_google("Francois One","fran")
showtext_auto()

# Color
tex <- '#070707'
col_send <- '#FA8334'
col_rec <- '#42047E'

# Load data
erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')
# Country names
iso<-read_delim("https://raw.githubusercontent.com/BjnNowak/TidyTuesday/main/data/iso.csv",delim=';')

# Prep data
send <- erasmus%>%
  mutate(ct=1)%>%
  group_by(sending_country_code,academic_year)%>%
  summarize(sending=sum(na.omit(participants)))%>%
  left_join(iso,by=c("sending_country_code"="code"))

rec <- erasmus%>%
  mutate(ct=1)%>%
  group_by(receiving_country_code,academic_year)%>%
  summarize(receiving=sum(na.omit(participants)))%>%
  left_join(iso,by=c("receiving_country_code"="code"))


data <- send%>%
  left_join(rec)%>%
  mutate(sending2=-sending)%>%
  group_by(country_name)%>%
  mutate(
    receiving_all=sum(na.omit(receiving)),
    sending_all=sum(na.omit(sending))
  )%>%
  filter(
    receiving_all>500
  )%>%
  filter(country_name!='NA')%>%
  ungroup()

resume <- data%>%
  group_by(country_name)%>%
  summarize(
    mean_rec=mean(na.omit(receiving)),
    receiving_all=mean(na.omit(receiving_all)),
    mean_send=mean(na.omit(sending))
  )

# Make plot

tit <- tibble(
  y='Ireland',
  x=-8000,
  label="Student\nexchanges\nin Europe"
)

sub <- tibble(
  y='Ireland',
  x=-8000,
  label=
'Country ranking based on a\n
sample of Erasmus programs.\n
Bars show the annual average\n
for the period, points show\n
the values for each year. '
)

out <- tibble(
  y='Ireland',
  x=-6500,
  label=
    'Outgoing\nstudents'
)

inc <- tibble(
  y='Ireland',
  x=6000,
  label=
    'Incoming\nstudents'
)

# Custom caption
caption <- tibble(
  x=-8000,
  y='Iceland',
  lab="**Data:** Data.Europa **| Plot:** @BjnNowak ",
  color="black"
)

ggplot(data,aes(y=fct_reorder(country_name,receiving_all),alpha=academic_year))+
  geom_text(
    tit,mapping=aes(x=x,label=label),
    y='Ireland',inherit.aes = FALSE,
    size=32, family='patua',hjust=0,lineheight=0.40,vjust=0
  )+
  geom_text(
    sub,mapping=aes(x=x,label=label),
    y='Cyprus',inherit.aes = FALSE,
    size=14, family='open',hjust=0,lineheight=0.25,vjust=1
  )+
  geom_richtext(
    caption,mapping=aes(x=x,label=lab),
    y='Iceland',inherit.aes = FALSE,
    size=12, family='open',hjust=0,lineheight=0.25,vjust=1,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  geom_text(
    out,mapping=aes(x=x,label=label),
    y='Hungary',inherit.aes = FALSE,
    size=20, family='fran',lineheight=0.40,vjust=0.5,col=col_send,hjust=0.5
  )+
  geom_text(
    inc,mapping=aes(x=x,label=label),
    y='Hungary',inherit.aes = FALSE,
    size=20, family='fran',lineheight=0.40,vjust=0.5,col=col_rec,hjust=0.5
  )+
  
  geom_segment(
    data=resume,
    aes(y=fct_reorder(country_name,receiving_all),yend=fct_reorder(country_name,receiving_all),
        x=0,xend=mean_rec
    ),
    size=9,
    col=col_rec,alpha=0.15,
    inherit.aes = FALSE
  )+
  
  geom_segment(
    data=resume,
    aes(y=fct_reorder(country_name,receiving_all),yend=fct_reorder(country_name,receiving_all),
        x=0,xend=-mean_send
    ),
    size=9,
    col=col_send,alpha=0.15,
    inherit.aes = FALSE
  )+
  
  geom_jitter(aes(x=receiving),size=3,height = 0.25,color=col_rec)+
  geom_jitter(aes(x=-sending),size=3,height = 0.25,color=col_send)+
  
  geom_text(
    data=data%>%group_by(country_name)%>%summarize(receiving_all=mean(receiving_all)),
    aes(y=fct_reorder(country_name,receiving_all),label=country_name),
    x=0,color=tex,alpha=0.5,size=15,family='oswald'
  )+
  
  scale_x_continuous(
    limits=c(-8000,8000),
    position = "top",
    breaks=c(-6000,-3000,3000,6000),
    labels=c('6,000','3,000','3,000','6,000')
  )+
  
  guides(
    color='none',
    alpha=guide_legend(
      range=c(0.3, 1),
      override.aes = list(color='black'),
      title='Academic\nyear')
  )+
  
  labs(
    x="Number of students",
    alpha="Academic\nyear"
  )+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#FAF0F0",color=NA),
    axis.text.x = element_text(size=35,family='open'),
    axis.title.x = element_text(size=45,family='fran',face='bold',margin=margin(0.2,0.2,0.2,0.2, unit = "cm")),
    plot.margin = margin(1,1,1,1, unit = "cm"),
    legend.position=c(0.85,0.15),
    legend.text = element_text(family='open',size=35,margin = margin(0,0,0,0, unit = "cm")),
    legend.title = element_text(family='fran',size=45,hjust=0.5,lineheight=0.35,face='bold',margin = margin(0,0,0,0, unit = "cm")),
    legend.spacing.x = unit(0, "cm"),
    legend.spacing.y = unit(0.25, "cm")
  )
