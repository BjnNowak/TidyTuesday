library(tidyverse)
library(patchwork)
library(camcorder)
library(showtext)
library(ggtext)
library(glue)


# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Roboto","rob")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 6.75*2, 
  height = 24, 
  units = "cm", 
  dpi = 600 
)

holiday_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-19/holiday_episodes.csv')
holiday_episode_genres <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-19/holiday_episode_genres.csv')

sm<-holiday_episodes%>%
  mutate(ct=1)%>%
  group_by(parent_original_title)%>%
  summarize(sm=sum(ct))%>%
  arrange(-sm)

sub<-sm%>%pull(parent_original_title)

scr<-holiday_episodes%>%filter(parent_original_title%in%sub[1:40])

holiday_episodes_one<-holiday_episodes%>%
  group_by(parent_original_title,year)%>%
  summarise(
    average_rating=mean(na.omit(average_rating)),
    parent_average_rating=mean(na.omit(parent_average_rating))
  )%>%
  ungroup()%>%
  mutate(
    gap=parent_average_rating-average_rating,
    gap_cl=case_when(
      gap<0~'below',
      gap==0~'same',
      gap>0~'above'
    )
  )

max(scr$average_rating)
min(scr$average_rating)

max(scr$year)
min(scr$year)

pal<-c(
  "below"="#7BDFF2",
  'same'='white',
  'above'='#F9DC5C'
)

col_avr <- '#A39594'

fun_plot<-function(i){

data<-holiday_episodes_one%>%filter(parent_original_title%in%sub[i])

mx_year<-max(na.omit(data$year))
mn_year<-min(na.omit(data$year))

show_average<-data$parent_average_rating[1]


pl<-ggplot(holiday_episodes,aes(x=year,y=average_rating))+
  geom_jitter(alpha=0.01,color="white")+
  
  geom_segment(
    holiday_episodes_one%>%filter(parent_original_title%in%sub[i]),
    mapping=aes(x=year,xend=year,y=show_average,yend=average_rating,color=gap_cl),
    lwd=0.5
  )+
  annotate(geom='segment',x=mn_year-0.2,xend=mx_year+0.2,y=show_average,yend=show_average,color=col_avr,lwd=0.5)+
  geom_point(
    holiday_episodes_one%>%filter(parent_original_title%in%sub[i]),
    mapping=aes(x=year,y=average_rating,color=gap_cl),
    size=0.75
  )+
  #annotate('text',x=mn_year,y=show_average+0.05,label=sub[i],hjust=0,vjust=0,size=2,alpha=0.5)+
  scale_y_continuous(limits=c(2,10))+
  scale_x_continuous(limits=c(1983,2023))+
  scale_color_manual(values=pal)+
  guides(color='none')+
  labs(title=sub[i])+
  theme_void()+
  theme(
    plot.title = element_text(size=50,family="bit",face='bold',color="white")
    #plot.background = element_rect(fill="#0B3866",color="#0B3866")
  )

return(pl)

}

fun_plot(1)+fun_plot(11)+
  fun_plot(18)+fun_plot(5)+
  fun_plot(6)+fun_plot(7)+
  fun_plot(35)+fun_plot(19)+
  fun_plot(10)+fun_plot(30)+
  plot_layout(ncol = 2)&
  theme(plot.background = element_rect(fill=NA,color=NA))

