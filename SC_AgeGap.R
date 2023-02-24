library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)


# Set fonts
font_add_google("Open Sans","open")
font_add_google("Hammersmith One","title")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 30, 
  height = 15, 
  units = "cm", 
  dpi = 300 
)

# Load data
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

# Clean data
clean<-age_gaps%>%
  filter(
    (character_1_gender=="man"&character_2_gender=="woman")|(character_2_gender=="man"&character_1_gender=="woman")
  )%>%
  mutate(
    man_age = case_when(
      character_1_gender=="man"~actor_1_age,
      character_2_gender=="man"~actor_2_age
    ),
    woman_age = case_when(
      character_1_gender=="woman"~actor_1_age,
      character_2_gender=="woman"~actor_2_age
    ),
    man_gap = man_age-woman_age
  )


man_res_age<-clean%>%
  group_by(man_age)%>%
  drop_na()%>%
  summarize(
    mn=mean(man_gap),
    sd=sd(man_gap)
  )%>%
  ungroup()%>%
  mutate(wn=man_age-mn,wn_round=round(wn))%>%
  mutate(label=glue::glue("**{man_age}** <span style='color:#f4711f;'>*{wn_round}*</span>"))

woman_res_age<-clean%>%
  group_by(woman_age)%>%
  drop_na()%>%
  summarize(
    mn=mean(-man_gap),
    sd=sd(-man_gap)
  )%>%
  ungroup()%>%
  mutate(wn=woman_age-mn,wn_round=round(wn))%>%
  filter(wn_round!=79)%>%
  mutate(label=glue::glue("**{woman_age}** <span style='color:#638b95;'>*{wn_round}*</span>"))

min_age <- min(min(clean$woman_age),min(clean$man_age))
max_age <- max(max(clean$woman_age),max(clean$man_age))

# Make plots
man_title <- tibble(
  y=-15,
  label = c("**the age of an actor** <span style='color:#f4711f;'>*vs the age of the actress he is dating*</span>")
)

woman_title <- tibble(
  y=-15,
  label = c("**the age of an actress** <span style='color:#638b95;'>*vs the age of the actor she is dating*</span>")
)

col_man <- "#638b95"
col_woman <- "#f4711f"

man<-ggplot(clean,aes(x=woman_age,y=-man_age))+
  geom_richtext(
    data=man_title,
    aes(x=12,y=y,label=label),hjust=0,
    size=12,family="open",color=col_man,lineheight=0.45,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    data=man_res_age,
    aes(x=12,y=-man_age,label=label),hjust=0,
    size=7,family="open",color=col_man,lineheight=0.45,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  
  geom_jitter(alpha=0.05)+
  geom_segment(
    man_res_age,
    mapping=aes(x=wn,xend=man_age,y=-man_age,yend=-man_age),
    color=col_woman,size=0.75,
    linewidth=0.5,inherit.aes=FALSE)+
  annotate("segment",x=17,xend=60,y=-17,yend=-60,color="#AC7E5A")+
  geom_point(
    man_res_age,
    mapping=aes(x=wn,y=-man_age),
    size=3,pch=21,color=col_woman,
    fill="#E4EAD7",inherit.aes=FALSE
  )+
  scale_x_continuous(limits=c(12,60))+
  scale_y_continuous(limits=c(-60,-15))+
  coord_fixed()+
  theme_void()+
  theme(
    plot.margin=margin(0,0,0,4,"cm"),
    plot.title = element_markdown(family="fira",size=30),
    plot.title.position = "panel"
  )

woman<-ggplot(clean,aes(x=man_age,y=-woman_age))+
  geom_richtext(
    data=woman_title,
    aes(x=12,y=y,label=label),hjust=0,
    size=12,family="open",color=col_woman,lineheight=0.45,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    data=woman_res_age,
    aes(x=12,y=-woman_age,label=label),hjust=0,
    size=7,family="open",color=col_woman,lineheight=0.45,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
 
  geom_jitter(alpha=0.05)+
  geom_segment(
    woman_res_age,
    mapping=aes(x=wn,xend=woman_age,y=-woman_age,yend=-woman_age),
    color=col_man,size=0.75,
    linewidth=0.5,inherit.aes=FALSE)+
  annotate("segment",x=17,xend=60,y=-17,yend=-60,color="#AC7E5A")+
  geom_point(
    woman_res_age,
    mapping=aes(x=wn,y=-woman_age),
    size=3,pch=21,color=col_man,
    fill="#E4EAD7",inherit.aes=FALSE
  )+
  scale_x_continuous(limits=c(12,60))+
  scale_y_continuous(limits=c(-60,-15))+
  coord_fixed()+
  theme_void()+
  theme(
    plot.margin=margin(0,4,0,0,"cm"),
    plot.title = element_markdown(family="fira",size=30),
    plot.title.position = "panel"
  )


# Assemble
woman+man+ plot_annotation(
  title = 'Hollywood Age Gap',
  subtitle = 'Age gap between women and men for **movie couples**',
  caption = '**Data** Data Is Plural | **Plot** @BjnNowak',
  theme = theme(
    plot.title = element_markdown(size=60,hjust=0,family="title",face="bold",margin=margin(0,0,0.2,0.5,"cm")),
    plot.subtitle = element_markdown(size=35,hjust=0,family="open",margin=margin(0,0,0.1,0.5,"cm")),
    plot.caption = element_markdown(size=25,hjust=0,family="open",margin=margin(0,0,0.1,0.5,"cm"))
  )
)&
  theme(
    plot.background = element_rect(fill="#F6F8F2",color=NA),
    plot.margin=margin(0,1,0,1,"cm"),
  )
