library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)
library(ggbump)


# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Playfair Display","play")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 16, 
  height = 9, 
  units = "cm", 
  dpi = 300 
)

# Create rank column
country_results_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-24/country_results_df.csv')%>%
  mutate(sm=p1+p2+p3+p4+p5+p6)%>%
  group_by(year)%>%
  arrange(-sm)%>%
  mutate(rank=row_number())
 
# Prepare for plot
yax<-tibble(
  y=seq(-100,-20,20),
  ypos=-y
)%>%
  mutate(
    lab=glue::glue("{ypos}th position")
  )

col_country<-"#f756aa"
col_point<-"#f75672"
col_labs<-"#E2E2E2"

# Make plot for all countries
name=levels(as.factor(country_results_df$country))

for (i in 1:length(name)){

p<-ggplot()+
  annotate(
    geom="segment",
    x=-Inf,xend=Inf,
    y=-1,yend=-1,
    linewidth=0.5,alpha=0.5,color=col_labs
  )+
  annotate(
    geom="text",
    label="Winner",
    x=-Inf, y=3,
    family="ral",size=8,hjust=0,
    alpha=0.5,color=col_labs
  )+
  geom_segment(
    yax,
    mapping=aes(y=y,yend=y,x=-Inf,xend=Inf),
    linewidth=0.25,alpha=0.25,
    color=col_labs
  )+
  geom_text(
    yax,
    mapping=aes(x=-Inf,y=y+4,label=lab),
    alpha=0.25,size=8,family="ral",hjust=0,
    color=col_labs
  )+
  geom_bump(
    country_results_df%>%
      filter(country==name[i])
    ,
    mapping=aes(x=year,y=-rank,group=country),
    color=col_country,size=1,smooth=20
  )+
  geom_point(
    country_results_df%>%
      filter(country==name[i])%>%
      filter(rank==1)
    ,
    mapping=aes(x=year,y=-rank,group=country),
    fill=col_point,size=2,color="white",pch=21
  )+
  guides(color='none')+
  labs(
    title="Country position in International Mathematical Olympiad",
    subtitle=name[i],
    caption="**Data** Havisha Khurana **| Plot** Benjamin Nowak"
  )+
  scale_y_continuous(limits=c(-112,3))+
  scale_x_continuous(limits=c(1959,2024))+
  theme_void()+
  theme(
    plot.margin=margin(0.5,1,0.5,1,"cm"),
    plot.background=element_rect(fill="#001011"),
    plot.title=element_text(size=30,family="bit",face="bold",color=col_labs),
    plot.subtitle=element_text(size=25,family="bit",face="bold",color=col_country),
    plot.caption = element_markdown(size=20,family="fira",color=alpha(col_labs,0.5),hjust=0)
  )

print(p)

}


gg_playback(
  name = file.path(tempdir(), "recording", "vignette_gif.gif"),
  first_image_duration = 5,
  last_image_duration = 5,
  frame_duration = 1,
  image_resize = 2000
)
