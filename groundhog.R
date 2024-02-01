library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)

# Set fonts
font_add_google("Bebas Neue","beb")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Fira Sans","fira")
font_add_google("Jost","jost")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 25, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

predictions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/predictions.csv')

clean <- predictions%>%
  filter(id==1)%>%
  select(-id)%>%
  mutate(shd_cl=case_when(
   shadow==TRUE~'winter',
   shadow==FALSE~'spring',
   TRUE~'no obs'
  ))

dim(clean)

a <- 2
b <- 3
theta <- seq(0,12*pi,(12*pi/dim(clean)[1])/3)
r <- a + b*theta
tib <- tibble(
  x=r*cos(theta), 
  y=r*sin(theta)
)%>%
  mutate(id=row_number())


extr<-tib %>%
  filter(row_number() %% 3 == 1) %>%
  filter(id!=1)%>%
  bind_cols(clean)

extr$year

sub<-seq(1900,2020,20)
sub<-c(1886,2023)

pal<-c(
  'winter'=
    '#6606C6',
    #"#42047e",
  'spring'="#07f49e",
  'no obs'="#999799"
)

ggplot() + 
  geom_path(tib, mapping=aes(x,y),alpha=0.15)+
  geom_point(
    extr,
    mapping=aes(x=x,y=y,color=shd_cl,size=year)
    #alpha=0.5
  )+
  geom_text(
    extr%>%filter(year%in%sub),
    mapping=aes(x=x,y=y,label=year),
    size=16,color="white", family='jost'
  )+
  labs(
    title="**Groundhog Predictions**",
    subtitle=
      "The original Groundhog Day ceremony is held in Punxsutawney (Pennsylvania) since 1886.<br>
    If a groundhog goes out on February 2 and sees its shadow, <b><span style='color:#7A07ED;'>winter will go on for six more weeks</span></b>.<br>
    If it does not, <b><span style='color:#07f49e;'>spring will arrive early</span></b> (grey points for <b><span style='color:#999799;'>years without predictions</span></b>).",
    caption="**Data** groundhog-day.com **| Plot** Benjamin Nowak"
  )+
  guides(color='none',size='none')+
  coord_equal()+
  scale_size(range=c(0.1,12))+
  scale_color_manual(values=pal)+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#0B0A07",color=NA),
    plot.margin = margin(1,0,1,0,'cm'),
    plot.title = element_markdown(size=60,hjust=0.5,color="white",family='ral'),
    plot.subtitle = element_markdown(
      size=35,hjust=0.5, lineheight=.45, family='jost',color="white",
      margin=margin(0.5,0,0.5,0,'cm')),
    plot.caption = element_markdown(
      size=32,hjust=0.5, lineheight=.45, family='jost',color="white",
      margin=margin(0.5,0,0.5,0,'cm'))
  )
  