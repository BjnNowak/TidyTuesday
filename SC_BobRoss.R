library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Calistoga","cal")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Read in the data
bob_ross <- read_csv(
  "https://raw.githubusercontent.com/jwilber/Bob_Ross_Paintings/master/data/bob_ross_paintings.csv",
) 

clean<-bob_ross%>%
  select(painting_title,season,episode,color_hex)%>%
  mutate(color_hex=str_replace_all(color_hex, "\\[|\\]|\\'", ""))%>%
  tidyr::separate_wider_delim(color_hex,",",names_sep="_",too_few="align_start")

max_season <- max(clean$season)

dt <- clean%>%
  pivot_longer(!c(painting_title,season,episode),names_to="color",values_to="hex")%>%
  drop_na()%>%
  mutate(hex=str_trim(hex))%>%
  group_by(season,episode)%>%
  mutate(
    ct=1,rk=row_number(),rk_min=rk-1,lg=sum(ct),
    xmin=season-1+(rk_min/lg),xmax=season-1+(rk/lg)
  )

xax <- tibble(
  x = seq(0,31,1)
)

yax <- tibble(
  y = seq(1,13,1)
)

leg <- tibble(
  x=c(0),
  y=c(-6),
  label=c("**The sectors** of the circle<br>refer to **the 31 seasons**<br>of the show and **the lines**<br>indicate **the episodes**<br><span style='font-size:35px;color:grey40;'>13 episodes per season</span>")
)

ggplot(dt)+
  geom_segment(
    data=yax,
    aes(x=-0.2,xend=31.2,y=y,yend=y),color="dimgrey",alpha=0.2,
    inherit.aes = FALSE
  )+
  geom_rect(
    aes(ymin=episode-0.40,ymax=episode+0.4,xmin=xmin,xmax=xmax,fill=hex)
  )+
  geom_segment(
    data=xax,
    aes(x=x,xend=x,y=max_ep+1,yend=max_ep+2),color="dimgrey",alpha=0.2,
    inherit.aes = FALSE
  )+
  geom_text(
    data=xax%>%filter(x!=0),
    aes(x=x-0.5,y=max_ep+1.5,label=x),color="dimgrey",family="ral",size=15,
    inherit.aes = FALSE
  )+
  geom_text(
    data=yax,
    aes(x=-0.4,y=y,label=y),color="dimgrey",family="bit",size=9,
    inherit.aes = FALSE
  )+
  geom_richtext(
    data=leg,
    aes(x=x,y=y,label=label),
    size=10,family="fira",color="black",lineheight=0.45,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+

  coord_polar()+
  scale_fill_identity()+
  scale_y_continuous(limits=c(-6,max_ep+2))+
  scale_x_continuous(limits=c(-0.4,31.4))+
  
  labs(
    title = "Bob Ross Color Painting Palette",
    subtitle = "The graph below shows all the colors used in the show",
    caption = "  **Data** Jared Wilber  | **Plot** @bjnnowak"
  )+
  
  theme_void()+
  theme(
    plot.background = element_rect(fill="#FAF1F0", color=NA),
    plot.title = element_text(size=50,family="cal",hjust=0.5,margin=margin(1,0,0,0,"cm"),face="bold"),
    plot.subtitle = element_text(size=35,family="fira",hjust=0.5,color="black",margin=margin(0.25,0,-1,0,"cm")),
    plot.caption = element_markdown(size=35,family="fira",hjust=0.5,color="black",margin=margin(-1,0,1,0,"cm"))
  )

