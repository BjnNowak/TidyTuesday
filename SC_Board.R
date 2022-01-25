
library(tidyverse)
library(ggforce)
library(ggtext)
library(camcorder)
library(showtext)

# Load fonts 
font_add_google("Mouse Memoirs","mouse")
font_add_google("Yanone Kaffeesatz","yanone")
font_add_google("Wendy One","wendy")
font_add_google("PT Sans Narrow","narrow")
# Automatically use {showtext} for plots
showtext_auto()

# Set plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10*1.618, 
  units = "cm", 
  dpi = 300 
)

# Load data
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

# Clean data
data <- ratings%>%
  left_join(details,by=c("id"))%>%
  filter(users_rated>1000)%>%
  filter(name==primary)%>%
  select(name,bayes_average,boardgamecategory,users_rated,owned)%>%
  drop_na()%>%
  mutate(cat=case_when(
    str_detect(boardgamecategory,"Abstract Strategy")~'abstract',
    str_detect(boardgamecategory,"Dice")~'dice',
    str_detect(boardgamecategory,"Action")~'action',
    str_detect(boardgamecategory,"Card Game")~'card',
    TRUE~'other'
  ))%>%
  group_by(cat)%>%
  slice_max(bayes_average,n=3)%>%
  ungroup()%>%
  mutate(
    max_own=max(owned)+20000,
    off = owned/max_own
  )%>%
  mutate(nr=case_when(
    cat=='abstract'~5,
    cat=='action'~4,
    cat=='card'~3,
    cat=='dice'~2,
    TRUE~1
  ))%>%
  arrange(cat, -bayes_average) %>%
  mutate(name=case_when(
    name=="Through the Ages: A New Story of Civilization"~"Through the Ages:<br>A New Story of Civilization",
    TRUE~name
  ))%>%
  group_by(cat) %>% 
  mutate(nc = rank(-bayes_average, ties.method = "first"))%>%
  # Create coordinates
  mutate(
    x0=nc+0.5,
    y0=nr+0.5,
    r=off/2,
    ax=nc,
    ay=nr
  )%>%
  mutate(name_lab = glue::glue("**{nc}.** {name}"))%>%
  ungroup()

# Create background
chess_1 <- tibble(
    rw=seq(1,5,1),
    cl=1
  )%>%
  mutate(cell=paste(rw,cl))%>%
  mutate(back=case_when(
    row_number() %% 2 == 0 ~ "white",
    TRUE ~ "black"
  ))%>%
  mutate(
    ax=cl,
    bx=cl,
    cx=cl+1,
    dx=cl+1,
    ay=rw,
    by=rw+1,
    cy=rw+1,
    dy=rw,
  )

chess_2 <- tibble(
  rw=seq(1,5,1),
  cl=2
)%>%
  mutate(cell=paste(rw,cl))%>%
  mutate(back=case_when(
    row_number() %% 2 == 1 ~ "white",
    TRUE ~ "black"
  ))%>%
  mutate(
    ax=cl,
    bx=cl,
    cx=cl+1,
    dx=cl+1,
    ay=rw,
    by=rw+1,
    cy=rw+1,
    dy=rw,
  )

chess_3 <- tibble(
  rw=seq(1,5,1),
  cl=3
)%>%
  mutate(cell=paste(rw,cl))%>%
  mutate(back=case_when(
    row_number() %% 2 == 0 ~ "white",
    TRUE ~ "black"
  ))%>%
  mutate(
    ax=cl,
    bx=cl,
    cx=cl+1,
    dx=cl+1,
    ay=rw,
    by=rw+1,
    cy=rw+1,
    dy=rw,
  )

chess <- chess_1%>%
  bind_rows(chess_2)%>%
  bind_rows(chess_3)

# Pivot to long
chess_shp<-chess%>%
  select(
    back,cell,
    ax,bx,cx,dx,
    ay,by,cy,dy
  )%>%
  pivot_longer(
    !c(back,cell),
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )

# Create labels for categories
lab <- tibble(
  rw=c(5.5,4.5,3.5,2.5,1.5),
  cl=0,
  lab=c(
    "Abstract\nstrategy",
    "Action and\ndexterity",
    "Card\ngame",
    "Dice",
    '0ther'
  )
)

# To add annotations
rate <- tibble(
  x=c(1.5,3.65,2.5,2.5),
  y=c(1.5,4.65,3.5,4.65),
  lab=c(
    "**Highest**<br>**rate**<br>(8.5)",
    "**Lowest**<br>**rate**<br>(7)",
    "**Most owned**<br>(111,275 players)",
    "**Least owned**<br>(9,649 players)"
  ),
  color=c("white","black","white","black")
)

# Custom caption
caption <- tibble(
  x=4,
  y=0.90,
  lab="**Data:** Board Game Geek **| Plot:** @BjnNowak ",
  color="black"
)

# Make plot

#Set colors

chess_col1 <- "#95ecb0"
chess_col2 <- "#f3f98a"

red_pal <- c(
  "#E6B3B3",
  "#D68587",
  "#C7575A",
  "#A8383C",
  "#7A292C",
  "#4C191B"
)

pal <-c(
  "white" = "#FAF0F0",
  "black" = "grey10"
)

# Plot

ggplot(data=data,aes(group=name))+
  geom_shape(
    data=chess_shp%>%filter(back=="black"),
    aes(x=x,y=y,group=cell),
    fill=chess_col1,
    inherit.aes = FALSE
  )+
  geom_shape(
    data=chess_shp%>%filter(back=="white"),
    aes(x=x,y=y,group=cell),
    fill=chess_col2,
    inherit.aes = FALSE
  )+
  geom_circle(
    aes(x0=x0,y0=y0,r=r,fill=bayes_average),
    color=NA
  )+
  geom_richtext(
    aes(x=ax+0.05,y=ay+0.05,label=name_lab),
    hjust=0,vjust=0,lineheight=0.3,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt"), 
    size=6,
    family = 'yanone'
  )+
  geom_richtext(
    data=rate,
    aes(x=x,y=y,label=lab,color=color),
    hjust=0.5,vjust=0.5,lineheight=0.3,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt"), 
    size=6,
    inherit.aes = FALSE,
    family = 'narrow'
  )+
  geom_richtext(
    data=caption,
    aes(x=x,y=y,label=lab,color=color),
    hjust=1,vjust=1,lineheight=0.3,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt"), 
    size=6,
    inherit.aes = FALSE,
    family = 'narrow'
  )+
  geom_text(
    data=lab,
    aes(y=rw,label=lab),x=0.3,
    family="mouse",
    hjust=0.5,vjust=0.5,lineheight=0.3,
    inherit.aes = FALSE,size=12,face='bold'
  )+
  annotate(
    geom = 'text',x=2,y=6.65,
    label="Let's play!",
    hjust=0.5,vjust=1,size=20,family='wendy',fontface='bold'
  )+
  annotate(
    geom = 'text',x=-0.10,y=6.35,
    label="This graph shows the Top 3 board games by category, according to players' rating on Board Game Geek\n
database. Color of each pawn indicates rating, size is related to the number of players who own the game.",
    hjust=0,vjust=1,size=8,family='yanone',lineheight=0.18
  )+
  coord_equal()+
  scale_x_continuous(limits=c(-0.5,4))+
  scale_y_continuous(limits=c(0.90,6.65))+
  scale_fill_gradientn(colours=red_pal)+
  scale_color_manual(values=pal)+
  guides(
    fill='none',
    color="none")+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#FAF0F0",color=NA)
  )


