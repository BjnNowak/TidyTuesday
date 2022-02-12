
# Set plot size
library(camcorder)
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10*1.618, 
  units = "cm", 
  dpi = 300 
)

# Colors
back<-"#fde4d0"
bl<-"#161411"

library(tidyverse)

# Load data
airmen <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv'
)

# Prepare data
data<-airmen%>%
  separate(graduation_date, c("year", "month","day"))%>%
  mutate(ct=1)%>%
  group_by(year)%>%
  summarize(grad=sum(ct))%>%
  ungroup()%>%
  mutate(year=as.numeric(year))


# Offsets for x/y-axis
xoff <- 10
yoff <- 0.16
# 1st year of y-axis
bottom_year <- 1939

# Coords for vertical polygon
vertical<-data%>%
  mutate(
    ax=grad-xoff,
    bx=grad-xoff,
    cx=grad+xoff,
    dx=grad+xoff,
    ay=bottom_year,
    by=year-yoff,
    cy=year+yoff,
    dy=bottom_year
  )%>%
  # Keep only requested columns
  select(year,ax,bx,cx,dx,ay,by,cy,dy)

# Pivot from wide to long 
vertical_long <- vertical %>%
  pivot_longer(
    !year,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )

# Load ggforce 
library(ggforce)

# Make first plot
pl <- ggplot()+
  ggforce::geom_shape(
    data=vertical_long,
    aes(x=x,y=y,group=year),
    fill="black",color="black"
  )+
  theme_minimal()
pl

# Coords for horizontal polygon
horizontal<-data%>%
  mutate(
    ax=-xoff,
    bx=grad+xoff,
    cx=grad-xoff,
    dx=-xoff,
    ay=year+yoff,
    by=year+yoff,
    cy=year-yoff,
    dy=year-yoff
  )%>%
  select(year,ax,bx,cx,dx,ay,by,cy,dy)

# Pivot from wide to long
horizontal_long <- horizontal %>%
  pivot_longer(
    !year,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
  )

# Add horizontal polygon to plot
pl<-pl+ggforce::geom_shape(
  data=horizontal_long,
  aes(x=x,y=y,group=year),
  fill="white",color="black"
)
pl

# Layout customization       
pl+
  scale_x_continuous(
    breaks=c(10,63,241,369)
  )+
  scale_y_continuous(
    breaks=c(1942,1943,1944,1945,1946,1948)
  )+
  theme(
    axis.title=element_blank(),
    axis.text.x = element_text(margin=margin(0,0,8,0)),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )
