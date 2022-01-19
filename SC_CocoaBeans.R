
library(tidyverse)
library(ggforce)
library(camcorder)
library(showtext)

# Load fonts 
font_add_google("Barlow Condensed","barlow")
font_add_google("Archivo Narrow","arch")
font_add_google("Kreon","kreon")
# Automatically use {showtext} for plots
showtext_auto()

# Set plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10*1.618, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Load data
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')
yield <- readr::read_csv('https://raw.githubusercontent.com/BjnNowak/TidyTuesday/main/data/CocoaYields.csv')

# Clean chocoloate data
data<-chocolate%>%
  mutate(cocoa_num=readr::parse_number(cocoa_percent))%>%
  filter(cocoa_num>79)%>%
  filter(country_of_bean_origin!="Blend")

# Clean yield data
yield_sub<-yield%>%
  mutate(Area=case_when(
    Area=="Bolivia (Plurinational State of)"~'Bolivia',
    Area=="Saint Lucia"~'St. Lucia',
    Area=="Trinidad and Tobago"~'Trinidad',
    Area=='United States of America'~'U.S.A.',
    Area=='Venezuela (Bolivarian Republic of)'~'Venezuela',
    Area=='Viet Nam'~'Vietnam',
    Area=="United Republic of Tanzania"~'Tanzania',
    TRUE~Area
  ))%>%
  filter(Element=="Yield")

# Merge both table
pl<-data%>%
  mutate(Area=country_of_bean_origin)%>%
  mutate(Year=review_date)%>%
  left_join(yield_sub)
  mutate(Area=case_when(
    Area %in% keep ~ Area,
    TRUE~'other'
  ))

# Compute mean stats
pl_sum<-pl%>%
  filter(Value<10000)%>%
  group_by(Area)%>%
  summarize(
    mean_rate = mean(rating),
    sd_rate = sd(rating),
    mean_yield = mean(Value),
    sd_yield = sd(Value)
  )%>%
  mutate(
    mean_yield_kg = mean_yield/10
  )
  

# Compute required surface for a 100g bar / 90% chocolate

pl_surf<-pl_sum%>%
  mutate(surf=(0.09*10000)/mean_yield_kg)%>%
  mutate(side=sqrt(surf)/3)%>%
  # Create coordinates
  mutate(
    ax=mean_rate,
    bx=mean_rate,
    cx=mean_rate+2*side,
    dx=mean_rate+2*side,
    ay=0,
    by=2*side,
    cy=2*side,
    dy=0
  )

# Pivot to long
pl_shp<-pl_surf%>%
  select(
    Area,
    ax,bx,cx,dx,
    ay,by,cy,dy
  )%>%
  pivot_longer(
    !Area,
    names_to = c("point", ".value"),
    names_pattern = "(.)(.)"
)



# Make plot

keep <- c(
  'Colombia','Madagascar','Uganda','Congo'
)

keep2 <- c(
  'Ecuador','Ghana','Fiji'
)

pal<- c(
  'Colombia' = '#2EC4B6',
  'Madagascar' = '#E71D36',
  'Uganda' = '#FF9F1C',
  'Congo' = '#461220',
  'Ecuador' = '#E71D36',
  #'Ghana' = '#306B34',
  'Ghana' = '#582707',
  'Fiji' = '#006BA6'
)
#2EC4B6

pl_shp$Area<-fct_relevel(pl_shp$Area, "Uganda",'Congo','Colombia',"Madagascar")

ggplot(
    data=pl_shp%>%filter(Area %in% keep),
    aes(fill=Area))+
  geom_shape(
    aes(x=x,y=y),
    alpha=0.5,
    radius = unit(0.5, 'cm')
  )+
  geom_shape(
    data=pl_shp%>%filter(Area %in% keep2),
    aes(x=x,y=-y),
    alpha=0.5,
    radius = unit(0.5, 'cm')
  )+
  geom_text(
    data=pl_shp%>%filter(Area %in% keep)%>%filter(point=="b"),
    aes(x=x,y=y+0.05,label=Area,color=Area),
    hjust=0, family = 'arch',size=12
  )+
  geom_text(
    data=pl_shp%>%filter(Area %in% keep2)%>%filter(point=="b"),
    aes(x=x,y=-y-0.05,label=Area,color=Area),
    hjust=0, family = 'arch',size=12
  )+
  geom_segment(
    data=pl_shp%>%filter(Area %in% keep)%>%filter(point=="b"),
    aes(
      x=x,xend=x, y=0,yend=0.1
    ),size=1
  )+
  geom_segment(
    data=pl_shp%>%filter(Area %in% keep2)%>%filter(point=="b"),
    aes(
      x=x,xend=x, y=0,yend=-0.1
    ),size=1
  )+
  geom_text(
    data=pl_shp%>%filter(Area %in% keep)%>%filter(point=="b"),
    aes(
      x=x,y=0.15,label=round(x,1)
    ),size=10,family='barlow'
  )+
  geom_text(
    data=pl_shp%>%filter(Area %in% keep2)%>%filter(point=="b"),
    aes(
      x=x,y=-0.15,label=round(x,1)
    ),size=10,family='barlow'
  )+
  annotate(
    geom = 'segment',y=0,yend=0,x=2.5,xend=4.2,
    size = 2, arrow = arrow()
  )+
  annotate(
    geom = 'text',
    x=3.65,y=0.1,size=12,
    label="Mean rating",
    hjust=0,vjust=0.5,family='barlow'
  )+
  annotate(
    geom = 'text',
    x=3.45,y=-1.95,size=12,color="white",
    label=
"With the lowest yield of this panel,\n
it takes about 10m2 of cocoa trees\n
to produce enough cocoa beans\n
to make a chocolate bar in Fiji...\n
\n
But it is among the best you can taste!",
    hjust=0,vjust=0,family='barlow',lineheight=0.15
  )+
  annotate(
    geom = 'text',
    x=2.94,y=0.35,size=12,color="white",
    label=
"It takes only 1m2\n
to make a chocolate\n
bar in Madagascar",
    hjust=0,vjust=0,family='barlow',lineheight=0.15
  )+
  annotate(
    geom = 'text',
    x=3.6,y=1,size=12,color="white",
    label=
"Congo's cocoa beans\n
make the best chocolate bars",
    hjust=0,vjust=0,family='barlow',lineheight=0.15
  )+
  

    annotate(
    geom = 'text',x=2.6,y=1.6,
    label="How much land to make a good chocolate bar?",
    hjust=0,vjust=0.5,size=17,family='kreon',fontface='bold'
  )+
  annotate(
    geom = 'text',x=2.6,y=1.45,
    label=
  "This graph shows the ranking of manufactured chocolate bars according to the country supplying the cocoa beans.\n
Size of each supplier is proportional to the required area to make a 100g chocolate bar (with 90% of cocoa)." ,
    hjust=0,vjust=0.5,size=10,family='barlow',lineheight=0.15
  )+
  annotate(
    geom = 'text',x=2.6,y=-2.2,color="grey20",
    label=
"Data:\n
Flavors of Cacao (Rating)\n
FAO (Cocoa beans yield)\n
Plot: @BjnNowak" ,
    hjust=0,vjust=0,size=8,family='barlow',lineheight=0.15
  )+
  coord_equal()+
  theme_void()+
  guides(
    fill='none',
    color='none'
  )+
  scale_y_continuous(limits=c(-2.2,1.6))+
  scale_color_manual(values=pal)+
  scale_fill_manual(values=pal)+
  theme(
    plot.background = element_rect(fill="#F5FAEF",color=NA)
  )

