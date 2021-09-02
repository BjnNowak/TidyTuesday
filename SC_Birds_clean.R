

# Clear space 
rm(list=ls())
gc()

# Load extensions
library(tidyverse)
library(plyr)
library(cowplot)
library(showtext)
font_add_google(name = "Oswald", family = "Oswald")
font_add_google(name = "Hepta Slab", family = "Hepta")
showtext_auto()

tit<-"Hepta"
tex<-"Oswald"

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

br <- plyr::ddply(
  data,
  .(bird_type,bioregions,urban_rural),
  summarize,
  nb=sum(bird_count)
)

top <- plyr::ddply(
  data,
  .(bird_type),
  summarize,
  nb=sum(bird_count)
  )%>%
  arrange(nb) %>%
  tail(10)%>%
  pull(bird_type)

aus <- plyr::ddply(
  data,
  .(bird_type),
  summarize,
  nb=sum(bird_count)
  )%>%
  filter(bird_type %in% top)

bio <- plyr::ddply(
  data,
  .(bird_type,bioregions),
  summarize,
  nb=sum(bird_count)
)%>% arrange(bioregions, -nb)

reg <- levels(as.factor(bio$bioregions))

# Top 3 birds per bioregion
# (could be simplify with for loop...)
bio1 <- br %>%
  filter(bioregions==reg[1])%>%
  filter(bird_type %in% 
    (plyr::ddply(
      filter(data,bioregions==reg[1]),
      .(bird_type),
      summarize,
      nb=sum(bird_count)
    )%>%
    arrange(nb) %>%
    tail(3)%>%
    pull(bird_type))
  )

bio2 <- br %>%
  filter(bioregions==reg[2])%>%
  filter(bird_type %in% 
           (plyr::ddply(
             filter(data,bioregions==reg[2]),
             .(bird_type),
             summarize,
             nb=sum(bird_count)
           )%>%
             arrange(nb) %>%
             tail(3)%>%
             pull(bird_type))
  )

bio3 <- br %>%
  filter(bioregions==reg[3])%>%
  filter(bird_type %in% 
           (plyr::ddply(
             filter(data,bioregions==reg[3]),
             .(bird_type),
             summarize,
             nb=sum(bird_count)
           )%>%
             arrange(nb) %>%
             tail(3)%>%
             pull(bird_type))
  )

bio4 <- br %>%
  filter(bioregions==reg[4])%>%
  filter(bird_type %in% 
           (plyr::ddply(
             filter(data,bioregions==reg[4]),
             .(bird_type),
             summarize,
             nb=sum(bird_count)
           )%>%
             arrange(nb) %>%
             tail(3)%>%
             pull(bird_type))
  )

bio5 <- br %>%
  filter(bioregions==reg[5])%>%
  filter(bird_type %in% 
           (plyr::ddply(
             filter(data,bioregions==reg[5]),
             .(bird_type),
             summarize,
             nb=sum(bird_count)
           )%>%
             arrange(nb) %>%
             tail(3)%>%
             pull(bird_type))
  )

bio6 <- br %>%
  filter(bioregions==reg[6])%>%
  filter(bird_type %in% 
           (plyr::ddply(
             filter(data,bioregions==reg[6]),
             .(bird_type),
             summarize,
             nb=sum(bird_count)
           )%>%
             arrange(nb) %>%
             tail(3)%>%
             pull(bird_type))
  )

bio7 <- br %>%
  filter(bioregions==reg[7])%>%
  filter(bird_type %in% 
           (plyr::ddply(
             filter(data,bioregions==reg[7]),
             .(bird_type),
             summarize,
             nb=sum(bird_count)
           )%>%
             arrange(nb) %>%
             tail(3)%>%
             pull(bird_type))
  )

bio8 <- br %>%
  filter(bioregions==reg[8])%>%
  filter(bird_type %in% 
           (plyr::ddply(
             filter(data,bioregions==reg[8]),
             .(bird_type),
             summarize,
             nb=sum(bird_count)
           )%>%
             arrange(nb) %>%
             tail(3)%>%
             pull(bird_type))
  )

bio9 <- br %>%
  filter(bioregions==reg[9])%>%
  filter(bird_type %in% 
           (plyr::ddply(
             filter(data,bioregions==reg[9]),
             .(bird_type),
             summarize,
             nb=sum(bird_count)
           )%>%
             arrange(nb) %>%
             tail(3)%>%
             pull(bird_type))
  )

bio10 <- br %>%
  filter(bioregions==reg[10])%>%
  filter(bird_type %in% 
           (plyr::ddply(
             filter(data,bioregions==reg[10]),
             .(bird_type),
             summarize,
             nb=sum(bird_count)
           )%>%
             arrange(nb) %>%
             tail(3)%>%
             pull(bird_type))
  )

whole <- rbind.data.frame(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10)
dim(whole)
whole$conc<-paste(whole$bioregions,whole$bird_type)

whole<-whole %>% arrange(bioregions, nb)

empty_bar <- 1
to_add <- data.frame( matrix(NA, empty_bar*nlevels(
  as.factor(whole$bioregions)), ncol(whole)) )
to_add
colnames(to_add) <- colnames(whole)
to_add$nb<-0
to_add$urban_rural<-"Urban"
to_add$conc <- rep(levels(as.factor(whole$bioregions)), each=empty_bar)
to_add
data <- rbind(whole, to_add)
data <- data %>% arrange(bioregions, nb)
#data$conc <- seq(1, nrow(data))
data

# Create labels
label_data = ddply(
  data,
  .(conc,bird_type),
  summarize,
  tot=sum(nb)
)
label_data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (seq(1:number_of_bar)-0.5) /number_of_bar     
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Create regions
reg_name<-cbind.data.frame(
  reg=reg,
  short=c('BBS','FLB','NNC','NSS','SCP','SEH','SEQ','SYB','VIM','SVP'),
  x=seq(3,41,4),
  start=seq(1.5,37.5,4),
  end=seq(4.5,40.5,4)
)
reg_name

# Make the plot
p <- ggplot() +       
  geom_bar(
    data=data,
    aes(x=as.factor(conc), y=nb, alpha=as.factor(urban_rural), fill=as.factor(bird_type)),
    stat="identity") +
  ylim(-75,200) +

  geom_segment(
    data=reg_name,
    aes(x=start,xend=end),y=-10,yend=-10,
    colour = "black", alpha=1, size=0.5 , inherit.aes = FALSE ) + 
  geom_text(data=reg_name,aes(x=x,label=short),y=-25,size=12,family=tex)+
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     
  ) +
  
  coord_polar(start = 0)+
  guides(
    alpha=FALSE,
    fill=FALSE
  )+
  scale_alpha_manual(values=c(0.5,1))+
  geom_text(data=label_data, aes(
    x=conc, y=tot+2, label=bird_type, hjust=hjust, colour=factor(bird_type)), 
    color="black", fontface="italic",alpha=0.6, size=10, angle= label_data$angle,
    family=tex
  )

library(jpeg)
img<-readJPEG("bird2.jpg")
library(png)
flag=readPNG("australian_flag.png")

library(grid)
rect=rectGrob(x=0.57,y=0.47,width=0.11,height=0.1,
              gp = gpar(fill = "skyblue2", alpha = 0.5)
)
  
ggdraw() +
  draw_image(img,x=0.1,y=0,width=0.8,height=0.8)+
  draw_image(flag,x=0.45,y=0.4,width=0.2,height=0.2)+
  draw_grob(rect)+
  draw_plot(p, x = -0.2, y = 0.4, width = 1, height = 0.6)+
  draw_text(
    text = "Top 3 most common bird species",  
    size = 60,
    family=tit,
    hjust=0,color="#343a40",
    x = 0.45, y = 0.92)+
draw_text(
  text = "per Australian bioregions",  
  size = 60,
  family=tit,
  hjust=0,color="#343a40",
  x = 0.45, y = 0.87)+
  draw_text(
    text = "Ranking based on number of sights in urban (dark colored)",  
    size = 50,
    family=tex,
    hjust=0,color="#343a40",
    x = 0.45, y = 0.80)+
  draw_text(
    text = "or rural (light colored) areas for two years",  
    size = 50,
    family=tex,
    hjust=0,color="#343a40",
    x = 0.45, y = 0.75)+
  draw_text(
    text = "in a row (2014 and 2015)",  
    size = 50,
    family=tex,
    hjust=0,color="#343a40",
    x = 0.45, y = 0.70)+
  draw_text(
    text = "Data & Map: Cleary et al. (2016)",  
    size = 40,
    family=tex,
    hjust=0,color="#343a40",
    x = 0.15, y = 0.42)

