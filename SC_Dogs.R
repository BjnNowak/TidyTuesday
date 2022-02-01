
library(tidyverse)
library(ggforce)
library(ggtext)
library(camcorder)
library(showtext)

# Get the Data
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
#breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank_all.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

# Load fonts 
font_add_google("Barlow Condensed","barlow")
font_add_google("Oswald","oswald")
# Automatically use {showtext} for plots
showtext_auto()

# Set plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10*1.618, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

data<-breed_traits%>%
  #filter(str_detect(Breed,"Labrador"))%>%
  select(
    Breed,
    Family='Affectionate With Family',
    Children='Good With Young Children',
    Dogs='Good With Other Dogs',
    Shedding='Shedding Level',
    Coat='Coat Grooming Frequency',
    Drooling='Drooling Level',
    Strangers='Openness To Strangers',
    Watchdog='Watchdog/Protective Nature',
    Adaptability='Adaptability Level',
    Trainability='Trainability Level',
    Energy='Energy Level',
    Barking='Barking Level',
    Mental='Mental Stimulation Needs'
    )

overall<-rowMeans(data[2:14])

data<-data%>%
  bind_cols(Overall=overall)

rank<-breed_rank_all%>%
  select(br=Breed,Rank='2020 Rank')

data<-data%>%
  bind_cols(rank)


min_grade<-data[1:50,]%>%
  mutate(min_grade=min)

clean<-data%>%
  mutate(
    last_rank=max(Rank),
    new_rank=last_rank-Rank+1
  )%>%
  mutate(
    x0=new_rank,
    y0=Overall-0.1,
    #a0=Overall/2,
    #b0=Overall/25,
    a0=Overall/3,
    b0=Overall/35,
    
    #x1=new_rank-3,
    #y1=Overall+0.1,
    x1=new_rank-1.5,
    y1=Overall+0.05,
    #a1=Family/5,
    #b1=Family/50,
    a1=Family/8,
    b1=Family/80,
    
    #x2=new_rank-1,
    #y2=Overall+0.2,
    x2=new_rank-0.5,
    y2=Overall+0.1,
    a2=Children/8,
    b2=Children/80,
    
    #x3=new_rank+1,
    #y3=Overall+0.2,
    x3=new_rank+0.5,
    y3=Overall+0.1,
    a3=Dogs/8,
    b3=Dogs/80,
    
    #x4=new_rank+3,
    #y4=Overall+0.1,
    x4=new_rank+1.5,
    y4=Overall+0.05,
    a4=Watchdog/8,
    b4=Watchdog/80
    
  )%>%
  filter(
    str_detect(Breed,"Labrador")|
      (Breed=="Bulldogs")|
      str_detect(Breed,"Bernese")|
      #str_detect(Breed,"Schnauzers")|
      str_detect(Breed,"Chihuahuas")|
      str_detect(Breed,"Corso")|
      str_detect(Breed,"Akitas")|
      str_detect(Breed,"Portuguese")
  )%>%
  mutate(round_over=round(Overall,2))%>%
  mutate(
    label=glue::glue(
      "<span>**{Rank}.** {Breed}</span><br>
      *Overall: {round_over}*"
      )
  )

legend_number <- clean%>%
  filter(Breed=="Bulldogs")%>%
  select(
    x0,x1,x2,x3,x4,
    y0,y1,y2,y3,y4)


cap <- tibble(
  x=140,y=4.45,label="**Data:** American Kennel Club **| Plot:** @BjnNowak"
)

ggplot(data=clean)+
  geom_ellipse(
    aes(x0=x0,y0=y0,a=a0,b=b0,angle=0),
    color=NA,fill="black"
  )+
  
  
  geom_richtext(
    aes(x=x0+1, y=y0-0.15, label = label),
    hjust=0,vjust=1,size=8,family='barlow',lineheight=0.4,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  
  geom_ellipse(
    aes(x0=x1,y0=y1,a=a1,b=b1,angle=0),
    color=NA,fill="black"
  )+
  geom_ellipse(
    aes(x0=x2,y0=y2,a=a2,b=b2,angle=0),
    color=NA,fill="black"
  )+
  geom_ellipse(
    aes(x0=x3,y0=y3,a=a3,b=b3,angle=0),
    color=NA,fill="black"
  )+
  geom_ellipse(
    aes(x0=x4,y0=y4,a=a4,b=b4,angle=0),
    color=NA,fill="black"
  )+
  
  geom_text(
    data=legend_number,aes(x=x0,y=y0),
    label="a.",family='barlow',size=8,hjust=0.5,
    vjust=0.5,color="white"
  )+
  geom_text(
    data=legend_number,aes(x=x1-1.5,y=y1+0.1),
    label="b.",family='barlow',size=8,hjust=0
  )+
  geom_text(
    data=legend_number,aes(x=x2-0.75,y=y2+0.1),
    label="c.",family='barlow',size=8,hjust=0
  )+
  geom_text(
    data=legend_number,aes(x=x3,y=y3+0.1),
    label="d.",family='barlow',size=8,hjust=0
  )+
  geom_text(
    data=legend_number,aes(x=x4+0.75,y=y4+0.1),
    label="e.",family='barlow',size=8,hjust=0
  )+
  
  annotate(
    geom="text",x=178,y=2.5,label="Legend",size=11,family='barlow',
    fontface='bold',vjust=1,hjust=0
  )+
  annotate(
    geom="text",x=178,y=2.35,size=9,family='barlow',vjust=1,hjust=0,
    label="a. Overall grade (among 13 traits)"
  )+
  annotate(
    geom="text",x=178,y=2.2,size=9,family='barlow',vjust=1,hjust=0,
    label="b. Affectionate with family"
  )+
  annotate(
    geom="text",x=195,y=2.5,size=9,family='barlow',vjust=1,hjust=0,
    label="c. Good with children"
  )+
  annotate(
    geom="text",x=195,y=2.35,size=9,family='barlow',vjust=1,hjust=0,
    label="d. Good with other dogs"
  )+
  annotate(
    geom="text",x=195,y=2.2,size=9,family='barlow',vjust=1,hjust=0,
    label="e. Watchdog"
  )+
  

  
  annotate("segment", x = 142, xend = 152, y = 2.2, yend = 2.2,
           colour = "black", size = 0.5, arrow = arrow(length=unit(3.5, "mm"))) +
  annotate("segment", x = 142, xend = 142, y = 2.2, yend = 3.0,
           colour = "black", size = 0.5, arrow = arrow(length=unit(3.5, "mm"))) +
  
  annotate(
    geom="text",x=146,y=2.10,size=9,family='barlow',vjust=1,hjust=0.5,
    label="Popularity ranking"
  )+
  
  annotate(
    geom="text",x=140,y=2.5,size=9,family='barlow',vjust=1,hjust=0.5,
    label="Overall grade",angle=90
  )+
  
  annotate(
    geom="text",x=140,y=4.7,size=16,family='oswald',vjust=1,hjust=0,
    label="Popular dog breeds are not always the best rated",fontface="bold"
  )+
  geom_richtext(
    data=cap,
    aes(x=x, y=y, label = label),
    hjust=0,vjust=1,size=9,family='barlow',
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  ) +
  
  
  scale_x_continuous(limits = c(135,205))+
  scale_y_continuous(limits = c(2,5))+
  
  theme_void()+
  theme(
    plot.background = element_rect(fill="white",color="black")
  )

 