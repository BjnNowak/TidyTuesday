library(tidyverse)
library(camcorder)
library(showtext)
library(patchwork)
library(scico)

# Set fonts
font_add_google("JetBrains Mono","jet")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 23.3, 
  height = 35, 
  units = "cm", 
  dpi = 600 
)


data <- read_delim('data/wheat/wheat_yields.csv',delim=",")%>%
  mutate(val_t=Value/10000)%>%
  mutate(val_cl=case_when(
    val_t<1~'1',
    val_t<2~'2',
    val_t<3~'3',
    val_t<4~'4',
    val_t<5~'5',
    val_t<6~'6',
    val_t<7~'7',
    val_t<8~'8',
    val_t<9~'9',
    val_t<10~'10',
    val_t<11~'11'
  ))

dat_1961<-data%>%
  filter(Year==1961)

sub<-dat_1961%>%
  pull(Area)

dat_2021<-data%>%
  filter(Year>2020)%>%
  filter(Area%in%sub)%>%
  filter(
    Area!="New Caledonia",
    Area!='Botswana',
    Area!='Chad',
    Area!='Mali',
    Area!='China, Taiwan Province of'
  )%>%
  group_by(Area)%>%
  summarise(val=mean(Value))%>%
  arrange(val)%>%
  mutate(ypos=row_number())%>%
  select(Area,ypos)
  





clean<-data%>%
  filter(Area%in%sub)%>%
  left_join(dat_2021)%>%
  select(Year,Area,ypos,val_cl)%>%
  drop_na()


labs<-clean%>%
  filter(Year==2022)%>%
  mutate(
    # Use (id-0.5), not just id, to center label on each item
    angle=0-180*(ypos-0.5)/max(ypos)
  )%>%
  # Right align on the left,
  # left align on the right
  mutate(
    hjust=case_when(
      angle<=-90~1,
      TRUE~0
    )
  )%>%
  # Flip left side labels
  mutate(
    angle=case_when(
      angle<=-90~angle+180,
      TRUE~angle
    )
  )%>%
  mutate(Area=case_when(
    Area=='United Kingdom of Great Britain and Northern Ireland'~'United Kingdom',
    TRUE~Area
  ))

pal<-c(
  "1"="#f15bb5",
  "2"="#f36fb7",
  "3"="#f582b9",
  "4"="#f8a9bc",
  "5"="#fcd0bf",
  "6"="#fff7c2",
  "7"="#c5ead0",
  "8"="#8bdcde",
  "9"="#51cfec",
  "10"="#34C8F3",
  "11"="#17c1fa"
)

col<-rev(scico(11, palette = 'lajolla'))

col<-scico(11, palette = 'batlow')


pal<-c(
  "1"=col[1],
  "2"=col[2],
  "3"=col[3],
  "4"=col[4],
  "5"=col[5],
  "6"=col[6],
  "7"=col[7],
  "8"=col[8],
  "9"=col[9],
  "10"=col[10],
  "11"=col[11]
)  

mx<-max(clean$ypos)

p2<-ggplot(clean)+
  geom_rect(
    aes(ymin=Year-0.5,ymax=Year+0.5,xmin=ypos-0.5,xmax=ypos+0.5,fill=val_cl,color=val_cl),
    #color=alpha("white",0.0),
    lwd=0.1
  )+
  geom_text(
    labs%>%filter(ypos<77),
    mapping=aes(y=Year-1,x=ypos,label=Area,angle=angle,hjust=hjust),
    color="white",family="jet",size=10
    #fontface='bold'
  )+
  geom_text(
    labs%>%filter(ypos>76),
    mapping=aes(y=Year-1,x=ypos,label=Area,angle=angle,hjust=hjust),
    color="dimgrey",family="jet",size=10
    #fontface='bold'
  )+
  scale_fill_manual(values=pal,na.value = pal[4])+
  scale_color_manual(values=pal,na.value = pal[4])+
  guides(fill='none',color='none')+
  scale_y_continuous(limits=c(1900,2022))+
  scale_x_continuous(limits=c(0,mx*2+1))+
  theme_void()+
  #theme(plot.background = element_rect(fill="#200A22"))+
  coord_polar(start=-pi/2)

p2
