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
  dpi = 300 
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

col<-rev(scico(11, palette = 'vik'))

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

p2<-ggplot(clean)+
  geom_rect(
    aes(xmin=Year-0.5,xmax=Year+0.5,ymin=ypos-0.5,ymax=ypos+0.5,fill=val_cl),
    color=alpha("white",0.05),lwd=0.1
  )+
  geom_text(
    clean%>%filter(Year==1961),
    mapping=aes(x=Year,y=ypos,label=Area),
    color="white",family="jet",hjust=0,size=8,
    fontface='bold'
  )+
  scale_fill_manual(values=pal,na.value = pal[4])+
  guides(fill='none')+
  theme_void()

p2
