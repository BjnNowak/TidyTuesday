
tuesdata <- tidytuesdayR::tt_load(2021, week = 33)

chain_investment <- tuesdata$chain_investment

library(tidyverse)
library(ggrepel)
library(showtext)
library(cowplot)
font_add_google(name = "Playfair Display", family = "Playfair Display")
font_add_google(name = "Open Sans", family = "Open Sans")
showtext_auto()

tit<-"Playfair Display"
tex<-"Open Sans"

whole <- chain_investment%>%
  filter(group_num==1)%>%
  mutate(lab=case_when(
    category=="Total basic infrastructure"~"Basic",
    category=="Total digital infrastructure"~"Digital",
    category=="Total social infrastructure"~"Social"
  ))%>%
  mutate(
    col_cat=case_when(
      lab=="Basic"~"#0F4C5C",
      lab=="Social"~"#E36414",
      lab=="Digital"~"#9A031E"
    ))

whole$lab<-factor(
  whole$lab,
  c(
    "Digital",
    "Social",
    "Basic"
  ))


final<-whole%>%
  filter(year=="2017")%>%
  arrange(desc(lab))%>%
  mutate(ypos=cumsum(gross_inv_chain))

pal_basic <- c(
  "#030F12",
  "#0E4958",
  "#2CB9DD",
  "#73D1E8",
  "#B9E8F4"
  
)

pal_social <- c(
  "#83390B",
  "#E36414",
  "#F4AA7C"
)

pal_digital <- c(
  "#3C010C",
  "#9A031E",
  "#FA0F3A",
  "#FC738C"
)


multiple <- chain_investment%>%
  filter(group_num==4|group_num==17|group_num==22)%>%
  mutate(lab=case_when(
    category=="Conservation and development"~"Conservation",
    category=="Private computers in NAICS 515, 517, 518, and 519"~"Computers",
    category=="Private software in NAICS 515, 517, 518, and 519"~"Software",
    category=="Private communications equipment in NAICS 515, 517, 518, and 519"~"Com. equipment",
    category=="Private communications structures"~"Com. structures",
    TRUE~category
  ))%>%
  mutate(col_pal=case_when(
    lab=="Water"~pal_basic[1],
    lab=="Transportation"~pal_basic[2],
    lab=="Sewer"~pal_basic[3],
    lab=="Power"~pal_basic[4],
    lab=="Conservation"~pal_basic[5],
    lab=="Public safety"~pal_social[1],
    lab=="Health"~pal_social[2],
    lab=="Education"~pal_social[3],
    lab=="Software"~pal_digital[1],
    lab=="Computers"~pal_digital[2],
    lab=="Com. structures"~pal_digital[3],
    lab=="Com. equipment"~pal_digital[4]
  ))

final_mult<-multiple%>%
  filter(year=="2017")%>%
  group_by(group_num)%>%
  arrange(desc(category))%>%
  mutate(ypos=cumsum(gross_inv_chain))%>%
  ungroup()

A<-ggplot(
  whole,
  aes(x=year,y=gross_inv_chain,fill=lab)
  )+
  annotate(
    geom = "segment",
    x=1947,xend=2017,
    y=600000,yend=600000,
    color="#343a40")+
  annotate(
    geom = "segment",
    x=1947,xend=2017,
    y=300000,yend=300000,
    color="#343a40")+
  annotate(
    geom = "text",
    x=1947,
    y=630000,
    label="600 billions $",
    color="#343a40",hjust=0,
    size=17,family=tex)+
  annotate(
    geom = "text",
    x=1947,
    y=330000,
    label="300 billions $",
    color="#343a40",hjust=0,
    size=17,family=tex)+
  geom_area(color="white")+
  scale_fill_manual(breaks=whole$lab,values=whole$col_cat)+
  scale_color_manual(breaks=whole$lab,values=whole$col_cat)+
  #geom_text(
  #  data=final,
  #  aes(y=ypos-100000,label=lab,color=lab),x=2020,hjust=0,
  #  size=25,
  #  fontface="bold"
  #)+
  coord_cartesian(clip = "off")+
  scale_x_continuous(limits = c(1945,2020),breaks=c(1950,1970,1990,2010) )+
  guides(
    fill=FALSE,
    color=FALSE
  )+
  theme_minimal()+
  theme(
    text = element_text(family = tex),
    axis.text = element_text(size=60),
    axis.text.y=element_blank(),
    axis.title=element_blank(),
    panel.grid = element_blank()
  )

B<-ggplot(
  multiple,
  aes(x=year,y=gross_inv_chain,fill=lab)
  )+
  geom_area(col='white')+
  facet_grid(meta_cat~.)+
  geom_text_repel(
    data=final_mult,
    aes(y=ypos-20000,label=lab,color=lab),
    x=2018,hjust=0,
    direction='y',
    min.segment.length = Inf,
    size=15,
    fontface="italic",
    family=tex
  )+
  coord_cartesian(clip = "off")+
  scale_y_continuous(
    limits = c(-20000,350000),
    breaks=c(100000,300000),
    labels=c("100","500"))+
  scale_x_continuous(limits = c(1947,2035),breaks=c(1950,1970,1990,2010) )+
  guides(
    fill=FALSE,
    color=FALSE
    )+
  scale_fill_manual(breaks=multiple$lab,values=multiple$col_pal)+
  scale_color_manual(breaks=multiple$lab,values=multiple$col_pal)+
  labs(
    x="",
    y=""
  )+
  theme_minimal()+
  theme(
    text = element_text(family = tex),
    axis.text=element_blank(),
    axis.title=element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid = element_blank()
  )







# Works for A4 format
ggdraw() +
  draw_plot(A, x = 0, y = 0, width = 0.5, height = 0.9) +
  draw_plot(B, x = 0.48, y = 0.035, width = 0.51, height = 1)+ 
  draw_plot_label(
    label = "Investments in US infrastructures",  
    size = 80,family=tit,hjust=0,color="#343a40",
    x = 0.04, y = 0.97)+
  draw_plot_label(
    label = "from 1947 to 2017",  
    size = 80,family=tit,hjust=0,color="#343a40",
    x = 0.04, y = 0.90)+
  draw_text(
    text = "Gross investment in 2012 dollars",  
    size = 60,family=tex,hjust=0,color="#343a40",
    x = 0.04, y = 0.80)+
  draw_text(
    text = "Digital",  
    size = 70,family=tit,hjust=0,color="#9A031E",
    x = 0.47, y = 0.85)+
  draw_text(
    text = "Social",  
    size = 70,family=tit,hjust=0,color="#E36414",
    x = 0.47, y = 0.55)+
  draw_text(
    text = "Basic",  
    size = 70,family=tit,hjust=0,color="#0F4C5C",
    x = 0.47, y = 0.25)+
  draw_text(
    text = "Data: Bureau of Economic Analysis",  
    size = 50,family=tex,hjust=0,color="#343a40",angle=90,
    x = 0.025, y = 0.08,vjust=0)


  

