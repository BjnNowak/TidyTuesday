library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)

# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Pacifico","pac")
font_add_google("Abril Fatface","abril")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 15, 
  units = "cm", 
  dpi = 300 
)

data <- read_delim('Data/Farms/clean_data.csv',delim=";")

clean<-data%>%
  filter(Size!="All sizes")%>%
  group_by(Entity)%>%
  mutate(
    nb = row_number()
  )%>%
  ungroup()%>%
  mutate(
    gr = case_when(
      (nb==1)|(nb==2)~1,
      (nb==11)~11,
      TRUE~as.numeric(nb)
    )
  )%>%
  group_by(Entity,gr)%>%
  summarize(
    num_gr = sum(Number),
    surf_gr = sum(Surface)
  )%>%
  ungroup()%>%
  group_by(Entity)%>%
  summarize(
    classe = gr,
    num_per = round(num_gr/sum(num_gr)*100),
    surf_per = round(surf_gr/sum(surf_gr)*100)
  )%>%
  ungroup()%>%
  mutate(
    lab_num = case_when(
      num_per<1~glue("<1%"),
      TRUE~glue("{num_per}%")
    ),
    lab_surf= case_when(
      surf_per<1~glue("<1%"),
      TRUE~glue("{surf_per}%")
    )
  )%>%
  mutate(gr=case_when(
    classe==1~"Small",
    classe==11~"Big",
    TRUE~"Other"
  ))
clean



gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21, 
  height = 29.7, 
  units = "cm", 
  dpi = 300 
)

col_big <- "#95B4EE"
col_small <- "#F7C59F"
col_hide <- "#E1E5EE"
hgh <- "#E1E5EE"
background <- "#099773"


p1<-ggplot(data=clean%>%filter(Entity=="World"))+
  
  geom_segment(
    aes(x=1,xend=2,y=num_per,yend=surf_per),
    color=col_hide,linewidth=0.25)+
  geom_point(aes(y=num_per),x=1,size=2,color=col_hide)+
  geom_point(aes(y=surf_per),x=2,size=2,color=col_hide)+

  
  geom_segment(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Small"),
    aes(x=1,xend=2,y=num_per,yend=surf_per),
    linewidth=3,
    color=col_small)+
  geom_point(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Small"),
    aes(y=num_per),
    color=col_small,
    x=1,size=6)+
  geom_point(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Small"),
    aes(y=surf_per),
    color=col_small,
    x=2,size=6)+
  geom_segment(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Small"),
    aes(x=1,xend=2,y=num_per,yend=surf_per),
    linewidth=0.5,color=hgh)+
  geom_point(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Small"),
    aes(y=num_per),
    color=hgh,
    x=1,size=2)+
  geom_point(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Small"),
    aes(y=surf_per),
    color=hgh,
    x=2,size=2)+
  geom_text(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Small"),
    aes(y=num_per,label=lab_num),
    x=0.95,hjust=1,vjust=0.5,size=27,
    color=col_small,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Small"),
    aes(y=surf_per,label=lab_surf),
    x=2.05,hjust=0,vjust=0.5,size=24,
    color=col_small,
    family = "cond"
  )+
  
  
  scale_x_continuous(limits = c(0.7,2.3))+
  guides(color='none',alpha='none')+
  theme_void()+
  theme(plot.background = element_rect(fill=NA,color=NA))

p1

p2<-ggplot(data=clean%>%filter(Entity=="World"))+
  
  geom_segment(
    aes(x=1,xend=2,y=num_per,yend=surf_per),
    color=col_hide,
    linewidth=0.25)+
  geom_point(aes(y=num_per),x=1,size=2,color=col_hide)+
  geom_point(aes(y=surf_per),x=2,size=2,color=col_hide)+
  
  
  geom_segment(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Big"),
    aes(x=1,xend=2,y=num_per,yend=surf_per),
    linewidth=3,
    color=col_big)+
  geom_point(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Big"),
    aes(y=num_per),
    color=col_big,
    x=1,size=6)+
  geom_point(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Big"),
    aes(y=surf_per),
    color=col_big,
    x=2,size=6)+
  geom_segment(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Big"),
    aes(x=1,xend=2,y=num_per,yend=surf_per),
    linewidth=0.5,color=hgh)+
  geom_point(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Big"),
    aes(y=num_per),
    color=hgh,
    x=1,size=2)+
  geom_point(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Big"),
    aes(y=surf_per),
    color=hgh,
    x=2,size=2)+
  geom_text(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Big"),
    aes(y=num_per,label=lab_num),
    x=0.95,hjust=1,vjust=0.5,size=27,
    color=col_big,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Big"),
    aes(y=surf_per,label=lab_surf),
    x=2.05,hjust=0,vjust=0.5,size=24,
    color=col_big,
    family = "cond"
  )+
  
  
  scale_x_continuous(limits = c(0.7,2.3))+
  guides(color='none',alpha='none')+
  theme_void()+
  theme(plot.background = element_rect(fill=NA,color=NA))

p1
p2


p1+p2

p2<-ggplot(data=clean%>%filter(Entity=="Low income"))+
  annotate(geom="segment",x=1,xend=1,y=0,yend=85,color=col_ax,size=8,alpha=0.3)+
  annotate(geom="segment",x=2,xend=2,y=0,yend=85,color=col_ax,size=8,alpha=0.3)+
  #annotate(
  #  geom="text",x=1.5,y=75,color=col_ax,alpha=0.5,size=30,family="abril",
  #  hjust=0.5,vjust=0.5,lineheight=0.25,
  #  label="Monde"
  #)+
  annotate(
    geom="text",x=1,y=55,color="black",alpha=1,size=24,family="fira",
    hjust=0.5,vjust=0.5,lineheight=0.25,fontface="bold",
    label="Nombre\nd'exploitations"
  )+
  annotate(
    geom="text",x=2,y=55,color="black",alpha=1,size=24,family="fira",
    hjust=0.5,vjust=0.5,lineheight=0.25,fontface="bold",
    label="Surface\ncultivée"
  )+
  
  
  
  geom_text(
    data=clean%>%filter(Entity=="Low income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=0.9,hjust=1,vjust=0.5,size=27,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="Low income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2.1,hjust=0,vjust=0.5,size=24,
    family = "cond"
  )+
  
  geom_segment(aes(x=1,xend=2,y=num_per,yend=surf_per,color=gr,alpha=gr))+
  
  geom_point(aes(y=num_per,color=gr
                 #,alpha=gr
  ),x=1,size=6)+
  geom_point(data=clean%>%filter(Entity=="Low income")%>%filter(gr=="Big"|gr=="Small"),
             aes(y=num_per,color=gr
                 #,alpha=gr
             ),x=1,size=6)+
  geom_point(aes(y=surf_per,color=gr
                 ,alpha=gr
  ),x=2,size=6)+
  geom_point(data=clean%>%filter(Entity=="Low income")%>%filter(gr=="Big"|gr=="Small"),
             aes(y=surf_per,color=gr
                 ,alpha=gr
             ),x=2,size=6)+
  
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()


p2

p3<-ggplot(data=clean%>%filter(Entity=="High income"))+
  annotate(geom="segment",x=1,xend=1,y=0,yend=85,color=col_ax,size=8,alpha=0.3)+
  annotate(geom="segment",x=2,xend=2,y=0,yend=85,color=col_ax,size=8,alpha=0.3)+
  #annotate(
  #  geom="text",x=1.5,y=75,color=col_ax,alpha=0.5,size=30,family="abril",
  #  hjust=0.5,vjust=0.5,lineheight=0.25,
  #  label="Monde"
  #)+
  
  
  
  
  geom_text(
    data=clean%>%filter(Entity=="High income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=0.9,hjust=1,vjust=0.5,size=27,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="High income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2.1,hjust=0,vjust=0.5,size=24,
    family = "cond"
  )+
  
  geom_segment(aes(x=1,xend=2,y=num_per,yend=surf_per,color=gr,alpha=gr))+
  
  geom_point(aes(y=num_per,color=gr
                 #,alpha=gr
  ),x=1,size=6)+
  geom_point(data=clean%>%filter(Entity=="High income")%>%filter(gr=="Big"|gr=="Small"),
             aes(y=num_per,color=gr
                 #,alpha=gr
             ),x=1,size=6)+
  geom_point(aes(y=surf_per,color=gr
                 ,alpha=gr
  ),x=2,size=6)+
  geom_point(data=clean%>%filter(Entity=="High income")%>%filter(gr=="Big"|gr=="Small"),
             aes(y=surf_per,color=gr
                 ,alpha=gr
             ),x=2,size=6)+
  
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()


p3



p2<-ggplot(data=clean%>%filter(Entity=="Low income"))+
  annotate(geom="segment",x=1,xend=1,y=0,yend=85,color=col_ax,size=6,alpha=0.4)+
  annotate(geom="segment",x=2,xend=2,y=0,yend=85,color=col_ax,size=6,alpha=0.4)+
  
  annotate(
    geom="text",x=1.5,y=75,color=col_ax,alpha=0.5,size=30,family="abril",
    hjust=0.5,vjust=0.5,lineheight=0.25,
    label="Pays à\nrevenu\nfaible"
  )+
  
  geom_segment(aes(x=1,xend=2,y=num_per,yend=surf_per,color=gr,alpha=gr))+
  geom_point(aes(y=num_per,color=gr,alpha=gr),x=1,size=4)+
  geom_point(aes(y=surf_per,color=gr,alpha=gr),x=2,size=4)+
  
  geom_text(
    data=clean%>%filter(Entity=="Low income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=0.9,hjust=1,vjust=0.5,size=15,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="Low income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2.1,hjust=0,vjust=0.5,size=15,
    family = "cond"
  )+
  
  
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()

p3<-ggplot(data=clean%>%filter(Entity=="High income"))+
  annotate(geom="segment",x=1,xend=1,y=0,yend=85,color=col_ax,size=6,alpha=0.4)+
  annotate(geom="segment",x=2,xend=2,y=0,yend=85,color=col_ax,size=6,alpha=0.4)+
  annotate(
    geom="text",x=1.5,y=75,color=col_ax,alpha=0.5,size=30,family="abril",
    hjust=0.5,vjust=0.5,lineheight=0.25,
    label="Pays à\nrevenu\nélevé"
  )+
  geom_point(aes(y=num_per,color=gr,alpha=gr),x=1,size=4)+
  geom_point(aes(y=surf_per,color=gr,alpha=gr),x=2,size=4,inherit.aes=FALSE)+
  
  geom_text(
    data=clean%>%filter(Entity=="High income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=0.9,hjust=1,vjust=0.5,size=15,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="High income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2.1,hjust=0,vjust=0.5,size=15,
    family = "cond"
  )+
  
  geom_segment(aes(x=1,xend=2,y=num_per,yend=surf_per,color=gr,alpha=gr))+
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()

p4<-ggplot(data=clean%>%filter(Entity=="Europe and Central Asia"))+
  annotate(geom="segment",x=1,xend=1,y=0,yend=85,color=col_ax,size=6,alpha=0.4)+
  annotate(geom="segment",x=2,xend=2,y=0,yend=85,color=col_ax,size=6,alpha=0.4)+
  annotate(
    geom="text",x=1.5,y=75,color=col_ax,alpha=0.5,size=30,family="abril",
    hjust=0.5,vjust=0.5,lineheight=0.25,
    label="Europe\net\nAsie centrale"
  )+
  geom_point(aes(y=num_per,color=gr,alpha=gr),x=1,size=4)+
  geom_point(aes(y=surf_per,color=gr,alpha=gr),x=2,size=4,inherit.aes=FALSE)+
  
  geom_text(
    data=clean%>%filter(Entity=="Europe and Central Asia")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=0.9,hjust=1,vjust=0.5,size=15,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="Europe and Central Asia")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2.1,hjust=0,vjust=0.5,size=15,
    family = "cond"
  )+
  
  geom_segment(aes(x=1,xend=2,y=num_per,yend=surf_per,color=gr,alpha=gr))+
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()

p5<-ggplot(data=clean%>%filter(Entity=="Latin America and the Caribbean"))+
  annotate(geom="segment",x=1,xend=1,y=0,yend=85,color=col_ax,size=6,alpha=0.4)+
  annotate(geom="segment",x=2,xend=2,y=0,yend=85,color=col_ax,size=6,alpha=0.4)+
  annotate(
    geom="text",x=1.5,y=75,color=col_ax,alpha=0.5,size=30,family="abril",
    hjust=0.5,vjust=0.5,lineheight=0.25,
    label="Amérique latine\net caraïbes"
  )+
  geom_point(aes(y=num_per,color=gr,alpha=gr),x=1,size=4)+
  geom_point(aes(y=surf_per,color=gr,alpha=gr),x=2,size=4,inherit.aes=FALSE)+
  
  geom_text(
    data=clean%>%filter(Entity=="Latin America and the Caribbean")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=0.9,hjust=1,vjust=0.5,size=15,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="Latin America and the Caribbean")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2.1,hjust=0,vjust=0.5,size=15,
    family = "cond"
  )+
  
  geom_segment(aes(x=1,xend=2,y=num_per,yend=surf_per,color=gr,alpha=gr))+
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()

p6<-ggplot(data=clean%>%filter(Entity=="Middle East and North Africa"))+
  annotate(geom="segment",x=1,xend=1,y=0,yend=85,color=col_ax,size=6,alpha=0.4)+
  annotate(geom="segment",x=2,xend=2,y=0,yend=85,color=col_ax,size=6,alpha=0.4)+
  annotate(
    geom="text",x=1.5,y=75,color=col_ax,alpha=0.5,size=30,family="abril",
    hjust=0.5,vjust=0.5,lineheight=0.25,
    label="Moyen Orient\net Afrique du Nord"
  )+
  geom_point(aes(y=num_per,color=gr,alpha=gr),x=1,size=4)+
  geom_point(aes(y=surf_per,color=gr,alpha=gr),x=2,size=4,inherit.aes=FALSE)+
  
  geom_text(
    data=clean%>%filter(Entity=="Middle East and North Africa")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=0.9,hjust=1,vjust=0.5,size=15,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="Middle East and North Africa")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2.1,hjust=0,vjust=0.5,size=15,
    family = "cond"
  )+
  
  geom_segment(aes(x=1,xend=2,y=num_per,yend=surf_per,color=gr,alpha=gr))+
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()

(p1+p2+p3)/(p4+p5+p6)

#########################################"

# Colors

col_point <- "#F18805"

col_ax2 <- "#202C59"

col_ax <- "#A4A8D1"

col_pal <- c(
  "Big" = "#d81159",
  "Small" = "#218380",
  "Other" = col_ax2
)

# alpha
alpha_pal <- c(
  "Big" = 1,
  "Small" = 1,
  "Other" = 0.25
)
alpha_ax <- 0.6
alpha_tit <- 0.9

# Size
tex_small <- 16
tex_xsmall <- 9
ax_small <- 4
ax_xsmall <- 3
point_small <- 2.5
point_xsmall <- 1.75
line_small <- 0.5
line_xsmall <- 0.25
lab_xsmall <- 10

p1<-ggplot(data=clean%>%filter(Entity=="World"))+
  annotate(geom="segment",x=1,xend=1,y=0,yend=85,color=col_ax,size=6,alpha=alpha_ax)+
  annotate(geom="segment",x=2,xend=2,y=0,yend=85,color=col_ax,size=6,alpha=alpha_ax)+
  annotate(
    geom="text",x=1.5,y=75,color=col_ax,alpha=alpha_tit,size=35,family="abril",
    hjust=0.5,vjust=0.5,lineheight=0.25,
    label="Monde"
  )+
  annotate(
    geom="text",x=1,y=55,color="black",alpha=1,size=14,family="fira",
    hjust=0.5,vjust=0.5,lineheight=0.25,fontface="bold",
    label="Nombre\nd'exploitations"
  )+
  annotate(
    geom="text",x=2,y=55,color="black",alpha=1,size=14,family="fira",
    hjust=0.5,vjust=0.5,lineheight=0.25,fontface="bold",
    label="Surface\ncultivée"
  )+
  
  geom_segment(aes(x=1,xend=2,y=num_per,yend=surf_per,color=gr,alpha=gr),size=1)+
  geom_point(aes(y=num_per,color=gr,alpha=gr),x=1,size=4)+
  geom_point(aes(y=surf_per,color=gr,alpha=gr),x=2,size=4)+
  
  geom_text(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=0.9,hjust=1,vjust=0.5,size=19,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="World")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2.1,hjust=0,vjust=0.5,size=19,
    family = "cond"
  )+
  
 
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()

p2<-ggplot(data=clean%>%filter(Entity=="Low income"))+
  annotate(geom="segment",x=1.1,xend=1.1,y=0,yend=85,color=col_ax,size=ax_small,alpha=alpha_ax)+
  annotate(geom="segment",x=1.9,xend=1.9,y=0,yend=85,color=col_ax,size=ax_small,alpha=alpha_ax)+
  
  annotate(
    geom="text",x=1.5,y=75,color=col_ax,alpha=alpha_tit,size=tex_small,family="abril",
    hjust=0.5,vjust=0.5,lineheight=0.25,
    label="Pays à\nrevenu\nfaible"
  )+
  
  geom_segment(aes(x=1.1,xend=1.9,y=num_per,yend=surf_per,color=gr,alpha=gr),size=line_small)+
  geom_point(aes(y=num_per,color=gr,alpha=gr),x=1.1,size=point_small)+
  geom_point(aes(y=surf_per,color=gr,alpha=gr),x=1.9,size=point_small)+
  
  geom_text(
    data=clean%>%filter(Entity=="Low income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=1,hjust=1,vjust=0.5,size=15,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="Low income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2,hjust=0,vjust=0.5,size=15,
    family = "cond"
  )+
  
  
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()

p3<-ggplot(data=clean%>%filter(Entity=="High income"))+
  annotate(geom="segment",x=1.1,xend=1.1,y=0,yend=85,color=col_ax,size=ax_small,alpha=alpha_ax)+
  annotate(geom="segment",x=1.9,xend=1.9,y=0,yend=85,color=col_ax,size=ax_small,alpha=alpha_ax)+
  annotate(
    geom="text",x=1.5,y=75,color=col_ax,alpha=alpha_tit,size=tex_small,family="abril",
    hjust=0.5,vjust=0.5,lineheight=0.25,
    label="Pays à\nrevenu\nélevé"
  )+
  geom_point(aes(y=num_per,color=gr,alpha=gr),x=1.1,size=point_small)+
  geom_point(aes(y=surf_per,color=gr,alpha=gr),x=1.9,size=point_small,inherit.aes=FALSE)+
  
  geom_text(
    data=clean%>%filter(Entity=="High income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=1,hjust=1,vjust=0.5,size=15,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="High income")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2,hjust=0,vjust=0.5,size=15,
    family = "cond"
  )+
  
  geom_segment(aes(x=1.1,xend=1.9,y=num_per,yend=surf_per,color=gr,alpha=gr),size=line_small)+
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()

p4<-ggplot(data=clean%>%filter(Entity=="Europe and Central Asia"))+
  annotate(geom="segment",x=1,xend=1,y=0,yend=85,color=col_ax,size=ax_xsmall,alpha=alpha_ax)+
  annotate(geom="segment",x=2,xend=2,y=0,yend=85,color=col_ax,size=ax_xsmall,alpha=alpha_ax)+
  annotate(
    geom="text",x=1.5,y=75,color=col_ax,alpha=alpha_tit,size=tex_xsmall,family="abril",
    hjust=0.5,vjust=0.5,lineheight=0.25,
    label="Europe\net Asie\ncentrale"
  )+
  geom_point(aes(y=num_per,color=gr,alpha=gr),x=1,size=point_xsmall)+
  geom_point(aes(y=surf_per,color=gr,alpha=gr),x=2,size=point_xsmall,inherit.aes=FALSE)+
  
  geom_text(
    data=clean%>%filter(Entity=="Europe and Central Asia")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=0.9,hjust=1,vjust=0.5,size=lab_xsmall,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="Europe and Central Asia")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2.1,hjust=0,vjust=0.5,size=lab_xsmall,
    family = "cond"
  )+
  
  geom_segment(aes(x=1,xend=2,y=num_per,yend=surf_per,color=gr,alpha=gr),size=line_xsmall)+
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()

p5<-ggplot(data=clean%>%filter(Entity=="Latin America and the Caribbean"))+
  annotate(geom="segment",x=1,xend=1,y=0,yend=85,color=col_ax,size=ax_xsmall,alpha=alpha_ax)+
  annotate(geom="segment",x=2,xend=2,y=0,yend=85,color=col_ax,size=ax_xsmall,alpha=alpha_ax)+
  annotate(
    geom="text",x=1.5,y=75,color=col_ax,alpha=alpha_tit,size=tex_xsmall,family="abril",
    hjust=0.5,vjust=0.5,lineheight=0.25,
    label="Amérique\nlatine et\nCaraïbes"
  )+
  geom_point(aes(y=num_per,color=gr,alpha=gr),x=1,size=point_xsmall)+
  geom_point(aes(y=surf_per,color=gr,alpha=gr),x=2,size=point_xsmall,inherit.aes=FALSE)+
  
  geom_text(
    data=clean%>%filter(Entity=="Latin America and the Caribbean")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=0.9,hjust=1,vjust=0.5,size=lab_xsmall,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="Latin America and the Caribbean")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2.1,hjust=0,vjust=0.5,size=lab_xsmall,
    family = "cond"
  )+
  
  geom_segment(aes(x=1,xend=2,y=num_per,yend=surf_per,color=gr,alpha=gr),size=line_xsmall)+
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()

p6<-ggplot(data=clean%>%filter(Entity=="Middle East and North Africa"))+
  annotate(geom="segment",x=1,xend=1,y=0,yend=85,color=col_ax,size=ax_xsmall,alpha=alpha_ax)+
  annotate(geom="segment",x=2,xend=2,y=0,yend=85,color=col_ax,size=ax_xsmall,alpha=alpha_ax)+
  annotate(
    geom="text",x=1.5,y=75,color=col_ax,alpha=alpha_tit,size=tex_xsmall,family="abril",
    hjust=0.5,vjust=0.5,lineheight=0.25,
    label="Moyen Orient\net Afrique\ndu Nord"
  )+
  geom_point(aes(y=num_per,color=gr,alpha=gr),x=1,size=point_xsmall)+
  geom_point(aes(y=surf_per,color=gr,alpha=gr),x=2,size=point_xsmall,inherit.aes=FALSE)+
  geom_text(
    data=clean%>%filter(Entity=="Middle East and North Africa")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=num_per,label=lab_num,color=gr),
    x=0.9,hjust=1,vjust=0.5,size=lab_xsmall,
    family = "cond"
  )+
  geom_text(
    data=clean%>%filter(Entity=="Middle East and North Africa")%>%filter(gr=="Big"|gr=="Small"),
    aes(y=surf_per,label=lab_surf,color=gr),
    x=2.1,hjust=0,vjust=0.5,size=lab_xsmall,
    family = "cond"
  )+
  
  geom_segment(aes(x=1,xend=2,y=num_per,yend=surf_per,color=gr,alpha=gr),size=line_xsmall)+
  scale_x_continuous(limits = c(0.7,2.3))+
  scale_color_manual(values=col_pal)+
  scale_alpha_manual(values=alpha_pal)+
  guides(color='none',alpha='none')+
  theme_void()

p1 + (p2+p3)/(p4+p5+p6)

