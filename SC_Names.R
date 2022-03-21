library(tidyverse)
library(glue)
library(ggtext)
library(camcorder)
library(showtext)
library(patchwork)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

# Fonts
font_add_google("Open Sans","open")
font_add_google("Bebas Neue","bebas")
font_add_google("League Spartan","spart")
font_add_google("Fira Sans","fira")

showtext_auto()

# Load data
babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

# Prep data
top10_m <- babynames%>%
  filter(sex=='M')%>%
  filter(year==2017)%>%
  arrange(-n)%>%
  head(10)%>%
  mutate(rank=row_number())%>%
  mutate(lab=glue("<b>{rank}.</b> {name}"))%>%
  mutate(prop=-prop)%>%
  mutate(xpos=prop-0.002)%>%
  mutate(hjust=1)%>%
  mutate(per=-round(prop*100),1)%>%
  mutate(per_lab=glue("{per}%"))%>%
  mutate(hjust2=0)%>%
  mutate(xpos2=prop+0.001)

top10_f <- babynames%>%
  filter(sex=='F')%>%
  filter(year==2017)%>%
  arrange(-n)%>%
  head(10)%>%
  mutate(rank=row_number())%>%
  mutate(lab=glue("{name} <b>.{rank}</b>"))%>%
  mutate(xpos=prop+0.002)%>%
  mutate(hjust=0)%>%
  mutate(per=round(prop*100),1)%>%
  mutate(per_lab=glue("{per}%"))%>%
  mutate(hjust2=1)%>%
  mutate(xpos2=prop-0.001)

top10_m_1900 <- babynames%>%
  filter(sex=='M')%>%
  filter(year==1900)%>%
  arrange(-n)%>%
  head(10)%>%
  mutate(rank=row_number())%>%
  mutate(lab=glue("<b>{rank}.</b> {name}"))%>%
  mutate(prop=-prop)%>%
  mutate(xpos=prop-0.002)%>%
  mutate(hjust=1)%>%
  mutate(per=-round(prop*100),1)%>%
  mutate(per_lab=glue("{per}%"))%>%
  mutate(hjust2=0)%>%
  mutate(xpos2=prop+0.001)

top10_f_1900 <- babynames%>%
  filter(sex=='F')%>%
  filter(year==1900)%>%
  arrange(-n)%>%
  head(10)%>%
  mutate(rank=row_number())%>%
  mutate(lab=glue("{name} <b>.{rank}</b>"))%>%
  mutate(xpos=prop+0.002)%>%
  mutate(hjust=0)%>%
  mutate(per=round(prop*100),1)%>%
  mutate(per_lab=glue("{per}%"))%>%
  mutate(hjust2=1)%>%
  mutate(xpos2=prop-0.001)

top10_2017 <- top10_m%>%
  bind_rows(top10_f)

top10_1900 <- top10_m_1900%>%
  bind_rows(top10_f_1900)

# Make plots
p2017<-ggplot(data=top10_2017,aes(x=prop,y=as.factor(-rank),fill=sex))+
  geom_bar(stat="identity")+
  geom_richtext(
    mapping=aes(label=name,x=xpos,hjust=hjust,color=sex),
    size=12,
    family='spart',
    lineheight=0.45,vjust=0.5,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  geom_richtext(
    data=top10_2017%>%filter(rank==1),
    mapping=aes(label=per_lab,x=xpos2,hjust=hjust2),
    size=10, 
    family='fira',
    #fontface='italic', 
    lineheight=0.45,vjust=0.5,
    color='#FFF8E8',
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  scale_x_continuous(
    limits=c(-0.067,0.067),
    position = "top",
    breaks=c(-0.01,-0.005,0.005,0.01),
    labels=c('1%','0.5%','0.5%','1%')
  )+
  scale_fill_manual(
    values=c('#c11e38','#220b34')
  )+
  scale_color_manual(
    values=c('#c11e38','#220b34')
  )+
  guides(
    fill='none',
    color='none'
  )+
  labs(
    x='2017'
  )+
  annotate(
    geom='text',label="Boys",x=-0.05,y=3,
    size=12,family="fira",fontface='bold',color='#220b34'
  )+
  annotate(
    geom='text',label="Girls",x=0.05,y=3,
    size=12,family="fira",fontface='bold',color='#c11e38'
  )+
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    #axis.text.x = element_text(size=25,family='open'),
    axis.text.x = element_blank(),
    axis.title.x = element_text(size=40,family='open',face='bold')
  )

p1900<-ggplot(data=top10_1900,aes(x=prop,y=as.factor(-rank),fill=sex))+
  geom_bar(stat="identity")+
  geom_richtext(
    mapping=aes(label=name,x=xpos,hjust=hjust,color=sex),
    size=12,
    family='spart',
    lineheight=0.45,vjust=0.5,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  geom_richtext(
    mapping=aes(label=per_lab,x=xpos2,hjust=hjust2),
    size=10, 
    family='fira',
    #fontface='italic', 
    lineheight=0.45,vjust=0.5,
    color='#FFF8E8',
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  scale_x_continuous(
    limits=c(-0.067,0.067),
    position = "top",
    breaks=c(-0.01,-0.005,0.005,0.01),
    labels=c('1%','0.5%','0.5%','1%')
  )+
  scale_fill_manual(
    values=c('#c11e38','#220b34')
  )+
  scale_color_manual(
    values=c('#c11e38','#220b34')
  )+
  guides(
    fill='none',
    color='none'
  )+
  labs(
    x='1900'
  )+
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    #axis.text.x = element_text(size=25,family='open'),
    axis.text.x = element_blank(),
    axis.title.x = element_text(size=40,family='open',face='bold')
  )


p1900+p2017+ plot_annotation(
  title = "The diversity of US newborns' names increases over time",
  subtitle = 'This plot compares the percentage accounted for by each name of the Top 10 for years 1900 and 2017',
  caption = '     Data: H. Wickham | Plot: @BjnNowak'
)&
  theme(
    plot.background = element_rect(fill="#F1FFE7",color=NA),
    plot.title = element_text(size=45,hjust=0.5,family = "spart",face='bold'),
    plot.subtitle = element_text(size=30,hjust=0.5,family = "fira"),
    plot.caption = element_text(size=20,hjust=0,family='open'),
    plot.caption.position = "panel"
  )
