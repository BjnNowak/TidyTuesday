library(tidyverse)
library(patchwork)
library(ggtext)
library(camcorder)
library(showtext)
library(ggbump)

# Messy code here coz I tested a few solutions before choosing the right format

# Set fonts
font_add_google("Anton","anton")
font_add_google("Londrina Solid","lon")
font_add_google("Quicksand","quick")
font_add_google("Open Sans","open")
font_add_google("Raleway","ral")
font_add_google("Staatliches","sta")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width =  14.3, 
  height = 31.1,
  units = "cm", 
  dpi = 300 
)

data<-read_delim('Data/exp_comp_ukr_rus.csv')%>%
  rename(
    from="Reporter Countries",
    to="Partner Countries"
  )

ue<-c(
  "Germany","Spain","Romania","Poland",
  "Italy","Hungary","Greece","Latvia"
)

fun_trade=function(crop,country){

  clean<-data%>%
    filter(Item==crop)%>%
    filter(from==country)

  clean_2018<-clean%>%
    filter(Year==2018)%>%
    select(to,val_2018=Value)

  clean_2022<-clean%>%
    filter(Year==2022)%>%
    select(to,val_2022=Value)

  bd <- clean_2018%>%
    full_join(clean_2022)%>% 
    mutate(
      val_2018 = replace_na(val_2018, 0),
      val_2022 = replace_na(val_2022, 0),
      
      gap = val_2022-val_2018,
      col=case_when(
        gap<0~"Decrease",
        gap==0~"Neutral",
        gap>0~'Increase'
      )
    )%>%
    arrange(-val_2022,-val_2018)%>%
    mutate(
      rk=row_number()
    )%>%
    group_by(to)%>%
    mutate(x_pos = max(val_2018,val_2022))%>%
    ungroup()%>%
    mutate(to=case_when(
      to=="Türkiye"~"Turkiye",
      to=="Côte d'Ivoire"~"Cote d Ivoire",
      to=="United Republic of Tanzania"~"Tanzania",
      TRUE~to
    ))%>%
    mutate(name=case_when(
      to%in%ue~glue::glue("<span style='color:#FF6F59;'><b>{rk}</b>. {to}</span>"),
      TRUE~glue::glue("<b>{rk}</b>. {to}")
    ))
  
  return(bd)
  
}

pal<-c(
  "Increase"= "#00ee6e",
  "Decrease"="#0c75e6",
  "Neutral"="dimgrey"
)  

xgrid <- tibble(
  x=seq(0,2000000,500000)
)

p1<-ggplot(
  fun_trade(crop="Wheat",country="Russian Federation")%>%
    filter(rk<31)
  )+
  geom_segment(
    xgrid,
    mapping=aes(x=x,xend=x,y=-Inf,yend=Inf),
    alpha=0.25
  )+
  geom_segment(aes(x=val_2022,xend=val_2018,y=-rk,yend=-rk,color=col),lwd=1.5)+
  geom_point(aes(x=val_2022,y=-rk),color="#202A25")+
  geom_richtext(
    aes(x=-5000,y=-rk,label=name),hjust=1,size=12,
    family="quick",
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  scale_x_continuous(limits=c(-300000,2250000))+
  scale_color_manual(values=pal)+
  guides(color='none')+
  theme_void()
p1

p2<-ggplot(
  fun_trade(crop="Wheat",country="Ukraine")%>%
    filter(rk<31)
)+
  geom_segment(
    xgrid,
    mapping=aes(x=x,xend=x,y=-Inf,yend=Inf),
    alpha=0.25
  )+
  geom_segment(aes(x=val_2022,xend=val_2018,y=-rk,yend=-rk,color=col),lwd=1.5)+
  geom_point(aes(x=val_2022,y=-rk),color="#202A25")+
  geom_richtext(
    aes(x=-5000,y=-rk,label=name),hjust=1,size=12,
    family="quick",
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  scale_x_continuous(limits=c(-300000,2250000))+
  scale_color_manual(values=pal)+
  guides(color='none')+
  theme_void()
p2

p1/p2






tst<-fun_trade(crop="Wheat",country="Russian Federation")%>%
  arrange(-val_2018)%>%
  mutate(rk_2018=row_number())%>%
  select(to,'2018'=rk_2018,'2022'=rk)%>%
  pivot_longer(!to,names_to="year",values_to='rank')

tst2<-fun_trade(crop="Wheat",country="Russian Federation")%>%
  arrange(-val_2018)%>%
  mutate(rk_2018=row_number())%>%
  select(to,'2018'=val_2018,'2022'=val_2022,col)%>%
  pivot_longer(!c(to,col),names_to="year",values_to='rank')

t<-fun_trade(crop="Wheat",country="Ukraine")

tst_ukr<-fun_trade(crop="Wheat",country="Ukraine")%>%
  arrange(-val_2018)%>%
  mutate(rk_2018=row_number())%>%
  select(to,'2018'=val_2018,'2022'=val_2022,col)%>%
  pivot_longer(!c(to,col),names_to="year",values_to='rank')

pal<-c(
  "Increase"= "#FFDD00",
  "Decrease"="#0057B7",
  "Neutral"="dimgrey"
)  



subs<-c(
  'Spain','Egypt','Turkiye','Indonesia',
  'Bangladesh','Poland','Greece',
  "Pakistan",'Romania','Philippines',
  'Morocco','Algeria','Lebanon'
)

labs<-tst_ukr%>%
  filter(year=="2022")%>%
  filter(to%in%subs)

yax=tibble(
  y=seq(100000,400000,100000)
)

ggplot()+
  geom_text(
    yax,
    mapping=aes(x=2017.9,y=y,label="-"),
    size=10,color="white"
  )+
  geom_bump(
    tst_ukr,
    mapping=aes(
      x=as.numeric(year),y=as.numeric(rank),group=to,color=col
    ),
    alpha=0.20
  )+
  geom_bump(
    tst_ukr%>%filter(to%in%subs),
    mapping=aes(
      x=as.numeric(year),y=as.numeric(rank),group=to,color=col
    ),
    alpha=0.90
  )+
  geom_text(
    labs,
    mapping=aes(x=2022.05,y=as.numeric(rank),label=to,color=col),
    hjust=0, family="quick",size=10,
    fontface='bold'
  )+
  guides(color='none')+
  scale_color_manual(values=pal)+
  scale_x_continuous(limits=c(2017,2023),breaks=c(2018,2022))+
  #scale_y_continuous(limits=c(0,2250000))+
  theme_void()+
  theme(
    #plot.background = element_rect(fill="#222725",color=NA)
    #axis.text.x=element_text(color="white",family="open",size=20)
  )


