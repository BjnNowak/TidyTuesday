library(tidyverse)
library(lubridate)
library(camcorder)
library(showtext)
library(ggtext)
library(patchwork)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 40, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

plots <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/plots.csv')
species <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/species.csv')
surveys <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/surveys.csv')



# specy kangaroo rat : DS
plot_krat_excl <- c(3,19,21)
# control plots
plot_control <- c(4,11,17)

# Counting number of surveys per year and per decade
count_surv<-surveys%>%
  #filter(plot==1)%>%
  mutate(ct=1)%>%
  group_by(month,day,year)%>%
  summarize(
    sm=sum(ct)
  )%>%
  ungroup()%>%
  mutate(ct=1)%>%
  mutate(decade=case_when(
    year<1990~1980,
    year<2000~1990,
    year<2010~2000,
    TRUE~2010
  ))%>%
  group_by(decade,year)%>%
  summarize(count_surv=sum(ct))%>%
  ungroup()%>%
  mutate(ct=1)%>%
  group_by(decade)%>%
  summarize(nb_year=sum(ct))%>%
  ungroup()
    

krat<-surveys%>%
  filter(plot%in%plot_krat_excl)%>%
  mutate(ct=1)%>%
  mutate(decade=case_when(
    year<1990~1980,
    year<2000~1990,
    year<2010~2000,
    TRUE~2010
  ))%>%
  group_by(species,decade)%>%
  summarize(
    sm=sum(ct)
  )%>%
  ungroup()%>%
  drop_na()%>%
  left_join(count_surv)%>%
  mutate(per_year=round(sm/nb_year))%>%
  filter(per_year>0)%>%
  select(species,decade,per_year)%>%
  left_join(species)

cont<-surveys%>%
  filter(plot%in%plot_control)%>%
  mutate(ct=1)%>%
  mutate(decade=case_when(
    year<1990~1980,
    year<2000~1990,
    year<2010~2000,
    TRUE~2010
  ))%>%
  group_by(species,decade)%>%
  summarize(
    sm=sum(ct)
  )%>%
  ungroup()%>%
  drop_na()%>%
  left_join(count_surv)%>%
  mutate(per_year=round(sm/nb_year))%>%
  filter(per_year>0)%>%
  select(species,decade,per_year)%>%
  left_join(species)


ggplot(krat,aes(x=decade,y=per_year,color=species))+
  geom_line()

# Sp most represented in krat
sub_sp<-c("PP","PB","RM","DM","DS")

pal<-c(
  "PP"="#21897E",
  "PB"="#FF70A6",
  "RM"="#FF9770",
  "DM"="#ffd670",
  "DS"="#70D6FF",
  "Others"="grey90"
)

fun_dens<-function(tb,dec){

  tst<-tb%>%filter(decade==dec)
  tst2<-splitstackshape::expandRows(tst, "per_year")

  x_coords <- tibble(x=runif(dim(tst2)[1], 1, 10))
  y_coords <- tibble(y=runif(dim(tst2)[1], 1, 10))

  tst3<-tst2%>%
    bind_cols(x_coords)%>%
    bind_cols(y_coords)%>%
    mutate(sp_cl=case_when(
    species%in%sub_sp~species,
    TRUE~'Others'
  ))

  pl<-ggplot(tst3,aes(x=x,y=y,size=meanwgt,color=sp_cl))+
    geom_point()+
    guides(color="none",size="none")+
    coord_fixed()+
    scale_x_continuous(limits=c(1,10))+
    scale_y_continuous(limits=c(1,10))+
    scale_color_manual(values=pal)+
    theme_void()+
    theme(
      panel.background = element_rect(fill=NA,color="black"),
      plot.margin = margin(1,1,0,1,"cm")  
    )

  return(pl)
  
}

fun_dens(tb=krat,dec="1980")+fun_dens(tb=krat,dec="1990")+fun_dens(tb=krat,dec="2000")+fun_dens(tb=krat,dec="2010")+
  fun_dens(tb=cont,dec="1980")+fun_dens(tb=cont,dec="1990")+fun_dens(tb=cont,dec="2000")+fun_dens(tb=cont,dec="2010")+ 
  plot_layout(ncol = 4)& 
  theme(
    plot.background = element_rect(fill=NA,color=NA)
  )
  