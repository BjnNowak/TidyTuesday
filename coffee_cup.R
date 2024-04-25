library(tidyverse)
library(packcircles)
library(ggforce)
library(camcorder)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  height = 10, 
  width = 10, 
  units = "cm", 
  dpi = 300 
)

data<-read_delim('Data/coffee.csv')

# Convert lbs to gr
k <- 453.592

cl<-data%>%
  mutate(
    cons_gr=k*consumption_lbs,
    nb_coffee_min=round(cons_gr/12),
    nb_coffee_max=round(cons_gr/7),
    day_min=round(nb_coffee_min/365),
    day_max=round(nb_coffee_max/365)
  )

clean<-data%>%
  mutate(
    cons_gr=k*consumption_lbs,
    day_min=round(cons_gr/12),
    day_max=round(cons_gr/7)
    #nb_coffee_min=round(cons_gr/12),
    #nb_coffee_max=round(cons_gr/7),
    #day_min=round(nb_coffee_min/365),
    #day_max=round(nb_coffee_max/365)
  )%>%
  select(country,day_min,day_max)

# Some parameters
itermax = 10

long <- clean%>%
  #filter(Age=="18-24")%>%
  pivot_longer(!country, names_to = "cat", values_to = "value")

fun_circ <-function(ctry){

  dots <- long%>%
    mutate(x=1,y=1)%>%
    filter(country==ctry)

  tot_min<-dots$value[dots$cat=="day_min"]
  tot_max<-dots$value[dots$cat=="day_max"]-tot_min

  dts <- tibble(
    x=rep(1,tot_min),
    y=rep(1,tot_min),
    v=rep(1.5,tot_min),
    cat="min"
  )%>%
  bind_rows(tibble(
      x=rep(1,tot_max),
      y=rep(1,tot_max),
      v=rep(1.5,tot_max),
      cat='max'
  ))

  dts$x <- jitter(dts$x)
  dts$y <- jitter(dts$y)

  simulation <- circleRepelLayout(
    x = dts, xysizecols = 1:3,
    wrap = FALSE, sizetype = "radius",
    maxiter = 400, weights =1)$layout%>%
    bind_cols(cat=dts$cat)

  return(simulation)
}
  
pal<-c(
  "max"='black',
  "min"='black'
)

col_pal<-c(
  "max"=alpha("white",0.5),
  "min"="white"
)

simulation=fun_circ(ctry = "Italy")

tst<-ggplot(simulation,aes(x=x,y=y))+
  geom_circle(
    aes(x0=x,y0=y,r=1, fill=cat,color=cat),
    lwd=0.3
  )+
  coord_fixed()+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=col_pal)+
  scale_x_continuous(limits=c(-70,70))+
  scale_y_continuous(limits=c(-70,70))+
  guides(fill='none',color='none')+
  theme_void()
  theme(plot.background=element_rect(fill="black"))

ggsave("tst.png",plot=tst,device="png",width=10,height=10,unit='cm')
