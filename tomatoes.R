# load libraries
library(tidyverse)

check_even <- function(number) {
  if (number %% 2 == 0) {
    return("Even")
  } else {
    return("Odd")
  }
}

grape_fun<-function(ctry){
  
  n=round(data$total[data$country==ctry]/val_tomato)
  
  # Créer des données pour représenter une grappe de tomates
  grappe <- tibble(
    x = if(check_even(n)=="Even"){
      rep(c(-0.35,0.35),n/2)
    }else{
      c(rep(c(-0.35,0.35),n/2),-0.35)
    },
    y = seq(2*n,1,-2),
    color = c(
      rep("red",n-round(n*data$ratio[data$country==ctry])),
      rep("green",round(n*data$ratio[data$country==ctry])))
  )
  
  return(grappe)
  
}


# Data: https://www.tomatonews.com/en/worldwide-total-fresh-tomato-production-in-2021_2_1911.html
data<-tibble(
  country=c("China","India","Turkey","USA","Italy","Egypt","Spain","Mexico"),
  total=c(67538340,21181000,13095258,10475265,6644790,6245787,4754380,4149241),
  industry=c(4800000,162000,2200000,10223000,6059000,440000,3185000,40000)
)%>%
  mutate(ratio=industry/total)

max_n=36
val_tomato=data$total[data$country=="China"]/max_n


pal<-c(
  "red"="#dc3115",
  "green"="#bcbc56"
)

tige<-"#6d6e3d"

grappe = grape_fun("Italy")

ggplot(grappe)+
  geom_segment(aes(x=0,xend=x,y=y,yend=y),lwd=0.5,color=tige)+
  geom_point(aes(x=x,y=y,color=color),size=5)+
  annotate("segment",x=0,xend=0,y=max(grappe$y)+1.5,yend=min(grappe$y)-0.5,lwd=1.5,color=tige)+
  scale_x_continuous(limits=c(-5,5))+
  scale_y_continuous(limits=c(1,2*max_n+1.5))+
  scale_color_manual(values=pal)+
  guides(color="none")+
  theme_void()

ggsave(
  "tomato.svg",
  width=10,
  height=10,
  dpi=300,
  units="cm"
  )

