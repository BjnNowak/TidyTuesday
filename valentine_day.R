library(tidyverse)
library(packcircles)
library(ggforce)
library(glue)
library(patchwork)
library(camcorder)
library(showtext)
library(ggtext)
library(MoMAColors)

# Set fonts
font_add_google("Bebas Neue","beb")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Fira Sans","fira")
font_add_google("Jost","jost")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  height = 6.75*2, 
  width = 12*2, 
  units = "cm", 
  dpi = 300 
)

# Set color palette

tst <- moma.colors("Klein", n=10, type="discrete")

tst

pal <- c(
  "SpendingCelebrating"=tst[10],
  "Candy"=tst[2],
  "Flowers"=tst[3],
  "Jewelry"=tst[4],
  "GreetingCards"=tst[1],
  "EveningOut"=tst[8],
  "Clothing"=tst[6],
  "GiftCards"=tst[5]
)


# Load data
gifts_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_age.csv')


rowSums(gifts_age[,2:9])

# Some parameters
itermax = 10

long <- gifts_age%>%
  #filter(Age=="18-24")%>%
  pivot_longer(!Age, names_to = "cat", values_to = "v")



make_circles <- function(years){ 
  
  dots <- long%>%
    filter(Age==years)%>%
    mutate(x=1,y=1)%>%
    select(x,y,v,cat,age=Age)

  a <- 2
  b <- 3
  theta <- seq(0,2*pi,(2*pi)/7)
  r <- a + b*theta

  tib <- tibble(
    x=r*cos(theta), 
    y=r*sin(theta)
  )

  dots$x <- tib$x
  dots$y <- tib$y


  # Compute each point centroid position
  simulation <- circleRepelLayout(
    x = dots, xysizecols = 1:3,
    wrap = FALSE, sizetype = "radius",
    maxiter = itermax, weights =1)$layout%>%
    bind_cols(cat=dots$cat)%>%
    bind_cols(age=years)

  return(simulation)
  
}



make_plot <- function(years){
  
  simulation <- make_circles(years)%>%
    mutate(cat2=case_when(
      cat=="GreetingCards"~"Greeting<br>cards",
      cat=="EveningOut"~"Evening<br>out",
      cat=="SpendingCelebrating"~"Spending<br>celebrating",
      cat=="GiftCards"~"Gift<br>cards",
      TRUE~cat
    ))
  
  sm<-sum(simulation$radius)
  
  mst <- simulation%>%
    filter(radius==max(radius))
    
  
  pl<-ggplot(simulation)+
      geom_circle(aes(x0=x,y0=y,r=radius, fill=cat),color=NA)+
      geom_richtext(
        data=simulation%>%filter(age=="18-24")%>%filter(cat!="Candy"),
        mapping=aes(x=x,y=y,label=cat2),
        size=5,family="ral",color="ivory",
        lineheight=0.35,vjust=0.5,
        fill = NA, label.color = NA, 
        label.padding = grid::unit(rep(0, 4), "pt") 
      )+
      geom_richtext(
        data=mst,
        mapping=aes(x=x,y=y,label=cat2),
        size=6,family="ral",color="ivory",
        lineheight=0.35,vjust=0.5,
        fill = NA, label.color = NA, 
        label.padding = grid::unit(rep(0, 4), "pt") 
      )+
      geom_circle(
        data=mst,
        aes(x0=x,y0=y,r=radius, fill=cat),
        color="white",fill=NA,lwd=1
      )+
      coord_fixed()+
      scale_x_continuous(limits=c(-150,150))+
      scale_y_continuous(limits=c(-200,200))+
      scale_fill_manual(
        values=pal
      )+
      guides(fill='none')+
      labs(
        title=glue("**{years} years**"),
        subtitle = glue("Total spendings  **{sm} $**")
      )+
      theme_void()+
      theme(
        plot.title = element_markdown(size=30,color="ivory",hjust=0.5,family='bit'),
        plot.subtitle = element_markdown(size=25,color="ivory",hjust=0.5,family='ral')
      )
  
  return(pl)
  
}


make_plot("18-24")+make_plot("25-34")+
  make_plot("35-44")+make_plot("45-54")+
  make_plot("55-64")+make_plot("65+")+
  plot_layout(ncol=6)+
  plot_annotation(
    title="**Valentine's Day spending by age group in the United States**",
    subtitle="As we age, spending for Valentine's Day declines and evolves: greeting cards are becoming the main expense item, while the amount dedicated to celebrations is declining.",
    caption="**Data** National Retail Federation **| Plot** Benjamin Nowak",
    theme=theme(
      plot.title = element_markdown(family="bit",size=45,color="white",margin=margin(0,0.5,0.25,0.5,'cm')),
      plot.subtitle = element_markdown(family="jost",size=27,color="white",margin=margin(0,0.5,0.5,0.5,'cm')),
      plot.caption = element_markdown(family="ral",size=20,color="white",margin=margin(0,0.5,0.5,0.5,'cm'))
    )
  )&
  theme(plot.background = element_rect(color  = NA, fill ="#171412"))
