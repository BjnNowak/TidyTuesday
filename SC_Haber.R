library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)

# Set plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20,
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Load fonts 
font_add_google("Barlow Condensed","barlow")
font_add_google("Oswald","oswald")
font_add_google("Playfair Display","playfair")
# Automatically use {showtext} for plots
showtext_auto()

# Colors
wh<-"#FCEFEF"

# Load data
pop<-readr::read_delim('https://raw.githubusercontent.com/BjnNowak/HaberBosch/main/Data/WorldPop.csv',delim=';')
hb<-readr::read_delim('https://raw.githubusercontent.com/BjnNowak/HaberBosch/main/Data/PercentPopHB.csv',delim=';')

pop<-pop%>%
  mutate(Year=round(Year))%>%
  group_by(Year)%>%
  summarize(Pop=mean(Pop))%>%
  ungroup()

hb<-hb%>%
  mutate(Year=round(Year))%>%
  group_by(Year)%>%
  summarize(PercentPopHB=round(mean(PercentPopHB)))%>%
  ungroup()

data<-hb%>%
  inner_join(pop)%>%
  mutate(
    WithHB=Pop*PercentPopHB/100,
    NoHB=Pop-WithHB,
    Total=WithHB+NoHB
  )%>%
  select(Year,WithHB,NoHB,Total)%>%
  pivot_longer(!Year,names_to = "Type", values_to = "Pop")


# Labels 

po <- data%>%filter((Year==1960|Year==1974|Year==1987)&Type=="Total")%>%
  mutate(
    lab=round(Pop/1000),
    lab_full=glue::glue("{lab} billions"))

cap <- tibble(
  x=2004,y=250,label="**Data:** Erisman *et al.* (2008)  **| Plot:** @BjnNowak"
)

lab <- tibble(
  x=c(1908,1913,1914,1915,1918,1931,1933,1934,1940,1945,1965,1970,1991),
  y=c(5000,4100,2250,1450,3250,2750,4700,3800,3300,1700,2500,5000,6250),
  lab=c(
    # 1908
    'Fritz Haber filed<br>his patent on the<br>*“synthesis of ammonia*<br>*from its elements”*',
    # 1913
    'Bosch created a factory<br>in Oppau, Germany<br>for ammonia synthesis',
    # 1914
    'Haber supports<br>the German army<br>during World War I',
    # 1915
    "First massive chemical attack with<br>10,000 victims at Ypres due to<br>chlorine gas created by Haber<br>
    Three weeks later, suicide of<br>Haber's wife Clara Immerwahr ",
    # 1918
    'Nobel Prize<br>in Chemistry<br>for Fritz Haber',
    # 1931
    'Nobel Prize<br>in Chemistry<br>for Carl Bosch',
    # 1933
    "Fritz Haber's<br>exile in England<br>due to the<br>Nazi regime",
    #1934
    "Death of Haber<br>in Switzerland",
    #1940
    "Death of Bosch<br>in Germany",
    # 1945 
    "Start of the Green Revolution<br>with the breeding of dwarf<br>wheat by Norman Borlaug",
    # 1965 
    "Huge yield increase<br>following the introduction<br>of dwarf wheat in Asia",
    # 1970
    "Nobel Peace<br>Prize for<br>Norman Borlaug",
    # 1991
    "First European<br>regulation to reduce<br>water pollution caused<br>by nitrogen from<br>agricultural sources"
  )
)%>%
  mutate(lab_full=glue::glue("**{x}**<br>{lab}"))


ggplot(
  data=data%>%filter(Type=='NoHB'|Type=='WithHB'),
  aes(x=Year,y=Pop,fill=Type)      
  )+
  geom_area()+
  geom_point(
    data=data%>%filter(Year==2008&Type=="WithHB"),
    aes(x=Year,y=Pop),color='#F46036',
    size=4
  )+
  geom_text(
    data=po,
    aes(x=Year,y=Pop,label=lab_full),
    size=12,family='oswald',
    inherit.aes=FALSE
  )+
  geom_richtext(
    data=lab,
    aes(x=x, y=y, label = lab_full),
    hjust=0,vjust=1,size=9,family='barlow',lineheight=0.4,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  annotate(
    geom = 'text',
    label='Total\nWorld\npopulation',
    x=2004,y=4500,hjust=1,
    family='oswald',size=16,lineheight=0.35
  )+
  annotate(
    geom = 'text',
    label='Share of the\npopulation fed by\nHaber-Bosch nitrogen',
    x=2004,y=1000,hjust=1,color='#FCEFEF',
    family='oswald',size=16,lineheight=0.35
  )+
  geom_richtext(
    data=cap,
    aes(x=x, y=y, label = label),color=wh,
    hjust=1,vjust=1,size=8,family='barlow',lineheight=0.4,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  #annotate(
  #  geom = 'text',
  #  label=bquote(N[2]*' + 3'~H[2]*' = 2'~NH[3]),
  #  x=2004,y=500,hjust=1,color='#FCEFEF',
  #  family='barlow',size=14,lineheight=0.35
  #)+
  
  annotate(
    geom = 'text',
    label=bquote(N[2]*' + 3'~H[2]*' = 2'~NH[3]),
    x=1954,y=3150,hjust=0.5,vjust=0.5,color='#414770',alpha=0.05,
    family='barlow',size=90,lineheight=0.35
  )+
  
  annotate(
    geom = 'text',
    label='In 2008, 48% of humanity\nwas fed by Haber-Bosch nitrogen',
    x=2006,y=3450,hjust=1,fontface='bold',color='#F46036',
    family='barlow',size=13,lineheight=0.35
  )+
  
  annotate(
    geom = 'text',
    label=
'Nitrogen is an essential element for plant growth. By creating a process to fix atmospheric nitrogen and\n
make it available to plants, Fritz Haber has helped feed billions of people. This method was developed on\n
an industrial scale by Carl Bosch to be known as the Haber-Bosch process, which provided the fertilizers\n
essential to the achievement of the Green Revolution at the end of World War II. But these fertilizers have\n
also triggered a cascade of environmental effects that must be adressed today.',
    x=1900,y=6150,hjust=0,vjust=1,color="black",
    family='oswald',size=10,lineheight=0.2
  )+
  
  annotate(
    geom = 'text',
    label=
      'A Nitrogen Story',
    x=1900,y=6500,hjust=0,vjust=1,color="black",
    family='playfair',size=25,lineheight=0.2,fontface='bold'
  )+
  
  annotate(
    "rect",  fill="#720E07",color=NA,
    xmin = 1914, xmax = 1918, ymin = 0, ymax = 250,
    alpha = .6
  )+
  annotate(
    "rect", fill="#720E07",color=NA,
    xmin = 1939, xmax = 1945, ymin = 0, ymax = 250,
    alpha = .6
  )+
  annotate(
    "text",  color=wh,
    x = 1916, y = 125, label="WWI",
    size=9,family='oswald'
  )+
  annotate(
    "text",  color=wh,
    x = 1942, y = 125, label="WWII",
    size=9,family='oswald'
  )+
  
  guides(fill='none')+
  scale_fill_manual(values=c("#D8DBE9","#414770"))+
  theme_void()+
  theme(
    plot.background=element_rect(fill=wh)
  )

