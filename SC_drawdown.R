library(tidyverse)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)


# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# See here for data :
# https://www.drawdown.org/solutions/table-of-solutions
data<-read_delim('Data/Drawdown/data/solutions.csv',delim=";")%>%
  mutate(Main_sector=str_trim(Main_sector))

sel<-c("Electricity","Food, Agriculture, and Land Use","Industry","Transportation","Sinks")

clean<-data%>%
  mutate(Main_sector=case_when(
    str_detect(Main_sector,"Sinks")~"Sinks",
    Main_sector%!in%sel~"Others",
    TRUE~Main_sector
  ))%>%
  select(Solution,Main_sector,Scenario_1,Scenario_2)%>%
  drop_na()%>%
  #filter(Main_sector=="Electricity")%>%
  group_by(Solution)%>%
  mutate(
    high=max(Scenario_2,Scenario_1),
    low=min(Scenario_2,Scenario_1),
    gap=high-low,
  )%>%
  ungroup()%>%
  arrange(-high)%>%
  mutate(rk=row_number())%>%
  mutate(
    sp = "  - ",
    lab_wide = glue("**{rk}** <span style='color:grey40;'>{Solution} {sp}</span>")
  )%>%
  group_by(Main_sector)%>%
  arrange(high)%>%
  mutate(id=row_number())%>%
  ungroup()%>%
  mutate(mx=max(id))%>%
  group_by(Main_sector)%>%
  mutate(mx_sector=max(id))%>%
  ungroup()%>%
  mutate(id=id+mx-mx_sector)
  

res<-clean%>%group_by(Main_sector)%>%summarize(mx=sum(high),mn=sum(low))



pal_trsport <- rev(c(
  "#ffcf67", "#cbbe70", "#b1b575", "#96ac79", "#7ca47e", "#629b82", "#2d898b"
))

pal_elec <- rev(c(
  "#ffcf67", "#ddc08d", "#ccb9a0", "#bbb1b3", "#aaaac6", "#99a2d9", "#7692ff"
))

pal_indus <- rev(c(
  "#ffcf67", "#fab365", "#f7a564", "#f49762", "#f18961", "#ee7b5f", "#e85f5c"
))

pal_agro <- rev(c(
  "#ffcf67", "#cccd6d", "#b2cc70", "#98cb72", "#7eca75", "#64c977", "#30c67c"
))

pal_others <- rev(c(
  "#ffcf67", "#d0ac70", "#b89b74", "#a08978", "#89787c", "#716680", "#414288"
))

pal_sinks <- rev(c(
  "#ffcf67", "#feb14f", "#fd9337", "#fd8c31", "#fd842b", "#fc751f", "#fb5607"
))

pl_fun <- function(tst2,pal_elec){

  pl<-ggplot(tst2)+
    geom_rect(aes(xmin=0,xmax=Scenario_1,ymin=id,ymax=id+0.75),fill=pal_elec[1])+
    geom_rect(aes(xmin=Scenario_1,xmax=Scenario_1+gap/6,ymin=id,ymax=id+0.75),fill=pal_elec[2])+
    geom_rect(aes(xmin=Scenario_1+gap/6,xmax=Scenario_1+2*gap/6,ymin=id,ymax=id+0.75),fill=pal_elec[3])+
    geom_rect(aes(xmin=Scenario_1+2*gap/6,xmax=Scenario_1+3*gap/6,ymin=id,ymax=id+0.75),fill=pal_elec[4])+
    geom_rect(aes(xmin=Scenario_1+3*gap/6,xmax=Scenario_1+4*gap/6,ymin=id,ymax=id+0.75),fill=pal_elec[5])+
    geom_rect(aes(xmin=Scenario_1+4*gap/6,xmax=Scenario_1+5*gap/6,ymin=id,ymax=id+0.75),fill=pal_elec[6])+
    geom_rect(aes(xmin=Scenario_1+5*gap/6,xmax=Scenario_1+6*gap/6,ymin=id,ymax=id+0.75),fill=pal_elec[7])+
    geom_richtext(
      aes(x=0,y=id+(0.75/2),label=lab_wide),
      hjust=1,size=6,family="fira",
      fill = NA, label.color = NA, 
      label.padding = grid::unit(rep(0, 4), "pt") 
    )+
    scale_y_continuous(limits=c(-5,27))+
    scale_x_continuous(limits=c(0,160))+
    theme_void()+
    coord_polar()

  return(pl)

}

elec<-clean%>%filter(Main_sector=="Electricity")
agro<-clean%>%filter(Main_sector=="Food, Agriculture, and Land Use")
indus<-clean%>%filter(Main_sector=="Industry")
trsport<-clean%>%filter(Main_sector=="Transportation")
sinks<-clean%>%filter(str_detect(Main_sector,"Sinks"))
others<-clean%>%filter(Main_sector=="Others")



# electricty
p1<-pl_fun(elec,pal_elec)
p1
# agriculture
p2<-pl_fun(agro,pal_agro)
# industry
p3<-pl_fun(indus,pal_indus)
# transport
p4<-pl_fun(trsport,pal_trsport)
# sinks
p5<-pl_fun(sinks,pal_sinks)
# others
p6<-pl_fun(others,pal_others)
p6

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 60, 
  height = 40, 
  units = "cm", 
  dpi = 300 
)

p1+p2+p5+p4+p3+p6&
  theme(plot.background = element_rect(fill=NA,color=NA))




