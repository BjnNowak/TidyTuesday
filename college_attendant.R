library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)

# Set fonts
font_add_google("Catamaran","cata")
font_add_google("Cabin","cab")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Roboto Slab","rob")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 12, 
  height = 26, 
  units = "cm", 
  dpi = 300 
)

college_admissions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-10/college_admissions.csv')
  
data<-college_admissions%>%
  group_by(name)%>%
  mutate(tot=sum(attend))%>%
  ungroup()%>%
  mutate(
    per_abs=attend/tot*100,
    per_abs=round(per_abs,2)
  )%>%
  drop_na(per_abs)%>%
  filter(par_income_lab=="Top 1")%>%
  select(name,per_abs)%>%
  arrange(-per_abs)%>%
  mutate(pos=row_number())%>%
  mutate(
    lab=glue::glue("**{pos}. {name}**<br><span style='color:grey50;'>{per_abs} %</span>")
  )%>%
  mutate(cl=case_when(
    pos<50~"top",
    TRUE~"bottom"
  ))

top<-data%>%
  tail(20)

bottom<-data%>%
  head(20)


clean<-top%>%
  add_row(per_abs=14,cl="transparent")%>%
  bind_rows(bottom)%>%
  arrange(per_abs)%>%
  mutate(rk=row_number())

pal<-c(
  "top"="#34073d",
  "transparent"=alpha("black",0),
  "bottom"="#ef745c"
)

ggplot()+
  geom_rect(
    clean,
    mapping=aes(
      xmin=0,xmax=per_abs,
      ymin=rk-0.25,ymax=rk+0.25,
      fill=cl
    )
  )+
  #geom_point(clean, mapping=aes(x=per_abs,y=rk))+
  geom_richtext(
    clean,
    mapping=aes(x=per_abs+0.5,y=rk,label=lab),
    hjust=0,
    size=6,
    lineheight=0.34,family="cab",
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  labs(
    title="      Share of students from top 1 % of income",
    subtitle="       Family who made about $630,000 or more.",
    caption="**Data** Opportunity Insights | **Plot** Benjamin Nowak"
  )+
  scale_x_continuous(limits=c(0,25))+
  scale_fill_manual(values=pal)+
  guides(fill='none')+
  theme_void()+
  theme(
    plot.background = element_rect(color=NA,fill="#FAFAFF"),
    plot.title = element_text(
      size=40,family="bit",
      face="bold",
      margin=margin(1,0,0.5,1,"cm")
    ),
    plot.subtitle = element_text(
      size=30,family="cab",
      margin=margin(0,0,-0.5,1,"cm")
    ),
    plot.caption = element_markdown(
      size=20,family="cab",hjust=0,
      margin=margin(-0.5,0,0,0.5,"cm")
    )
  )

