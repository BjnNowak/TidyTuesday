library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)

# Set fonts
font_add_google("Bebas Neue","beb")
font_add_google("Raleway","ral")
font_add_google("Open Sans","open")
font_add_google("Jost","jo")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 14.40, 
  height = 25.60, 
  units = "cm", 
  dpi = 300 
)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv')
grant_opportunity_details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grant_opportunity_details.csv')


clean <- grants%>%
  left_join(
    grant_opportunity_details%>%select(-c(expected_number_of_awards,version)))

clean2<-clean%>%drop_na(category_agriculture)


grts <- grants%>%
  mutate(
    yr=year(grants$posted_date),
    category_climate = stringr::str_detect(tolower(opportunity_title), "climate")
  )%>%
  group_by(yr)%>%
  summarize(
    sm_all=sum(estimated_funding, na.rm=TRUE),
    sm_climate=sum(estimated_funding[category_climate==TRUE], na.rm=TRUE)
  )%>%
  ungroup()%>%
  mutate(ratio=sm_climate/sm_all)

clean<-grants%>%
  mutate(
    title=tolower(opportunity_title),
    title=str_replace_all(title, "[^[:alnum:]]", " "),
    title_list=str_split(title, " ")
  )%>%
  unnest(title_list)


stopwords<-c(
  "","a","and","the","of","for",
  "program","department","united","states","fy",
  "year","grant","act","to","84","grants","number",
  "2021","under","rescue","ope","arp","heerf",
  "2003","in","fund","funding","opportunity","listing",
  "announcement","state","2023","section","plan","american",
  "national","other","needs","programs","notice",
  "2022","425e","425f","cfda","s","office","assistance","relief","higher","lost")


clean_list<-clean%>%
  filter(title_list%!in%stopwords)%>%
  group_by(title_list)%>%
  summarize(sm=sum(estimated_funding, na.rm=TRUE))%>%
  ungroup()%>%
  mutate(title_list=case_when(
    title_list=="postsecondary"~"post-secondary",
    title_list=="hiv"~"HIV",
    TRUE~title_list
  ))%>%
  mutate(sm_round=round(sm/1000000000))%>%
  arrange(-sm)%>%
  mutate(rk=row_number())%>%
  head(19)%>%
  mutate(
    x=10000000000,
    lab=glue::glue("**{title_list}** {sm_round} billion $"),
    lab2=glue::glue("**{title_list}** {title_list} {title_list} {title_list} {title_list} {title_list}")
  )%>%
  mutate(cum_sm=cumsum(sm))%>%
  mutate(fl=case_when(
    rk %% 2 == 0~"0",
    TRUE~"1"
  ))

sub<-tibble(
  x=0,
  lab="Funding granted for the period 2003 to 2023 ranked according to<br>**the words in the title** of the US government grant opportunities"
)

cap<-tibble(
  x=0,
  lab="**Data** Tidy Tuesday **| Plot** Benjamin Nowak"
)

# Set colors
#bck <- "#192A51"
bck <- "#0A3161"
  
bar <- "#ef476f"
lab <- "#ffd166"

tit<-"white"

#pal<-c("#ad336d","#f8997d")
# Changing color palette here to mimick US flag
pal<-rev(c("#B31942","#ffffff"))

ggplot(clean_list)+
  annotate(
    "text",x=0,y=200000000000,label="US Grant Opportunities",
    size=51,family="beb",hjust=0,color=tit
  )+
  geom_richtext(
    sub,mapping=aes(x=x,y=70000000000,label=lab),
    family="open",size=13,hjust=0,lineheight=0.4,
    fill = NA, label.color = NA, color=tit,
    label.padding = grid::unit(rep(0, 4), "pt") 
  ) +
  geom_richtext(
    cap,mapping=aes(x=x,y=-max(clean_list$cum_sm)-70000000000,label=lab),
    family="open",size=10,hjust=0,lineheight=0.45,
    fill = NA, label.color = NA, color=tit,
    label.padding = grid::unit(rep(0, 4), "pt") 
  ) +
  geom_rect(aes(xmin=0,xmax=1,ymin=-(cum_sm-sm),ymax=-cum_sm,fill=fl))+
  geom_richtext(
    aes(x=0.05,y=-cum_sm+sm/2,label=lab, size=sm,color=fl),
    family="ral",
    #size=12,
    hjust=0,lineheight=0.45,
    vjust=0.5, 
    #color="white",
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  ) +
  guides(fill='none',size='none',color="none")+
  scale_size(range=c(8,33))+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=rev(pal))+
  theme_void()+
  theme(
    plot.margin=margin(0.5,0.2,0,0.5,"cm"),
    plot.background = element_rect(fill=bck)
  )
