# Clear space 
rm(list=ls())
gc()

# Load data
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

# Load extensions
library(tidyverse)
library(tidygraph)
library(ggraph)
library(patchwork)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(gender)
# If 1st time with {gender}: 
# remotes::install_github("lmullen/genderdata")
library(genderdata)

# Set plot parameters
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21*1.618, 
  height = 21, 
  #width = 10*1.618, 
  #height = 10, 
  units = "cm", 
  dpi = 300 
)


font_add_google(name = "Fjalla One", family = "fjalla")
font_add_google(name = "Roboto Condensed", family = "roboto")
showtext_auto()

tit<-'fjalla'
tex<-'roboto'

col_women <- '#F42C04' 
col_men <- '#228176'
col_unknown <- '#ADA296'

thm <- theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
  ) 


# Select paper years and add gender
# (Whole process may be simplified by a function f(year)...)

# 73
paper_authors_73<-paper_authors%>%
  left_join(papers)%>%
  left_join(authors)%>%
  filter(year==1973)%>%
  separate(name,c("given",NA))
name_73<-gender(paper_authors_73$given, years = c(1973-60,1973-30))
paper_authors_73<-paper_authors_73%>%
  left_join(name_73,by= c("given"="name"))

#1980
paper_authors_80<-paper_authors%>%
  left_join(papers)%>%
  left_join(authors)%>%
  filter(year==1980)%>%
  separate(name,c("given",NA))
name_80<-gender(paper_authors_80$given, years = c(1980-60,1980-30))
paper_authors_80<-paper_authors_80%>%
  left_join(name_80,by= c("given"="name"))

#1990
paper_authors_90<-paper_authors%>%
  left_join(papers)%>%
  left_join(authors)%>%
  filter(year==1990)%>%
  separate(name,c("given",NA))
name_90<-gender(paper_authors_90$given, years = c(1990-60,1990-30))
paper_authors_90<-paper_authors_90%>%
  left_join(name_90,by= c("given"="name"))

#2000
paper_authors_00<-paper_authors%>%
  left_join(papers)%>%
  left_join(authors)%>%
  filter(year==2000)%>%
  separate(name,c("given",NA))
name_00<-gender(paper_authors_00$given, years = c(2000-60,2000-30))
paper_authors_00<-paper_authors_00%>%
  left_join(name_00,by= c("given"="name"))%>%
  select(author,paper,gender)

#2010
paper_authors_10<-paper_authors%>%
  left_join(papers)%>%
  left_join(authors)%>%
  filter(year==2010)%>%
  separate(name,c("given",NA))
name_10<-gender(paper_authors_10$given, years = c(2010-60,2010-30))
paper_authors_10<-paper_authors_10%>%
  left_join(name_10,by= c("given"="name"))%>%
  select(author,paper,gender)

#2020
paper_authors_20<-paper_authors%>%
  left_join(papers)%>%
  left_join(authors)%>%
  filter(year==2020)%>%
  separate(name,c("given",NA))
name_20<-gender(paper_authors_20$given, years = c(2020-60,2020-30))
paper_authors_20<-paper_authors_20%>%
  left_join(name_20,by= c("given"="name"))%>%
  select(author,paper,gender)

# Clever way to prepare for networks:
# https://stackoverflow.com/questions/50525722/co-authorship-network-edge-list-for-igraph-r
# 73
routes_73 <- paper_authors_73 %>%
  inner_join(paper_authors_73, by='paper')%>%
  filter(author.x < author.y)%>%
  count(author.x, author.y)%>%
  mutate(
    from=author.x,
    to=author.y
  )%>%
  select(from,to)

# 80
routes_80 <- paper_authors_80 %>%
  inner_join(paper_authors_80, by='paper')%>%
  filter(author.x < author.y)%>%
  count(author.x, author.y)%>%
  mutate(
    from=author.x,
    to=author.y
  )%>%
  select(from,to)

# 90
routes_90 <- paper_authors_90 %>%
  inner_join(paper_authors_90, by='paper')%>%
  filter(author.x < author.y)%>%
  count(author.x, author.y)%>%
  mutate(
    from=author.x,
    to=author.y
  )%>%
  select(from,to)

# 00
routes_00 <- paper_authors_00 %>%
  inner_join(paper_authors_00, by='paper')%>%
  filter(author.x < author.y)%>%
  count(author.x, author.y)%>%
  mutate(
    from=author.x,
    to=author.y
  )%>%
  select(from,to)

# 10
routes_10 <- paper_authors_10 %>%
  inner_join(paper_authors_10, by='paper')%>%
  filter(author.x < author.y)%>%
  count(author.x, author.y)%>%
  mutate(
    from=author.x,
    to=author.y
  )%>%
  select(from,to)

# 20
routes_20 <- paper_authors_20 %>%
  inner_join(paper_authors_20, by='paper')%>%
  filter(author.x < author.y)%>%
  count(author.x, author.y)%>%
  mutate(
    from=author.x,
    to=author.y
  )%>%
  select(from,to)

# Make network
# https://rviews.rstudio.com/2019/03/06/intro-to-graph-analysis/
graph_routes_73 <- as_tbl_graph(routes_73)
graph_routes_80 <- as_tbl_graph(routes_80)
graph_routes_90 <- as_tbl_graph(routes_90)
graph_routes_00 <- as_tbl_graph(routes_00)
graph_routes_10 <- as_tbl_graph(routes_10)
graph_routes_20 <- as_tbl_graph(routes_20)

# Join gender to nodes
# 73
graph_routes_73<-graph_routes_73%>%
  activate(nodes)%>%
  mutate(author=name)%>%
  left_join(paper_authors_73%>%distinct(author,.keep_all=TRUE))
# 80
graph_routes_80<-graph_routes_80%>%
  activate(nodes)%>%
  mutate(author=name)%>%
  left_join(paper_authors_80%>%distinct(author,.keep_all=TRUE))
# 90
graph_routes_90<-graph_routes_90%>%
  activate(nodes)%>%
  mutate(author=name)%>%
  left_join(paper_authors_90%>%distinct(author,.keep_all=TRUE))
# 00
graph_routes_00<-graph_routes_00%>%
  activate(nodes)%>%
  mutate(author=name)%>%
  left_join(paper_authors_00%>%distinct(author,.keep_all=TRUE))
# 10
graph_routes_10<-graph_routes_10%>%
  activate(nodes)%>%
  mutate(author=name)%>%
  left_join(paper_authors_10%>%distinct(author,.keep_all=TRUE))
# 20
graph_routes_20<-graph_routes_20%>%
  activate(nodes)%>%
  mutate(author=name)%>%
  left_join(paper_authors_20%>%distinct(author,.keep_all=TRUE))

# Plot networks
p73 <- graph_routes_73 %>%
  ggraph(layout = "kk") +
  geom_node_point(aes(color=gender)) +
  scale_color_manual(values=c('male'=col_men,'female'=col_women),na.value=col_unknown)+
  geom_edge_diagonal(color = "dimgrey", alpha = 0.8)+
  thm

p80 <- graph_routes_80 %>%
  ggraph(layout = "kk") +
  geom_node_point(aes(color=gender)) +
  scale_color_manual(values=c('male'=col_men,'female'=col_women),na.value=col_unknown)+
  geom_edge_diagonal(color = "dimgrey", alpha = 0.8)+
  thm

p90 <- graph_routes_90 %>%
  ggraph(layout = "kk") +
  geom_node_point(aes(color=gender)) +
  scale_color_manual(values=c('male'=col_men,'female'=col_women),na.value=col_unknown)+
  geom_edge_diagonal(color = "dimgrey", alpha = 0.8)+
  thm

p00 <- graph_routes_00 %>%
  ggraph(layout = "kk") +
  geom_node_point(aes(color=gender)) +
  scale_color_manual(values=c('male'=col_men,'female'=col_women),na.value=col_unknown)+
  geom_edge_diagonal(color = "dimgrey", alpha = 0.8)+
  thm

p10 <- graph_routes_10 %>%
  ggraph(layout = "kk") +
  geom_node_point(aes(color=gender)) +
  geom_edge_diagonal(color = "dimgrey", alpha = 0.2)+
  scale_color_manual(values=c('male'=col_men,'female'=col_women),na.value=col_unknown)+
  thm

p20 <- graph_routes_20 %>%
  ggraph(layout = "kk") +
  geom_node_point(aes(color=gender)) +
  geom_edge_diagonal(color = "dimgrey", alpha = 0.2)+
  scale_color_manual(values=c('male'=col_men,'female'=col_women),na.value=col_unknown)+
  thm

# Assemble plots
layout=c(
  area(t=1,l=1,b=2,r=2),
  area(t=1,l=3,b=2,r=4),
  area(t=1,l=5,b=2,r=6),
  area(t=3,l=1,b=4,r=2),
  area(t=3,l=3,b=4,r=4),
  area(t=3,l=5,b=4,r=6)
)

p73+p80+p90+
  p00+p10+p20+
  plot_layout(design=layout)+
  plot_annotation(
    title = "Evolution of co-autorship networks for National Bureau of Economic Research papers",
    subtitle = glue("<span>Each graph shows papers published in one year, each point is an author (</span><b style='color: #F42C04'>woman</b><span>, </span><b style='color: #228176'>man</b><span> or </span><b style='color: #ADA296'>unknow</b><span>) and lines show<br/>co-publications between these authors<br/></span> "),
    caption="Data: TidyTuesday & Ben Davies | Plot: @BjnNowak",
    tag_levels = list(c('1973', '1980','1990','2000','2010','2020')),
    theme = theme(
      plot.title = element_text(size=80,family = tit,color = "grey15"),
      plot.subtitle = element_markdown(size=60,family = tex, lineheight=0.4),
      plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
      plot.caption.position = "panel",
      plot.caption = element_text(family=tex,size=40)
    )
  )&
  theme(
    plot.tag = element_text(size = 60,family = tit,color = "grey25")
  )
