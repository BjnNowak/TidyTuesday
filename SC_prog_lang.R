library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)
library(ggforce)
library(glue)


# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Jost","jo")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 36, 
  units = "cm", 
  dpi = 300 
)


languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

# Vector with top language names
top <- languages%>%arrange(-github_repo_subscribers)%>%head(100)%>%pull(title)

# make tibble for individual lines
ln <- languages%>%
  filter(title%in%top)%>%
  filter(github_repo_issues>0)%>%
  select(title,github_repo_stars,github_repo_subscribers,github_repo_forks,github_repo_issues)%>%
  pivot_longer(!title,names_to="action",values_to="value")%>%
  mutate(x=case_when(
    action=="github_repo_stars"~1,
    action=="github_repo_subscribers"~2,
    action=="github_repo_forks"~3,
    action=="github_repo_issues"~4,
  ))

ln_sta <- ln%>%
  filter(action=="github_repo_stars")%>%
  mutate(x=0.5)

ln_iss <- ln%>%
  filter(action=="github_repo_issues")%>%
  mutate(x=4.5)

ln_full <- ln%>%
  bind_rows(ln_sta)%>%
  bind_rows(ln_iss)

# Create mean line
ln_res <- ln%>%
  group_by(action)%>%
  summarize(
    value=mean(value),
    x=mean(x)
  )

ln_res_sta <- ln_res%>%
  filter(action=="github_repo_stars")%>%
  mutate(x=0.5)

ln_res_iss <- ln_res%>%
  filter(action=="github_repo_issues")%>%
  mutate(x=4.5)

ln_res_full <- ln_res%>%
  bind_rows(ln_res_sta)%>%
  bind_rows(ln_res_iss)

# labels for mean line
ln_res_lab <- ln_res%>%
  mutate(rd=round(value))%>%
  mutate(type=case_when(
    action=="github_repo_stars"~"stars",
    action=="github_repo_subscribers"~"subscribers",
    action=="github_repo_forks"~"forks",
    action=="github_repo_issues"~"issues"
  ))%>%
  mutate(label=glue::glue("Average of<br><span style='font-size:90pt;'>**{rd} {type}**</span><br>per repository"))

# Make plot
###########
alpha_light = 0.1
col_light = "#03DAC6"
col_high = "#03DAC6"
lin_low = 0.5
lin_high = 2
back = "#121212"

# Color for light mode
#col_light = "#1B9CFE"
#col_high = "#1B9CFE"

ggplot(ln_full)+
  geom_line(
    aes(x=x,y=log(value),group=title),
    alpha=alpha_light,col=col_light)+
  geom_text(
    ln_full%>%filter(x==0.5),
    mapping=aes(x=x,y=log(value),label=title),size=6,
    hjust=0,vjust=0,family="cond",
    color=col_light,alpha=alpha_light
  )+
  geom_text(
    ln_full%>%filter(x==4.5),
    mapping=aes(x=x,y=log(value),label=title),size=6,
    hjust=1,vjust=0,family="cond",
    color=col_light,alpha=alpha_light
  )+
  geom_line(
    ln_res_full,
    mapping=aes(x=x,y=log(value)),
    alpha=1,col=col_high,linewidth=lin_high)+
  geom_richtext(
    data=ln_res_lab,
    aes(x=x,y=0.5,label=label),
    size=14,family="jo",color=col_high,lineheight=0.35,
    angle=90,hjust=0,vjust=0.5,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  scale_x_continuous(limits=c(0,5))+
  theme_void()+
  theme(plot.background = element_rect(fill=back,color=NA))

