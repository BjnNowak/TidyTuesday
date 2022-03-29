
library(tidyverse)
library(broom)
library(ggrepel)
library(patchwork)
library(ggtext)
library(glue)
library(camcorder)
library(showtext)


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 15, 
  units = "cm", 
  dpi = 300 
)

# Fonts
font_add_google("Open Sans","open")
font_add_google("Bebas Neue","bebas")
font_add_google("League Spartan","spart")
font_add_google("Fira Sans","fira")
font_add_google("Fira Sans Extra Condensed","cond")

showtext_auto()

# Load data
sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

# Prep data
###########

# Women only
data_female <- sports%>%
  mutate(partic_women = replace_na(partic_women, 0))%>%
  mutate(partic_men = replace_na(partic_men, 0))%>%
  mutate(per_female=partic_women/ef_female_count*100)%>%
  mutate(per_male=partic_men/ef_male_count*100)%>%
  mutate(per_partic=(partic_women+partic_men)/(ef_male_count+ef_female_count)*100)%>%
  select(institution_name,year,per_female,sports)%>%
  group_by(institution_name,sports)%>%
  summarize(per=mean(per_female))

# Men only
data_male <- sports%>%
  mutate(partic_men = replace_na(partic_men, 0))%>%
  mutate(per_male=partic_men/ef_male_count*100)%>%
  select(institution_name,year,per_male,sports)%>%
  group_by(institution_name,sports)%>%
  summarize(per=mean(per_male))

# Only keep most popular sports
###############################
most_pop <- data%>%
  filter_all(all_vars(!is.infinite(.)))%>%
  group_by(sports)%>%
  summarize(per_all=mean(na.omit(per)))%>%
  head(15)%>%
  pull(sports)

# Women 
label_female<- data_female%>%
  filter(sports != "Track and Field, Outdoor")%>%
  filter(sports != "Track and Field, Indoor")%>%
  filter(sports != "Track and Field, X-Country")%>%
  filter(sports != "Swimming and Diving")%>%
  filter_all(all_vars(!is.infinite(.)))%>%
  group_by(sports)%>%
  summarize(per_all=mean(na.omit(per)))%>%
  arrange(-per_all)%>%
  head(15)%>%
  mutate(per_round=round(per_all,1))%>%
  mutate(lab=case_when(
    sports=="All Track Combined"~glue("**Tracks**<br>{per_round}%"),
    TRUE~glue("**{sports}**<br>{per_round}%")  
  ))%>%
  select(sports,lab)

most_pop_female<-label_female%>%  
  pull(sports)

# Men
label_male<- data_male%>%
  filter(sports != "Track and Field, Outdoor")%>%
  filter(sports != "Track and Field, Indoor")%>%
  filter(sports != "Track and Field, X-Country")%>%
  filter(sports != "Swimming and Diving")%>%
  filter_all(all_vars(!is.infinite(.)))%>%
  group_by(sports)%>%
  summarize(per_all=mean(na.omit(per)))%>%
  arrange(-per_all)%>%
  head(15)%>%
  mutate(per_round=round(per_all,1))%>%
  mutate(lab=case_when(
    sports=="All Track Combined"~glue("**Tracks**<br>{per_round}%"),
    TRUE~glue("**{sports}**<br>{per_round}%")  
  ))%>%
  select(sports,lab)

most_pop_male<-label_male%>%  
  pull(sports)



# From long to wide
###################
data_wide <- data%>%
  ungroup()%>%
  filter(sports %in% most_pop)%>%
  pivot_wider(names_from = sports,values_from=per)%>%
  # Filter all rows with Inf or NA values
  mutate(across(everything(), ~replace_na(.x, 0)))%>%
  filter_all(all_vars(!is.infinite(.)))
data_wide

# Women
data_wide_female <- data_female%>%
  ungroup()%>%
  filter(sports %in% most_pop_female)%>%
  pivot_wider(names_from = sports,values_from=per)%>%
  # Filter all rows with Inf or NA values
  mutate(across(everything(), ~replace_na(.x, 0)))%>%
  filter_all(all_vars(!is.infinite(.)))

# Men
data_wide_male <- data_male%>%
  ungroup()%>%
  filter(sports %in% most_pop_male)%>%
  pivot_wider(names_from = sports,values_from=per)%>%
  # Filter all rows with Inf or NA values
  mutate(across(everything(), ~replace_na(.x, 0)))%>%
  filter_all(all_vars(!is.infinite(.)))

# Perform PCA
#############

# Women
PCA_female <-data_wide_female%>%
  select(-institution_name)%>%
  # Perform PCA with scaled variables
  prcomp(scale = TRUE)

PCA_female%>%
  tidy(matrix = "eigenvalues")

# Add 'year' variable to plot results 
PCA_female_indiv<-PCA_female%>%
  broom::augment(data_wide_female)

# Men
PCA_male <-data_wide_male%>%
  select(-institution_name)%>%
  # Perform PCA with scaled variables
  prcomp(scale = TRUE)

PCA_male%>%
  tidy(matrix = "eigenvalues")

# Add 'year' variable to plot results 
PCA_male_indiv<-PCA_male%>%
  broom::augment(data_wide_male)

# Plot of individuals
#####################
ggplot(
  data=PCA_female_indiv,
  aes(.fittedPC1, .fittedPC2))+
  geom_point()+
  labs(
    title = 'Plot of individuals',
    subtitle = 'Color shows year of song creation',
    x='PC1 (25%)',
    y='PC2 (12%)',
    color='Year'
  )+
  theme_minimal()

ggplot(
  data=PCA_male_indiv,
  aes(.fittedPC1, .fittedPC2))+
  geom_point()+
  labs(
    title = 'Plot of individuals',
    subtitle = 'Color shows year of song creation',
    x='PC1 (24%)',
    y='PC2 (9%)',
    color='Year'
  )+
  theme_minimal()

# Plot of variables

PCA_female_var<-PCA_female %>%
  # Extract variable coordinates
  tidy(matrix = "rotation") %>%
  # Format table form long to wide
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value")%>%
  # Rename column with variable names
  rename(Variable=column)%>%
  # 'Clean' variable names 
  # Upper case on first letter
  mutate(Variable=stringr::str_to_title(Variable))%>%
  # Change '_' for space
  mutate(Variable=stringr::str_replace_all(Variable,"_"," "))%>%
  left_join(label_female,by=c('Variable'='sports'))%>%
  mutate(PC1_lab=case_when(
    Variable=='Swimming'~(PC1-0.03),
    Variable=='Soccer'~(PC1-0.04),
    Variable=='Basketball'~(PC1+0.03),
    Variable=='Rowing'~(PC1-0.02),
    Variable=='Softball'~(PC1-0.02),
    TRUE~PC1
  ))%>%
  mutate(PC2_lab=case_when(
    Variable=='Swimming'~(PC2+0.01),
    Variable=='Basketball'~(PC2-0.02),
    Variable=='Bowling'~(PC2-0.03),
    Variable=='Other Sports'~(PC2+0.01),
    Variable=='Rowing'~(PC2+0.02),
    Variable=='Softball'~(PC2+0.02),
    TRUE~PC2
  ))

PCA_male_var<-PCA_male %>%
  # Extract variable coordinates
  tidy(matrix = "rotation") %>%
  # Format table form long to wide
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value")%>%
  # Rename column with variable names
  rename(Variable=column)%>%
  # 'Clean' variable names 
  # Upper case on first letter
  mutate(Variable=stringr::str_to_title(Variable))%>%
  # Change '_' for space
  mutate(Variable=stringr::str_replace_all(Variable,"_"," "))%>%
  left_join(label_male,by=c('Variable'='sports'))%>%
  mutate(PC1_lab=case_when(
    Variable=='Rodeo'~(PC1-0.02),
    Variable=='Tennis'~(PC1-0.03),
    Variable=='Football'~(PC1+0.03),
    Variable=='Wrestling'~(PC1-0.03),
    TRUE~PC1
  ))%>%
  mutate(PC2_lab=case_when(
    Variable=='Rodeo'~(PC2-0.03),
    Variable=='Tennis'~(PC2+0.03),
    Variable=='Football'~(PC2-0.03),
    Variable=='Wrestling'~(PC2+0.03),
    Variable=='Bowling'~(PC2-0.03),
    TRUE~PC2
  ))

head(PCA_male_var)


# Prep plot 

col_fem <- '#a8201a'
col_male <- '#36558F'

caption <- tibble(
  x=0.6,
  y=-0.6,
  lab="**Data:** Equity in Athletics **<br>Plot:** @BjnNowak ",
  color="black"
)

lab1 <- tibble(
  x=-0.6,
  y=0.325,
  lab="
*Figures give the<br>
average percentage<br> 
of participants<br>
for each sport*",
  color="black"
)

lab2 <- tibble(
  x=0.6,
  y=0.525,
  lab="
*Lacrosse and<br>
swimming are<br>
often popular<br>
in the same<br>
universities*",
  color="black"
)

lab3 <- tibble(
  x=0,
  y=-0.55,
  lab="
*Rodeo and<br>
hockey are<br>
rarely practiced<br>
in the same<br>
universities*",
  color="black"
)

var_female<-ggplot(data=PCA_female_var,aes(PC1, PC2)) +
  # Add variables arrows
  geom_segment(
    xend = 0, yend = 0,
    color="grey80",
    arrow = arrow(
      length = unit(0.03, "npc"),
      ends = "first"
    )
  )+
  # Add variables names
  geom_richtext(
    data=PCA_female_var,
    aes(x=PC1_lab,y=PC2_lab,label=lab),
    inherit.aes = FALSE,
    size=8,lineheight=0.35,family='cond',alpha=0.75,
    color=col_fem,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  annotate(
    geom = 'text',x=-0.6,y=0.6,label='Women',
    size=14,family='fira',hjust=0,fontface='bold',color=col_fem
  )+
  geom_richtext(
    lab1,mapping=aes(x=x,y=y,label=lab),
    inherit.aes = FALSE,
    size=9, family='cond',hjust=0,lineheight=0.45,vjust=1,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  geom_richtext(
    lab3,mapping=aes(x=x,y=y,label=lab),
    inherit.aes = FALSE,
    size=9, family='cond',hjust=1,lineheight=0.45,vjust=0,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  coord_fixed()+
  scale_y_continuous(limits=c(-0.6,0.6))+
  scale_x_continuous(limits=c(-0.6,0.0))+
  labs(
    x='PC1 (25%)',
    y='PC2 (12%)'
  )+
  theme_void()+
  theme(
    plot.title = element_blank()
  )

var_male<-ggplot(data=PCA_male_var,aes(-PC1, PC2)) +
  # Add variables arrows
  geom_segment(
    xend = 0, yend = 0, 
    color="grey80",
    arrow = arrow(
      length = unit(0.03, "npc"),
      ends = "first"
    )
  )+
  # Add variables names
  geom_richtext(
    data=PCA_male_var,
    aes(x=-PC1_lab,y=PC2_lab,label=lab),
    inherit.aes = FALSE,
    size=8,lineheight=0.35,alpha=0.75,
    color=col_male,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  annotate(
    geom = 'text',x=0.6,y=0.6,label='Men',
    size=14,family='fira',hjust=1,fontface='bold',col=col_male,
  )+
  geom_richtext(
    caption,mapping=aes(x=x,y=y,label=lab),
    inherit.aes = FALSE,
    size=8, family='cond',hjust=1,lineheight=0.45,vjust=0,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+
  geom_richtext(
    lab2,mapping=aes(x=x,y=y,label=lab),
    inherit.aes = FALSE,
    size=9, family='cond',hjust=1,lineheight=0.45,vjust=1,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+

  coord_fixed()+
  scale_y_continuous(limits=c(-0.6,0.6))+
  scale_x_continuous(limits=c(-0.0,0.6))+
  labs(
    x='PC1 (24%)',
    y='PC2 (9%)'
  )+
  theme_void()+
  theme(
    plot.title = element_blank()
  )

# Assemble plots
var_female+var_male+plot_annotation(
  title = "Most popular sports combinations in US colleges",
  subtitle = 
'The graphs of variables below present the results of the principal component analyses performed\n
based on the percentage of participants for the different sports practiced in each university.\n
The closest sports are those most likely to be popular in the same university.',
  #caption = '     Data: H. Wickham | Plot: @BjnNowak'
)&
  theme(
    plot.margin = margin(0.5,0,0,0,unit='cm'),
    plot.background = element_rect(fill="#FCFBED",color=NA),
    plot.title = element_text(size=48,hjust=0.5,family = "spart",face='bold',color='#050505'),
    plot.subtitle = element_text(size=30,hjust=0.5,family = "cond",lineheight=0.2,margin=margin(0.15,0,0,0,'cm')),
    plot.caption = element_text(size=20,hjust=0,family='open'),
    plot.caption.position = "panel"
  )
