# Clear space 
#############
rm(list=ls())
gc()

# Load packages 
###############
library(tidyverse)
library(plyr)

tuesdata <- tidytuesdayR::tt_load('2021-05-18')
survey <- tuesdata$survey

survey_clean<-survey%>%
  filter(
    country=='United States'|
    country=='USA'|
    country=="US"|
    country=='U.S.'|
    country=='usa'|
    country=='Usa'|
    country=='United States of America'|
    country=='United State'|
    country=='U.S>'
  )%>%mutate(
    Experience=case_when(
      overall_years_of_professional_experience=="1 year or less"~"10 years or less",
      overall_years_of_professional_experience=="2 - 4 years"~"10 years or less",
      overall_years_of_professional_experience=="5-7 years" ~"10 years or less",
      overall_years_of_professional_experience=="8 - 10 years"~"10 years or less", 
      overall_years_of_professional_experience=="11 - 20 years"~"11 to 30 years",
      overall_years_of_professional_experience=="21 - 30 years"~"11 to 30 years",
      overall_years_of_professional_experience=="31 - 40 years"~"More than 30 years",
      overall_years_of_professional_experience=="41 years or more"~"More than 30 years",
      TRUE ~ "NA"
    )
  )%>%filter(
    gender=='Woman'|
    gender=='Man'
)%>%mutate(
  Education=case_when(
    highest_level_of_education_completed=="High School"~"High school or college", 
    highest_level_of_education_completed=="College degree"~"High school or college", 
    highest_level_of_education_completed=="Some college"~"High school or college",
    highest_level_of_education_completed=="Master's degree"~"Master or PhD", 
    highest_level_of_education_completed=="PhD"~"Master or PhD",  
    highest_level_of_education_completed=="Professional degree (MD, JD, etc.)"~"Professional degree",
    TRUE ~ "NA"
  )
)%>%ddply(
  .(highest_level_of_education_completed,Experience),
  summarize,
  salary_woman=mean(na.omit(annual_salary[gender=='Woman'])),
  salary_man=mean(na.omit(annual_salary[gender=='Man']))
)%>%na.omit()%>%
  mutate(highest_level_of_education_completed=
           fct_reorder(
             factor(highest_level_of_education_completed),salary_man)
  )%>%mutate(
    Gap=round(salary_man-salary_woman)
  )%>%mutate(
    ypos=(salary_man+salary_woman)/2
  )

ggplot(data=na.omit(survey_clean))+
  geom_segment( aes(x=highest_level_of_education_completed, xend=highest_level_of_education_completed, y=salary_woman, yend=salary_man), color="grey80",alpha=1,size=3.5) +
  geom_point( aes(x=highest_level_of_education_completed, y=salary_man), color="dodgerblue", size=3.5 ) +
  geom_point( aes(x=highest_level_of_education_completed, y=salary_woman), color="firebrick", size=3.5 ) +
  facet_grid(Experience~.)+
  coord_flip()+
  labs(
    y="Annual salary ($)",
    x="",
    title="Salary gap between women and men depending on education and experience",
    subtitle="Data extracted from 'Ask a Manager Survey' for the US only (about 20k answers)"
  )+
  geom_text(
    data=filter(survey_clean,highest_level_of_education_completed=="College degree"&Experience=="More than 30 years"),
    aes(x=highest_level_of_education_completed, y=salary_woman),nudge_y=-10000,nudge_x=0.2, label="Woman", color="firebrick", size=4 ) +
  geom_text(
    data=filter(survey_clean,highest_level_of_education_completed=="College degree"&Experience=="More than 30 years"),
    aes(x=highest_level_of_education_completed, y=salary_man),nudge_y=10000,nudge_x=0.2, label="Man", color="dodgerblue", size=4 ) +
  
  geom_text(aes(x=highest_level_of_education_completed,y=ypos,label=paste(Gap,'$')),size=2.5)+
  theme_minimal()

