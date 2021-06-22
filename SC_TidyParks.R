
# Clear space 
rm(list=ls())
gc()

###########################################################################

# Load packages
library(tidyverse)
library(pdftools)
library(formattable)
library(showtext)
library(magrittr)
library(knitr)
library(kableExtra)
library(plyr)

# Load data
tuesdata <- tidytuesdayR::tt_load('2021-06-22')
parks <- tuesdata$parks

# Using color_tile with more than two colors: 
my_color_tile <- function() {
  return_col <- function(y) 
    map_chr(y,function(x) case_when(x > 90  ~ "#006837",
                                    x > 80  ~ "#1A9850",
                                    x > 70  ~ "#66BD63",
                                    x > 60  ~ "#A6D96A",
                                    x >= 50  ~ "#D9EF8B",
                                    x >= 40  ~ "#FEE08B",
                                    x >= 30  ~ "#FDAE61",
                                    x >= 20  ~ "#F46D43",
                                    x >= 10  ~ "#D73027",
                                    x >= 0  ~ "#A50026"
    ))
  formatter("span", 
            style = function(y) style(
              display = "block",
              padding = "5 5px",
              "border-radius" = "10px",
              "color" = ifelse( return_col(y) %in% c("#A50026","#D73027","#F46D43","#006837","#1A9850","#66BD63"),
                                csscolor("white"), csscolor("black")),
              "background-color" = return_col(y)
            )
  )
}

# Scaling for colorbar
# https://www.displayr.com/formattable/
unit.scale = function(x) (x - min(x)) / (max(x) - min(x))

# Computing ranking progress from 2019 to 2020
prog<-plyr::ddply(
  parks,
  .(city),
  summarize,
  rank_20=rank[year=="2020"],
  rank_19=rank[year=="2019"]
  )%>%
  mutate(
    Progress=rank_19-rank_20
  )

# Chart:
tidy_parks<-parks %>% 
  filter(year=='2020') %>%
  merge(prog,by='city')%>%
  arrange(rank)%>%
  mutate(city=case_when(
    city=="Charlotte/Mecklenburg County"~"Charlotte County",
    TRUE~city
  ))%>%
  mutate(
    'Score (%)' = color_bar('lightgrey',fun = unit.scale)(round(total_pct)),
    Rank=cell_spec(rank,"html",color = "dimgrey",align="center",bold=T),
    Evolution=
      case_when(
        Progress>0~cell_spec(Progress,"html",color = "forestgreen",align="center",bold=T),
        Progress==0~cell_spec(Progress,"html",color = "dimgrey",align="center",bold=T),
        Progress<0~cell_spec(Progress,"html",color = "red",align="center",bold=T)
      ),
    City=cell_spec(city,"html",color = "dimgrey",align="center",bold=T),
    Surface=my_color_tile()(park_pct_city_points),
    Proximity=my_color_tile()(pct_near_park_points),
    Spending=my_color_tile()(spend_per_resident_points),
    Basketball=my_color_tile()(basketball_points),
    Dogpark=my_color_tile()(dogpark_points),
    Playground=my_color_tile()(playground_points),
    Senior=my_color_tile()(rec_sr_points),
    Restroom=my_color_tile()(restroom_points),
    Splashpad=my_color_tile()(splashground_points)
  )%>%
  select('Score (%)',Rank,Evolution,City,Surface,Proximity,Spending,Basketball,Dogpark,Playground,Senior,Restroom,Splashpad)%>%
  kable(
    "html", escape = F,align=c("rcclccccccccc"),
    #caption = "<center><strong><big>>US cities ranking based on the quality of their parks</big></strong></center>"
  ) %>%
  kable_styling(bootstrap_options =c("hover","condensed"), full_width = T) %>%
  #row_spec(0, angle = -45)%>%
  add_header_above(c("\nGlobal score for 2020\nand ranking evolution since 2019"=4, "\nScore per category\nfrom 1 (poor quality) to 100 (high quality)" = 9))%>%
  column_spec(1,width_min='5cm')%>%
  column_spec(4,width_min='4cm')%>%
  column_spec(5:13,width_min='3cm')%>%
  footnote(general = "<strong>Source:</strong> The Trust for public land from Tidy Tuesday <strong>| Author:</strong> @BjnNowak",general_title = "",escape=F)
tidy_parks

# Save:
#webshot::install_phantomjs()
save_kable(tidy_parks,file="Tidy_Parks5.png")
