
# Clear space 
rm(list=ls())
gc()

library(tidyverse)
library(gt)
library(gtExtras)

# Load data: 
# Whole race winners
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')
# Stages winners
tdf_stages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv')

# Data preparation:
most_wins<-tdf_winners%>%
  # Remove Armstrong (convicted for drug use)
  filter(winner_name!="Lance Armstrong")%>%
  # Keep only one spelling for Indurain
  mutate(winner_name=case_when(
    winner_name=='Miguel Induráin'~'Miguel Indurain',
    TRUE~winner_name
  ))%>%
  # Add variable to count titles
  mutate(ct=1)%>%
  # Group by winner name
  group_by(winner_name)%>%
  summarize(
    # Count titles
    Titles=sum(ct),
    # Add nationality
    Country=nationality[1],
    # Add nickname
    Nickname=nickname[1])%>%
  # Keep only winners with 3 titles or more
  filter(Titles>2)%>%
  # Sort by descending order
  arrange(-Titles)%>%
  # Ordering columns
  select(
    Rider=winner_name,
    Nickname,Country,Titles)%>%
  # Cleaning nicknames
  mutate(Nickname=case_when(
    str_detect(Rider,'Hinault')~'The Badger',
    str_detect(Rider,'Anquetil')~'Maître Jacques',
    str_detect(Rider,'Indurain')~'Miguelón',
    str_detect(Rider,'LeMond')~"The American",
    str_detect(Rider,'Bobet')~'Zonzon',
    str_detect(Rider,'Thys')~'The Basset Hound',
    TRUE~Nickname
  ))%>%
  mutate(Country = case_when(
    str_detect(Country,'France') ~ 'https://raw.githubusercontent.com/BjnNowak/TdF/main/fr.png',
    str_detect(Country,'Belgium') ~ 'https://raw.githubusercontent.com/BjnNowak/TdF/main/be.png',
    str_detect(Country,'Great Britain') ~ 'https://raw.githubusercontent.com/BjnNowak/TdF/main/uk.png',
    str_detect(Country,'Spain') ~ 'https://raw.githubusercontent.com/BjnNowak/TdF/main/sp.png',
    str_detect(Country,'United States') ~ 'https://raw.githubusercontent.com/BjnNowak/TdF/main/us.png'
  ))

# Create a vector with names of riders with most wins 
names_most_wins<- most_wins %>%
  pull(Rider)

year_wins<-tdf_winners%>%
  # Rider column with one spelling for Indurain
  mutate(Rider=case_when(
    winner_name=='Miguel Induráin'~'Miguel Indurain',
    TRUE~winner_name
  ))%>%
  # Add ct variable to count years, 
  # with 1 for year with a title 
  mutate(ct=1)%>%
  # ... and create new rows with ct=0 
  # for years with no title 
  complete(Rider, edition, fill = list(ct = 0))%>%
  group_by(Rider)%>%
  # Create list for each rider
  summarise(Timeline = list(ct))%>%
  filter(Rider %in% names_most_wins)

most_stages<- tdf_stages %>%
  mutate(Rider=case_when(
    Winner=='Miguel Induráin'~'Miguel Indurain',
    TRUE~Winner
  ))%>%
  filter(Rider %in% names_most_wins)%>%
  # Keep only 3 types of stages:
  # Time trial, mountain or plain
  mutate(TypeClean = case_when(
    str_detect(Type,"trial")~"Time trial",
    str_detect(Type,"mountain")~"Mountain stage",
    str_detect(Type,"Mountain")~"Mountain stage",
    str_detect(Type,"Hilly")~"Mountain stage",
    TRUE~"Plain stage"
  ))%>%
  group_by(Rider,TypeClean) %>%
  mutate(ct=1) %>%
  summarize(
    Wins=sum(ct)
  )%>%
  ungroup()%>%
  # Complete with NA for empty couples {rider*type of stages} 
  complete(Rider, TypeClean, fill = list(Wins = NA)) %>% 
  group_by(Rider)%>%
  summarise(Stages = list(Wins))


# Make table

# Set color palette
pal_stages <- c('#264653','#e9c46a','#e76f51')

tab<-most_wins%>%
  # Join tables
  left_join(year_wins)%>%
  left_join(most_stages)%>%
  # Make table
  gt()%>%
  # Set title
  tab_header(
    title = "Most sucessful riders in the Tour de France"
  )%>%
  # Set theme
  # Merge riders' name and nickname on same column
  gtExtras::gt_merge_stack(col1 = Rider, col2 = Nickname)%>%
  # Add flag images
  gtExtras::gt_img_rows(columns = Country, height = 15)%>%
  # Add yellow jerseys
  gtExtras::gt_fa_repeats(
    column=Titles,palette = "orange",
    name = "tshirt",align='left'
  )%>%
  # Add timeline
  gtExtras::gt_sparkline(
    Timeline, range_colors=c("#ABB4C4","#ef233c"),
    line_color="#DBDFE6"
  )%>%
  gt_plt_bar_stack(
    column=Stages, position = 'stack', 
    labels = c("Mountain stage", "Plain stage", "Time trial"),
    palette = pal_stages, width = 60, trim=TRUE
  )%>%
  cols_label(
    Titles = md("Number<br>of titles"),
    Country = "Country",
    Timeline = md("Titles<br/>timeline")
  )%>%
  cols_align(
    align = "center",
    columns = c(Country,Titles)
  )%>%
  tab_spanner(
    label = "Stages won",
    columns = c(Stages)
  )%>%
  tab_header(
    title = "Les forçats de la route",
    subtitle = md("*Les forçats de la route*, translated as *Convicts on the road*, is a report by Albert Londres about the **Tour de France** 1924, an annual men's multiple-stage bicycle contest. In this race across France, the leader is designated with **the yellow jersey**. The first race was organized in 1903 and in 108 editions, **only eight riders have won three or more titles.**")
  )%>%
  tab_source_note(
    source_note = md("**Data:** Alastair Rushworth & TidyTuesday | **Table:** @BjnNowak")
  )%>%
  tab_footnote(
    footnote = md("Race not contested from 1915 to 1918 and 1940 to 1946 due to World Wars.<br>Lance Armstrong's wins from 1999 to 2005 were removed due to drugs use, with no alternative winners for those years."),
    locations = cells_title(groups = "subtitle")
  )%>%
  
  # Style options
  # Title
  tab_style(
    style = list(
      cell_text(font=google_font(
        #name = "Roboto Condensed"
        name = "Playfair Display"), weight='800',align = "left",color='#203B46')),
    locations = cells_title(groups = "title")
  )%>%
  # Subtitle
  tab_style(
    style = list(
      cell_text(font=google_font(
        #name = "Roboto Condensed"
        name = "Roboto"), align = "left")),
    locations = cells_title(groups = "subtitle")
  )%>%
  # Header
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
        #name = "Roboto"
      ), align = "left",v_align = "middle")),
    locations = cells_column_labels(
      columns = c(
        Rider,Stages)
    )
  )%>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
        #name = "Roboto"
      ), align = "center",v_align = "middle")),
    locations = cells_column_labels(
      columns = c(
        Country,Titles,Timeline)
    )
  )%>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
        #name = "Roboto"
      ), align = "center",size='small')),
    locations = cells_column_labels(
      columns = c(Stages)
    )
  )%>%
  # Spanner
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
        #name = "Roboto"
      ), align = "center"
      #size='small'
      )),
    locations = cells_column_spanners()
  )%>%
  # Body
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Muli"),align = 'left'
      )),
    locations = cells_body(
      columns = c(Rider,Titles)
    )
  )%>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Muli")
      ),align='left'),
    locations = cells_body(
      columns = c(Stages)
    )
  )%>%
  # Footnote
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
      ),style = "italic")),
    locations = cells_footnotes()
  )%>%
  # Footnote
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
      ))),
    locations = cells_source_notes()
  )%>%
  # Borders
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden"
  )
tab

gtsave_extra(data=tab,filename='TdF.png')
