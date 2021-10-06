
# Clear space 
rm(list=ls())
gc()


tuesdata <- tidytuesdayR::tt_load(2021, week = 41)
nurses<-tuesdata$nurses

library(tidyverse)
library(gt)
library(gtExtras)


test<-nurses%>%
  filter(Year==2020)

dat<-nurses%>%
  filter(Year==2020)%>%
  mutate(max_annual=max(get('Annual Salary Median')))%>%
  mutate(ratio=max_annual/get('Annual Salary Median'))%>%
  mutate(ratio_integer=round(ratio))%>%
  arrange(get('Annual Salary Median'))%>%
  select(State,'Annual Salary Median',ratio_integer)


bar<-nurses%>%
  filter(Year==2020)%>%
  group_by(State)%>%
  mutate(
    X25=round(get('Hourly 25th Percentile')),
    X50=round(get('Hourly Wage Median')),
    X75=round(get('Hourly 75th Percentile'))
  )%>%
  arrange(-X75)%>%
  summarise(Salary = list(c(X25,X50,X75)))


ord<-nurses%>%
  filter(Year==2020)%>%
  mutate(
    X25=round(get('Hourly 25th Percentile')),
    X50=round(get('Hourly Wage Median')),
    X75=round(get('Hourly 75th Percentile')),
    total=get('Total Employed RN'),
    disp=get('Location Quotient')
  )%>%
  mutate(disp=case_when(
    disp<=quantile(disp)[2]~1,
    disp<=quantile(disp)[3]~2,
    disp<=quantile(disp)[4]~3,
    TRUE~4
  ))%>%
  arrange(-X75)%>%
  select(State,total,disp)


# Set color palette
pal_salary <- c('#E7B1B1','#CD5C5C','#9C3030')

tab<-ord%>%
  left_join(bar)%>%
  gt()%>%
  fmt_number(
    columns = total,
    sep_mark = ',',drop_trailing_zeros=TRUE
  )%>%
  gt_highlight_rows(
    rows = 1, 
    fill = "lightgrey",
    bold_target_only = TRUE,
    target_col = State
  )%>%
  gt_color_rows(total, palette = "ggsci::brown_material")%>%
  gt_fa_repeats(
    column=disp,palette = "indianred",
    name = "user-nurse",align='center',
  )%>%
  gt_plt_bar_stack(
    column=Salary, palette = pal_salary,
    position = 'stack', labels = c("1st quartile", "Median", "3rd quartile"),
    width = 60,trim=TRUE
  )%>%
  tab_header(
    title = "U.S. nurses in 2020",
    subtitle = md("If you are looking for a new job, being a nurse in **California** is a job to consider: this is the state with the highest hourly wage but also one of the lowest nurses' availability, despite the highest number of nurses of the country.<br>")
  )%>%
  tab_spanner(
    label = "State",columns = c(State)
  )%>%
  tab_spanner(
    label = "Employment",columns = c(total)
  )%>%
  tab_spanner(
    label = "Hourly wage ($)",columns = c(Salary)
  )%>%
  tab_spanner(
    label = "Availability",columns = c(disp),
    id='av'
  )%>%
  cols_label(
    State = md("Ordered by decreasing wages"),
    
  )%>%
  cols_label(
    disp = md("4 for highest availability")
  )%>%
  cols_label(
    total = "# of registred nurses"
  )%>%
  tab_footnote(
    footnote = 'Estimated from Location Quotient',
    locations = cells_column_spanners(spanners = "av")
  )%>%
  tab_source_note(
    source_note = md("**Data:** Data.world & TidyTuesday | **Table:** @BjnNowak")
  )%>%
  # Style
  gt_theme_nytimes()%>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Bebas Neue"), 
        size='xx-large',
        color='indianred'
        )),
    locations = cells_title(groups = "title")
  )%>%
  # Subtitle
  tab_style(
    style = list(
      cell_text(font=google_font(
        #name = "Roboto Condensed"
        name = "Roboto"), align = "left",size='small')),
    locations = cells_title(groups = "subtitle")
  )%>%
  # Headers
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Noto Sans Display"), 
        align = "center",v_align='middle',
        transform = 'capitalize',weight='bold'),
      cell_borders(color='dimgrey',style='solid',sides=c('top'))
  ),
    locations = cells_column_spanners()
  ) %>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
      ), align = "center",size='small',
      transform = 'lowercase',v_align='middle'),
      cell_borders(color='dimgrey',style='solid',sides=c('bottom'))),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Fira Sans"),align = 'left'
      )),
    locations = cells_body(columns = c(State))
  )%>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Fira Sans"),align = 'center'
      )),
    locations = cells_body(columns = c(total))
  )%>%
  # Footnote
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
      ),style = "italic"),
      cell_borders(color='dimgrey',style='solid',sides=c('top'))),
    locations = cells_footnotes()
  )%>%
  # Source note
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
      ))),
    locations = cells_source_notes()
  )%>%
  # Borders
  tab_options(
    table.border.top.style = "solid",
    table.border.top.color = "dimgrey",
    table.border.bottom.style = "hidden"
  )
tab
