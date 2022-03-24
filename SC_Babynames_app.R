library(shiny)
library(dplyr)
library(ggplot2)
library(babynames)

# Database for gal
gal<-babynames%>%
  filter(sex==as.character("F"))
# Database for boys
babynames<-babynames%>%
  filter(sex=="M")

# Extract girls and boys names for selection
nms_gal  <- gal%>%
  group_by(name)%>%
  summarize(ct=sum(n))%>%
  pull(name)
nms  <- babynames%>%
  group_by(name)%>%
  summarize(ct=sum(n))%>%
  pull(name)

# Functions
###########
# Plot for girls
plotNameGal <- function(data,nm){
  gg<-ggplot(data%>%dplyr::filter(name==nm),aes(x=year,y=n))+
    geom_bar(stat="identity",fill='#FF5722')+
    scale_x_continuous(breaks = integer_breaks())+
    scale_y_continuous(breaks = integer_breaks())+
    labs(
      x='Year',
      y='Number of babies'
    )+
    theme_minimal()+
    theme(
      axis.text=element_text(size=12,color='dimgrey'),
      axis.title=element_text(size=14,color='dimgrey')
    )
  return(gg)
}

# Plot for boys
plotName <- function(data,nm){
  gg<-ggplot(data%>%dplyr::filter(name==nm),aes(x=year,y=n))+
    geom_bar(stat="identity",fill='#00ADB5')+
    scale_x_continuous(breaks = integer_breaks())+
    scale_y_continuous(breaks = integer_breaks())+
    labs(
      x='Year',
      y='Number of babies'
    )+
    theme_minimal()+
    theme(
      axis.text=element_text(size=12,color='dimgrey'),
      axis.title=element_text(size=14,color='dimgrey')
    )
  return(gg)
}

# To extract data for most popular year
yearPop <- function(data,nm){
  yr<-data%>%
    filter(name==nm)%>%
    mutate(max=max(n))%>%
    filter(n==max)%>%
    head(1)%>%
    pull(year)
  return(yr)
}

nbPop <- function(data,nm){
  nb<-data%>%
    filter(name==nm)%>%
    mutate(max=max(n))%>%
    filter(n==max)%>%
    head(1)%>%
    pull(n)
  return(nb)
}

# Function to get only integer on axis values 
# (Found on Stack but can't find link again...)
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# Make app
##########

# Define UI 
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Francois+One&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Paytone+One&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Open+Sans&display=swap%27');
      h2 {
        font-family: 'Francois One', sans-serif;
        text-align:center; 
        font-size:35px; 
        background-color:#303841;
        color:white;
        padding: 10px;
      }
      body {
        font-family: 'Open Sans', sans-serif;
      }
      #sidebar {
            background-color: #F7F4F3;
        }
  "))),
  
  title = "babynames",
  #theme = theme_ex,
  titlePanel("Baby names popularity in the US"),
  br(),
  fluidRow(
      column(width=6,
             div(
               style = 
                 "text-align:center;
             font-family: 'Paytone One', sans-serif;
             font-size:30px; 
             color: #FF5722;
              background-color:white;
          padding: 0 0 10px 0;",HTML("Girls")),
             selectizeInput('fooGal', "Type name below:",choices = NULL,multiple=FALSE,width='100%'),
             span(textOutput("text"),style="color:#303841;text-align:center; font-size:15px; font-style: italic;"),
             plotOutput("plotGal")
    ),
    
      column(width=6,
             div(
               style = 
                 "text-align:center;
                font-family: 'Paytone One', sans-serif;
                font-size:30px; 
                color: #00ADB5;
                background-color:white;
                padding: 0 0 10px 0;",HTML("Boys")),
             selectizeInput('foo', "Type name below:",choices = NULL,multiple=FALSE,width='100%'),
             span(textOutput("textBoy"),style="color:#303841;text-align:center; font-size:15px; font-style: italic;"),
             plotOutput("plot")
    )),
  br(),
  div(style = "
                text-align:center;
                bottom:0;
                width:100%;
                height:50px; /* Height of the footer */
                color: grey;
                padding: 0 0 20px 0;
                font-size: 12px;
                ",
      HTML('Website created by <a href="https://bjnnowak.netlify.app/about/" >Benjamin Nowak</a><br>using data from Hadley Wickham <a href="https://cran.r-project.org/web/packages/babynames/index.html" >{babynames}</a> package</a>')
  )
)

# Server side
server <- function(input, output, session) {
  # Girl panel
  updateSelectizeInput(session, 'fooGal', choices = nms_gal,  server = TRUE)
  
  output$plotGal <- renderPlot({
    plotNameGal(data=gal,nm=input$fooGal)
  })
  
  output$text <- renderText({paste(input$fooGal,'was most popular in', yearPop(data=gal,nm=input$fooGal),',with',nbPop(data=gal,nm=input$fooGal),'babies with this name.' )})
  
  # Boy panel
  updateSelectizeInput(session, 'foo', choices = nms, server = TRUE)
  
  output$plot <- renderPlot({
    plotName(data=babynames,nm=input$foo)
  })
  
  output$textBoy <- renderText({paste(input$foo,'was most popular in', yearPop(data=babynames,nm=input$foo),',with',nbPop(data=babynames,nm=input$foo),'babies with this name.' )})

}

# Run the application 
shinyApp(ui = ui, server = server)
