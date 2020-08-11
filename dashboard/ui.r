library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)

ui <- dashboardPage(

  dashboardHeader(title = "Display"),
  dashboardSidebar(
    sidebarMenu(id="tabs",
                sidebarMenuOutput("menu")
    )
  ),
  dashboardBody(
    includeCSS("www/styles.css"),
    tabItems(
      #graphing tab
      tabItem(tabName = "m1",
              div(style="display: inline-block;vertical-align:top; width: 250px;",
                  #name search
                textInput("authG", "Enter author names seperated by commas:", "Zhang")),
              div(style="display: inline-block;vertical-align:top;padding:20px;",
                  #mallet logo
                  img(src='logo.png', height = '71px', width = '170px')
                  ),
              #results from name search
              selectInput("qNames", "Select Author", c=(""), multiple=TRUE),
              div(style="display: inline-block;vertical-align:top; width: 400px;",
                  tags$style(HTML(".shiny-text-output {background: white}")),
                  tags$style(HTML(".shiny-text-output {font-size: 16px}")),
                  #added names from results
                  textOutput("authOut"),
                  #add button which adds names to authout
                  actionButton("submit", "Add"),
                  #clears all names from authout
                  actionButton("clear", "Clear"),
                  #graphs!
                  actionButton("graph", "Graph")),
              div(style="display: inline-block;vertical-align:top; width: 200px;",
                  radioButtons("radioG", "Select Type of Graphing",
                               #histogram or line graph
                           c("Histogram" = "Histogram",
                             "Line" = "Line"))),
              div(style="display: inline-block;vertical-align:top; width: 150px;",
                  radioButtons("radioGT", "Select Query",
                               c("Citations" = "cit",
                                 "Publications" = "pubs"))),
              #citations or publications
              div(style="display: inline-block;vertical-align:top; width: 100px;",
                  #check boxes to determine what to graph
                  checkboxInput("Yearly","Yearly", TRUE),
                  checkboxInput("Cumulative","Cumulative", TRUE),
                  checkboxInput("hindex", "H-Index", FALSE)),
              div(style="display: inline-block;vertical-align:top; width: 100px;",
                  checkboxInput("g", "G-Index", FALSE),
                  checkboxInput("i10", "I10 Index", FALSE)),
              #reactive ranges for graphs
              sliderInput("rangeG",label="Date Range" ,min=1950, max=2021, 
                          value=c(2014,2021),sep=""),
              #export buttons
              downloadButton("downloadD", "Export PNG"),
              downloadButton("downloadFrame", "Export CSV"),
              fluidRow(
              div(style = "position:relative",
                  #the actual graph itself
                  plotOutput("popPlot", dblclick = "popDubs",
                             hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                             brush = brushOpts(
                             id = "popBrush", clip = TRUE, resetOnNew = TRUE)),
                  #part of the zoom function ^
                  uiOutput("hover_info")
              ))),
      #second tab to search for google scholar links
      tabItem(tabName = "m2",
              selectInput("queueFilter", "Filter", c("Completed", "In Progress", "Queued", "Errors")),
              dataTableOutput("queueTable")),
      tabItem(tabName = "m3",
              tags$style('#errMsg {background-color:#ECF0F5}'),
              useShinyjs(),
              #third tab which shows basically what's in the database
              textInput("link", "Search Author Name", "Bengio"),
              fluidRow(
              div(style = "display: inline-block;vertical-align:top;padding-left:15px;padding-bottom:10px",
                selectInput("m3Author", "Select Author", c(NULL)),
                div(class = "errMsg",
                  textOutput("errMsg"))
              )),
              fluidRow(
              div(style = "display: inline-block;vertical-align:top;padding-left:15px;padding-bottom:10px",
                actionButton("gSearchButton", "Search Google"),
                actionButton("clearSearch", "Clear Search"),
                actionButton("addToQueueButton", "Add to Queue")
              )),
              radioButtons("qType", "Select the Query Type", c("Individual Papers", "All Entries")),
              #another reactive slider range
              sliderInput("rangeR",label="Date Range" ,min=1970, max=2021, 
                          value=c(2014,2021),sep=""),
              downloadButton("downloadData", "Export"),
              #export in csv format
              dataTableOutput("scrape"),
              dataTableOutput("records"))
      #the actual table output
              
    )
  )
)