library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Display"),
  dashboardSidebar(
    sidebarMenu(id="tabs",
                sidebarMenuOutput("menu")
    )
  ),
  dashboardBody(
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
              textInput("ID", "Enter author link:", "Zhang"),
              tableOutput("auth")),
      tabItem(tabName = "m3",
              #third tab which shows basically what's in the database
              textInput("link", "Enter author link", "01GFu9cAAAAJ"),
              radioButtons("qType", "Select the Query Type", c("All Entries", "Individual Papers")),
              #another reactive slider range
              sliderInput("rangeR",label="Date Range" ,min=1970, max=2021, 
                          value=c(2014,2021),sep=""),
              sliderInput("maxR",label="Maximum Entries", min=5, max=50,
                          value=5, sep=""),
              downloadButton("downloadData", "Export"),
              #export in csv format
              tableOutput("records"))
      #the actual table output
              
    )
  )
)