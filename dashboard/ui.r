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
      tabItem(tabName = "m1",
              div(style="display: inline-block;vertical-align:top; width: 250px;",
                textInput("authG", "Enter author names seperated by commas:", "Zhang")),
              div(style="display: inline-block;vertical-align:top;padding:20px;",
                  img(src='logo.png', height = '71px', width = '170px')
                  ),
              selectInput("qNames", "Select Author", c=(""), multiple=TRUE),
              div(style="display: inline-block;vertical-align:top; width: 200px;",
                  radioButtons("radioG", "Select Type of Graphing",
                           c("Histogram" = "Histogram",
                             "Line" = "Line"))),
              div(style="display: inline-block;vertical-align:top; width: 150px;",
                  radioButtons("radioGT", "Select Query",
                               c("Citations" = "cit",
                                 "Publications" = "pubs"))),
              div(style="display: inline-block;vertical-align:top; width: 150px;",
                  checkboxInput("Yearly","Yearly", TRUE),
                  checkboxInput("Cumulative","Cumulative", TRUE),
                  checkboxInput("hindex", "H-Index", FALSE),
                  checkboxInput("g", "G-Index", FALSE),
                  checkboxInput("i10", "I10 Index", FALSE)),
              sliderInput("rangeG",label="Date Range" ,min=1950, max=2021, 
                          value=c(2014,2021),sep=""),
              downloadButton("downloadD", "Export"),
              fluidRow(
              div(style = "position:relative",
                  plotOutput("popPlot", dblclick = "popDubs",
                             hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                             brush = brushOpts(
                             id = "popBrush", clip = TRUE, resetOnNew = TRUE)),
                  uiOutput("hover_info")
              ))),
      tabItem(tabName = "m2",
              textInput("ID", "Enter author link:", "Zhang"),
              tableOutput("auth")),
      tabItem(tabName = "m3",
              textInput("link", "Enter author link", "01GFu9cAAAAJ"),
              radioButtons("qType", "Select the Query Type", c("All Entries", "Individual Papers")),
              sliderInput("rangeR",label="Date Range" ,min=1970, max=2021, 
                          value=c(2014,2021),sep=""),
              sliderInput("maxR",label="Maximum Entries", min=5, max=50,
                          value=5, sep=""),
              downloadButton("downloadData", "Export"),
              tableOutput("records"))
              
    )
  )
)