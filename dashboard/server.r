library(shiny)
library(shinydashboard)
library(DBI)
library(pool)
library(dplyr)
library(RMySQL)
library(ggplot2)
library(patchwork)
library(pryr)
library(reticulate)

source_python('authorSearch.py')

#get the database connection through the dns
pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "bibliometrics",
  host = "localhost",
  username = "luke",
  password = "K8H,3Cuq]?HzG*W7"
)

cumulativeTransform <- function(x){
  for(i in 2:length(x)){
    x[i] <- x[i] + x[i-1]
  }
  x
}

#a global variable used to get around observer objects constantly changing
names <- NULL

#make sure to close connections when the program exits
onStop(function() {
  poolClose(pool)
})

#This function queries the queue and lists what's left to be queried.

getQueue <- function(searchString){
  query <-
    paste0(searchString)
  dbFrame <- dbGetQuery(pool, query)
  dbFrame
}

updateRanges <- function(session)({
  str <- paste0("'", names, "'")
  for (x in names) {
    if (x != names[1]) {
      str <- paste0(str, "or author =", "'", x, "'")
    }
  }
  query <-
    paste0("Select min(pub_date), max(pub_date) from citations where author=",
           str
           ,
           ";")
  df <- dbGetQuery(pool, query)
  updateSliderInput(
    session,
    "rangeG",
    min = df$"min(pub_date)",
    max = df$"max(pub_date)",
    value = c(min, max)
  )
})

#This function is just a query to get the H-index values from the database and 
#combines them with the citation dataframe.
getHindex <- function(df, max, min, authList) {
  #It takes the dataframe which has the citation metrics, the ranges, and the 
  #list to query.
  ws <- data.frame(hindex = integer())
  #initializes and empty frame and tells it to expect integers.
  for (auths in authList) {
    #going through all the authors it was supplied to query.
    query <-
      paste0("select link from profiles where author = '", auths, "'")
    dbFrame <- dbGetQuery(pool, query)
    link <- (unlist(dbFrame[1,]))
    #grabs the links from the profiles table to search the h-index table.
    query <-
      paste0("select h_index, i_ten, year, g_index from metrics where profile = '",
             link,"' and year < ", min, " and year > ", max)
    dbFrame <- dbGetQuery(pool, query)
    author <- rep(auths, nrow(dbFrame))
    dbFrame <- cbind(dbFrame, author)
    #searches the metrics table with the profile links supplied by the profile table.
    if (nrow(ws) == 0) {
      ws <- dbFrame
      #if this is the first entry, then fill the blank dataframe.
    }
    else{
      ws <- rbind(ws, dbFrame)
      #else, keep adding to the dataframe
    }
  }
  colnames(ws)[1] <- "hindex"
  #format the frame correctly for graphing
  return(ws)
}

#queries the database and searches for strings in the search box
getNames <- function(name, input) {
  argL <- unlist(strsplit(name[1], ","))
  #format the list to be searched correctly
  sql <-
    paste0(
      "Select distinct(author), link from citations where",
      " author like '%",
      (argL)[1],
      "%';"
    )
  query <- sqlInterpolate(pool, sql, id = input$ID)
  dbFrame1 <- dbGetQuery(pool, query)
  #once again, this first search populates the vector, then keeps searching
  #through the following loop.
  for (i in argL) {
    sql <- paste0("Select distinct(author), link from citations where",
                  " author like '%",
                  i,
                  "%';")
    query <- sqlInterpolate(pool, sql, id = input$ID)
    dbFrame <- dbGetQuery(pool, query)
    dbFrame1 <- rbind(dbFrame, dbFrame1)
  }
  #keep adding names to populate list
  v <- as.vector(dbFrame1$author)
  return(v)
}

#initialize some reactive ranges for graphing and queries
ranges <- reactiveValues(x = NULL, y = NULL)

#function used to change the global variable names, this is done because
#r's observer objects really, really don't like to make deep copies
init <- function(var){
  names <<- c(names, var)
  names <- unique(names)
  cat(paste(names, collapse=", "))
}

#empty out the global variable when the clear button is pressed.
clearNames <- function(){
  names <<- NULL
  names
}

getFrame <- function(){
  qString <- ''
  cString <- ''
  for (x in names){
    qString <- paste0(qString, " citations.author='", x,"'", " or")
    cString <- paste0(cString, " author='", x, "'", " or")
  }
  qString <- gsub('.{3}$', '', qString)
  cString <- gsub('.{3}$', '', cString)
  sql <- paste0("select distinct metrics.year, citations.author, metrics.h_index,
                metrics.i_ten, metrics.g_index from metrics inner join citations
                on metrics.profile=citations.link where ", qString, 
                "order by citations.author asc, metrics.year asc;")
  query <- sqlInterpolate(pool, sql)
  dbFrame <- dbGetQuery(pool, query)
  sqlCit <- paste0("select author, sum(count), year from citations where",
                    cString,
                    "group by author, year;")
  citQuery <- sqlInterpolate(pool, sqlCit)
  citFrame <- dbGetQuery(pool, citQuery)
  retFrame <- merge(dbFrame, citFrame, by=c("author", "year"))
  names(retFrame)[names(retFrame) == "sum(count)"] <- "Citation Count"
  retFrame
}


#the actual server function which handles all rendering
server <- function(input, output, session) {
  
  #clear button, dump the names vector
  observeEvent(input$clear,{
    names <- clearNames()
    output$authOut <- renderPrint({
      cat("")
    })
  })
  
  #the add button, adds names to the names vector
  observeEvent(input$submit,{
    newNames <- input$qNames
    output$authOut <- renderPrint({
      init(newNames)
    })
  })
  
  output$downloadFrame <- downloadHandler(
    filename = function() {
      paste("export.csv", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getFrame(), file, row.names = FALSE)
    }
  )
  
  #zoom function, drag a box around the graph, double click to zoom, double 
  #click again to go back
  observeEvent(input$popDubs, {
    brush <- input$popBrush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    }
    else{
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  #the search box, reactively adds names from what is typed
  observe({
    x <- getNames(input$authG, input)
    y <- c()
    updateSelectInput(session, "qNames", c = (x))

  })
  
  #the massive plotinput function.
  #this needs to be condensed, A LOT, but it figures out what to query, then graphs.
  plotInput <- function() {
    #checking for what string to add to the query, either looking for citation
    #information or publication information.
    if (input$radioGT == "pubs") {
      qString <- "count(distinct title)"
    }
    else if (input$radioGT == "cit") {
      qString <- "sum(count)"
    }
    query <-
      paste0(
        "SELECT author,pub_date, ",
        qString,
        " FROM citations",
        " WHERE author='",
        names[1],
        "' and pub_date > ",
        as.integer(input$rangeG)[1],
        " and pub_date < ",
        as.integer(input$rangeG[2]),
        " group by pub_date;"
      )
    df <- dbGetQuery(pool, query)
    cFrame <- df[order(df$pub_date),]
    cumulative <- 0
    #again, first query is the first name, then it iterates through rest of tail
    for (x in 1:nrow(cFrame)) {
      ex <- cFrame[x, qString]
      cumulative <- cumulative + ex
      cFrame[x, qString] <- cumulative
      cFrame[x, "author"] <-
        paste0(cFrame[x, "author"], "(cumulative)")
    }
    for (x in tail(names, -1)) {
      query <-
        paste0(
          "SELECT author,pub_date, ",
          qString,
          " FROM citations",
          " WHERE author='",
          x ,
          "' and pub_date > ",
          as.integer(input$rangeG)[1],
          " and pub_date < ",
          as.integer(input$rangeG[2]),
          " group by pub_date;"
        )
      df2 <- dbGetQuery(pool, query)
      cFrame2 <- df2[order(df2$pub_date),]
      cumulative2 <- 0
      #a frame for cumulative metrics is pre-loaded
      for (x in 1:nrow(cFrame2)) {
        ex2 <- cFrame2[x, qString]
        cumulative2 <- cumulative2 + ex2
        cFrame2[x, qString] <- cumulative2
        cFrame2[x, "author"] <-
          paste0(cFrame2[x, "author"], "(cumulative)")
      }
      cFrame <- rbind(cFrame, cFrame2)
      df <- rbind(df, df2)
    }
    #these logical checks figure out what to add to the data frame.
    #there's a lot of repetitive copy pasting, and could use a lot of work.
    if (input$Cumulative == TRUE && input$Yearly == TRUE) {
      dataset <-
        getHindex(df, input$rangeG[1], input$rangeG[2], names)
      i <- sapply(dataset, is.factor)
      dataset[i] <- lapply(dataset[i], as.character)
      df$pub_date <- as.numeric(as.character(df$pub_date))
      culmH <-
        dplyr::left_join(dataset, df, by = c("year" = "pub_date", "author" = "author"))
      culmH[is.na(culmH)] <- 0
      colnames(culmH)[3] <- "pub_date"
      df <- rbind(df, cFrame)
    }
    else if (input$Cumulative == TRUE && input$Yearly == FALSE) {
      dataset <-
        getHindex(df, input$rangeG[1], input$rangeG[2], names)
      i <- sapply(dataset, is.factor)
      dataset[i] <- lapply(dataset[i], as.character)
      df$pub_date <- as.numeric(as.character(df$pub_date))
      culmH <-
        dplyr::left_join(dataset, df, by = c("year" = "pub_date", "author" = "author"))
      culmH[is.na(culmH)] <- 0
      colnames(culmH)[3] <- "pub_date"
      df <- cFrame
    }
    else if (input$Cumulative == FALSE && input$Yearly == TRUE) {
      dataset <-
        getHindex(df, input$rangeG[1], input$rangeG[2], names)
      i <- sapply(dataset, is.factor)
      dataset[i] <- lapply(dataset[i], as.character)
      df$pub_date <- as.numeric(as.character(df$pub_date))
      culmH <-
        dplyr::left_join(dataset, df, by = c("year" = "pub_date", "author" = "author"))
      culmH[is.na(culmH)] <- 0
      colnames(culmH)[3] <- "pub_date"
    }
    else if (input$Cumulative == FALSE &&
             input$Yearly == FALSE && input$hindex == TRUE) {
      dataset <-
        getHindex(df, input$rangeG[1], input$rangeG[2], names)
      i <- sapply(dataset, is.factor)
      dataset[i] <- lapply(dataset[i], as.character)
      df$pub_date <- as.numeric(as.character(df$pub_date))
      culmH <-
        dplyr::left_join(dataset, df, by = c("year" = "pub_date", "author" = "author"))
      culmH[is.na(culmH)] <- 0
      colnames(culmH)[3] <- "pub_date"
      df <- culmH
    }
    else if (input$Cumulative == FALSE &&
             input$Yearly == FALSE && input$hindex == FALSE) {
      return()
    }
    #all the plotting functions, using logical states to decide which graph
    #to display
    if (input$radioG == "Histogram") {
      if (input$radioGT == "pubs") {
        p1 <-
          ggplot(data = df,
                 aes(
                   x = df$pub_date,
                   y = df$"count(distinct title)",
                   fill = author
                 )) +
          geom_bar(stat = "identity", position = position_dodge()) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
          ggtitle("Publications") +
          labs(x = "Date"+scale_x_continuous(labels = NULL))
        xd <- dplyr::filter(culmH, !grepl("(cumulative)", author))
        if (input$hindex == TRUE) {
          xd <- dplyr::filter(culmH, !grepl("(cumulative)", author))
          p2 <-
            ggplot(data = xd,
                   aes(
                     x = xd$pub_date,
                     y = xd$"hindex",
                     fill = author
                   )) +
            geom_bar(stat = "identity", position = position_dodge()) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
            ggtitle("H-Index") +
            labs(x = "Date",
                 y = "H-Index"+scale_x_continuous(labels = NULL))
          if (input$Yearly == TRUE || input$Cumulative == TRUE) {
            p1 + p2
          }
          else{
            p2 
          }
        }
        else{
          print(cumulativeTransform(df$"count(distinct title)"))
          p1 + scale_y_continuous("Miles/gallon", sec.axis = sec_axis(cumsum(~.), name = "ex"))
        }
      }
      else if (input$radioGT == "cit") {
        p1 <- ggplot(data = df,
                     aes(
                       x = df$pub_date,
                       y = df$"sum(count)",
                       fill = author
                     )) +
          ggtitle("Citations") +
          geom_bar(stat = "identity", position = position_dodge()) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
          labs(x = "Date",
               y = "Citations"+scale_x_continuous(labels = NULL))
        xten <- dplyr::filter(culmH, !grepl("(cumulative)", author))
        p3 <-
          ggplot(data = xten,
                 aes(
                   x = xten$pub_date,
                   y = xten$"i_ten",
                   fill = author
                 )) +
          geom_bar(stat = "identity", position = position_dodge()) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
          ggtitle("I10 Index") +
          labs(x = "Date",
               y = "i10 Index"+scale_x_continuous(labels = NULL))
        gten <- dplyr::filter(culmH, !grepl("(cumulative)", author))
        p4 <-
          ggplot(data = gten,
                 aes(
                   x = gten$pub_date,
                   y = gten$"g_index",
                   fill = author
                 )) +
          geom_bar(stat = "identity", position = position_dodge()) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
          ggtitle("G-Index") +
          labs(x = "Date",
               y = "G-Index"+scale_x_continuous(labels = NULL))
        if (input$hindex == TRUE) {
          xd <- dplyr::filter(culmH, !grepl("(cumulative)", author))
          p2 <-
            ggplot(data = xd,
                   aes(
                     x = xd$pub_date,
                     y = xd$"hindex",
                     fill = author
                   )) +
            geom_bar(stat = "identity", position = position_dodge()) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
            ggtitle("H-Index") +
            labs(x = "Date",
                 y = "H-Index"+scale_x_continuous(labels = NULL))
          if (input$Yearly == TRUE || input$Cumulative == TRUE) {
            p1 + p2
          }
          else if (input$hindex == TRUE && input$i10 == TRUE) {
            p2 + p3
          }
          else if (input$hindex == TRUE && input$g == TRUE) {
            p2 + p4
          }
          else{
            p2
          }
        }
        else{
          p1
        }
      }
    }
    else if (input$radioG == "Line") {
      if (input$radioGT == "pubs") {
        ggplot(data = df,
               aes(
                 x = df$pub_date,
                 y = df$"count(distinct title)",
                 group = author
               )) +
          ggtitle("Publications") +
          geom_line(aes(color = author)) +
          geom_point(aes(color = author)) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
          labs(x = "Date",
               y = "Citations"+scale_x_continuous(labels = NULL))
      }
      else if (input$radioGT == "cit") {
        ggplot(data = df,
               aes(
                 x = df$pub_date,
                 y = df$"sum(count)",
                 group = author
               )) +
          ggtitle("Citations") +
          geom_line(aes(color = author)) +
          geom_point(aes(color = author)) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
          labs(x = "Date",
               y = "Citations"+scale_x_continuous(labels = NULL))
      }
    }
  }
  
  observe({
    #Graph button.  If anything is in the authout box it plots input given the
    #graphing selections.
    observeEvent(input$graph,{
      updateRanges(session)
      output$popPlot <- renderPlot({
        if(length(names > 0)){
          plotInput()
        }
      })
    })
    
    #export button, saves a png of the graph
    output$downloadD <- downloadHandler(
      filename = 'export.png',
      content = function(file) {
        ggsave(file, plot = plotInput() + theme_bw(base_size = 10),
        width = 20, height = 4, dpi = 300, units = "in", device='png')
      }
    )
  })
  
  #the left hand menu
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem(
        "Graphing",
        tabName = "m1",
        icon = icon("database")
      ),
      menuItem(
        "Queue Status",
        tabName = "m2",
        icon = icon("database")
      ),
      menuItem(
        "Records Search",
        tabName = "m3",
        icon = icon("database")
      )
    )
  })
  
  isolate({
    updateTabItems(session, "tabs", "m1")
  })
  
  #Queue Status Display
  #Four possibilities:
  #Completed, Not completed, Errors, In Progress
  output$queueTable <- renderTable({
    searchString <- "select author, link from profiles where"
    if(input$queueFilter == "Completed"){
      searchString <- paste0(searchString, " search_date IS NOT NULL")
    }
    else if(input$queueFilter == "Queued"){
      searchString <- paste0(searchString, " search_date IS NULL and queue_status = 0")
    }
    else if(input$queueFilter == "Errors"){
      searchString <- paste0(searchString, " err_flag=1")
    }
    else if(input$queueFilter == "In Progress"){
      searchString <- paste0(searchString, " search_date IS NULL and queue_status = 1")
    }
    getQueue(searchString)
    
  })
  
  #basically shows what the database has in a condensed manner
  getRecords <- function() {
    if (input$qType == "All Entries") {
      sql <- paste0(
        "Select title, author, description, pub_date, year ",
        "from citations where author = ?id",
        " and pub_date > ",
        as.integer(input$rangeR)[1],
        " and pub_date < ",
        as.integer(input$rangeR)[2],
        " limit ",
        as.integer(input$maxR),
        ";"
      )
      query <- sqlInterpolate(pool, sql, id = input$m3Author)
      df <- dbGetQuery(pool, query)
      return(df)
    }
    else{
      sql <-
        paste0(
          "Select distinct(title), author, description, pub_date ",
          "from citations where author = ?id",
          " and pub_date > ",
          as.integer(input$rangeR)[1],
          " and pub_date < ",
          as.integer(input$rangeR)[2],
          " group by pub_date",
          " limit ",
          as.integer(input$maxR),
          ";"
        )
      query <- sqlInterpolate(pool, sql, id = input$m3Author)
      df <- dbGetQuery(pool, query)
      return(df)
    }
  }
  observe({
    x <- getNames(input$link, input)
    y <- c()
    updateSelectInput(session, "m3Author", c = (x))
    
  })
  
  observeEvent(input$gSearchButton, {
    strex <- search(input$gSearch)
    output$records <- renderTable({
      strex
    })
  })
  
  #updating records table
  observe({
    ex <- data.frame()
    query <-
      paste0(
        "select min(pub_date), max(pub_date) from citations where author='",
        input$m3Author,
        "';"
      )
    df <- dbGetQuery(pool, query)
    updateSliderInput(
      session,
      "rangeR",
      min = df$"min(pub_date)",
      max = df$"max(pub_date)",
      value = c(min, max)
    )
    
    #actual rendering of the table
    output$records <- renderTable({
      getRecords()
    })
    
    
    #export button for the table data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$link, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(getRecords(), file, row.names = FALSE)
      }
    )
  })
}