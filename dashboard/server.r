library(shiny)
library(shinydashboard)
library(DBI)
library(pool)
library(dplyr)
library(RMySQL)
library(ggplot2)
library(patchwork)


pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "bibliometrics",
  host = "localhost",
  username = "luke",
  password = "K8H,3Cuq]?HzG*W7"
)

onStop(function() {
  poolClose(pool)
})

getHindex <- function(df, max, min, authList) {
  ws <- data.frame(hindex = integer())
  for (auths in authList) {
    query <-
      paste0("select link from profiles where author = '", auths, "'")
    dbFrame <- dbGetQuery(pool, query)
    link <- (unlist(dbFrame[1,]))
    query <-
      paste0("select h_index, i_ten, year, g_index from metrics where profile = '",
             link,
             "'")
    dbFrame <- dbGetQuery(pool, query)
    author <- rep(auths, nrow(dbFrame))
    dbFrame <- cbind(dbFrame, author)
    if (nrow(ws) == 0) {
      ws <- dbFrame
    }
    else{
      ws <- rbind(ws, dbFrame)
    }
  }
  colnames(ws)[1] <- "hindex"
  return(ws)
}

ranges <- reactiveValues(x = NULL, y = NULL)


server <- function(input, output, session) {
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
  
  
  #gets names for graphing function
  getNames <- function(name, input) {
    argL <- unlist(strsplit(input$authG[1], ","))
    sql <-
      paste0(
        "Select distinct(author), link from citations where",
        " author like '%",
        (argL)[1],
        "%';"
      )
    query <- sqlInterpolate(pool, sql, id = input$ID)
    dbFrame1 <- dbGetQuery(pool, query)
    for (i in argL) {
      sql <- paste0("Select distinct(author), link from citations where",
                    " author like '%",
                    i,
                    "%';")
      query <- sqlInterpolate(pool, sql, id = input$ID)
      dbFrame <- dbGetQuery(pool, query)
      dbFrame1 <- rbind(dbFrame, dbFrame1)
    }
    v <- as.vector(dbFrame1$author)
    return(v)
  }
  
  observe({
    x <- getNames(input$authG, input)
    y <- c()
    updateSelectInput(session, "qNames", c = (x))
    
    
  })
  
  plotInput <- function() {
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
        (input$qNames)[1],
        "' and pub_date > ",
        as.integer(input$rangeG)[1],
        " and pub_date < ",
        as.integer(input$rangeG[2]),
        " group by pub_date;"
      )
    df <- dbGetQuery(pool, query)
    cFrame <- df[order(df$pub_date),]
    cumulative <- 0
    for (x in 1:nrow(cFrame)) {
      ex <- cFrame[x, qString]
      cumulative <- cumulative + ex
      cFrame[x, qString] <- cumulative
      cFrame[x, "author"] <-
        paste0(cFrame[x, "author"], "(cumulative)")
    }
    for (x in tail(input$qNames, -1)) {
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
    if (input$Cumulative == TRUE && input$Yearly == TRUE) {
      dataset <-
        getHindex(df, input$rangeG[1], input$rangeG[2], input$qNames)
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
        getHindex(df, input$rangeG[1], input$rangeG[2], input$qNames)
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
        getHindex(df, input$rangeG[1], input$rangeG[2], input$qNames)
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
        getHindex(df, input$rangeG[1], input$rangeG[2], input$qNames)
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
          scale_y_continuous(name = "Publications") +
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
          p1
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
  
  plotLine <- function(df, culM) {
    
  }
  
  observe({
    str <- paste0("'", input$qNames[1], "'")
    for (x in input$qNames) {
      if (x != input$qNames[1]) {
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
    if (length(input$qNames) > 0) {
      output$popPlot <- renderPlot({
        plotInput()
      })
    }
    output$downloadD <- downloadHandler(
      filename = 'export.png',
      content = function(file) {
        ggsave(file, plot = plotInput())
      }
    )
  })
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem(
        "Graphing",
        tabName = "m1",
        icon = icon("database")
      ),
      menuItem(
        "Author Search",
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
  
  output$auth <- renderTable({
    #author list for search
    sql <-
      paste0("Select distinct(author), link from citations where author like '%",
             input$ID,
             "%';")
    query <- sqlInterpolate(pool, sql, id = input$ID)
    dbGetQuery(pool, query)
    
  })
  getRecords <- function() {
    if (input$qType == "All Entries") {
      sql <- paste0(
        "Select title, author, description, pub_date, year ",
        "from citations where link = ?id",
        " and pub_date > ",
        as.integer(input$rangeR)[1],
        " and pub_date < ",
        as.integer(input$rangeR)[2],
        " limit ",
        as.integer(input$maxR),
        ";"
      )
      query <- sqlInterpolate(pool, sql, id = input$link)
      df <- dbGetQuery(pool, query)
      return(df)
    }
    else{
      sql <-
        paste0(
          "Select distinct(title), author, description, pub_date ",
          "from citations where link = ?id",
          " and pub_date > ",
          as.integer(input$rangeR)[1],
          " and pub_date < ",
          as.integer(input$rangeR)[2],
          " group by pub_date",
          " limit ",
          as.integer(input$maxR),
          ";"
        )
      query <- sqlInterpolate(pool, sql, id = input$link)
      df <- dbGetQuery(pool, query)
      return(df)
    }
  }
  #updating records table
  observe({
    ex <- data.frame()
    query <-
      paste0(
        "select min(pub_date), max(pub_date) from citations where link='",
        input$link,
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
    output$records <- renderTable({
      getRecords()
    })
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