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

onStop(function(){
  poolClose(pool)
})

getHindex <- function(df, max, min, authList){
  ds <- data.frame(hindex = integer())
  xs <- c(1:nrow(df))
  for(x in xs){
    query <- paste0("select distinct(title), sum(count) from citations where", 
                    " author = '", df[x,1],"' and year <=", df[x,2]," group by title;")
    dbFrame <- dbGetQuery(pool, query)
    hindex <- 1
    while(TRUE){
      vals <- sum(dbFrame$"sum(count)" > hindex)
      if(vals < hindex) break
      hindex = 1 + hindex
    }
    ds <- rbind(ds,hindex=(hindex))
  }
  names(ds)[1]<- "hindex"
  return(ds)
}

ranges <- reactiveValues(x = NULL, y = NULL)


server <- function(input, output, session) {
  observeEvent(input$popDubs, {
    brush <- input$popBrush
    if(!is.null(brush)){
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    }
    else{
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
  #gets names for graphing function
  getNames <- function(name, input){
    argL <- unlist(strsplit(input$authG[1], ","))
    sql <- paste0("Select distinct(author), link from citations where",
                  " author like '%", (argL)[1],
                  "%';")
    query <- sqlInterpolate(pool, sql, id = input$ID)
    dbFrame1 <- dbGetQuery(pool, query)
    for(i in argL){
      sql <- paste0("Select distinct(author), link from citations where",
                    " author like '%", i,
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
    updateSelectInput(session, "qNames", c=(x))
    
    
  })
  
  observe({
    str <- paste0("'",input$qNames[1],"'")
    for(x in input$qNames){
      if(x != input$qNames[1]){
        str <- paste0(str, "or author =", "'",x,"'")
      }
    }
    query <- paste0("Select min(pub_date), max(pub_date) from citations where author=", str
                    ,";")
    df <- dbGetQuery(pool, query)
    updateSliderInput(session, "rangeG",min=df$"min(pub_date)", max=df$"max(pub_date)", 
                      value=c(min,max))
    if(length(input$qNames) > 0){
      output$popPlot <- renderPlot({
        if(input$radioGT == "pubs"){
          qString <- "count(distinct title)"
        }
        else if(input$radioGT == "cit"){
          qString <- "sum(count)"
        }
        query <- paste0("SELECT author,pub_date, ", qString, " FROM citations",
                        " WHERE author='",(input$qNames)[1],
                        "' and pub_date > ", 
                        as.integer(input$rangeG)[1],
                        " and pub_date < ",as.integer(input$rangeG[2]),
                        " group by pub_date;")
        df <- dbGetQuery(pool, query)
        cFrame <- df[order(df$pub_date),]
        cumulative <- 0
        for(x in 1:nrow(cFrame)){
          ex <- cFrame[x, qString]
          cumulative <- cumulative + ex
          cFrame[x, qString] <- cumulative
          cFrame[x, "author"] <- paste0(cFrame[x, "author"],"(cumulative)")
        }
        for (x in tail(input$qNames, -1)){
          query <- paste0("SELECT author,pub_date, ", qString, " FROM citations",
                          " WHERE author='", x , "' and pub_date > ", 
                          as.integer(input$rangeG)[1],
                          " and pub_date < ",as.integer(input$rangeG[2]),
                          " group by pub_date;")
          df2 <- dbGetQuery(pool, query)
          cFrame2 <- df2[order(df2$pub_date),]
          cumulative2 <- 0
          for(x in 1:nrow(cFrame2)){
            ex2 <- cFrame2[x, qString]
            cumulative2 <- cumulative2 + ex2
            cFrame2[x, qString] <- cumulative2
            cFrame2[x, "author"] <- paste0(cFrame2[x, "author"],"(cumulative)")
          }
          cFrame <- rbind(cFrame, cFrame2)
          df <- rbind(df, df2)
          write.csv(df,file = "C:/Users/Luke/Desktop/ex.csv")
        }
        if(input$Cumulative == TRUE && input$Yearly == TRUE){
          dataset <- getHindex(df, input$rangeG[1], input$rangeG[2], input$qNames)
          culmH <- cbind(df, dataset)
          df <- rbind(df, cFrame)
        }
        else if(input$Cumulative == TRUE && input$Yearly == FALSE){
          dataset <- getHindex(df, input$rangeG[1], input$rangeG[2], input$qNames)
          culmH <- cbind(df, dataset)
          df <- cFrame
        }
        else if(input$Cumulative == FALSE && input$Yearly == TRUE){
          dataset <- getHindex(df, input$rangeG[1], input$rangeG[2], input$qNames)
          culmH <- cbind(df, dataset)
        }
        else if(input$Cumulative == FALSE && input$Yearly == FALSE && input$hindex == TRUE){
          dataset <- getHindex(df, input$rangeG[1], input$rangeG[2], input$qNames)
          culmH <- cbind(df, dataset)
          df <- culmH
        }
        else if(input$Cumulative == FALSE && input$Yearly == FALSE && input$hindex == FALSE){
          return()
        }
        if(input$radioG == "Histogram"){
          if(input$radioGT == "pubs"){
            p1 <- ggplot(data = df, aes(x = df$pub_date, y = df$"count(distinct title)", 
                                fill = author)) + 
              geom_bar(stat="identity", position = position_dodge()) +
              coord_cartesian(xlim = ranges$x, ylim = ranges$y) + 
                ggtitle("Publications") +
                scale_y_continuous(
                  name = "Publications"
                )+
                labs(x = "Date"+
                scale_x_continuous(labels=NULL))
            xd <- dplyr::filter(culmH, !grepl("(cumulative)",author))
            if(input$hindex == TRUE){
              xd <- dplyr::filter(culmH, !grepl("(cumulative)",author))
              p2 <- ggplot(data = xd, aes(x = xd$pub_date, y = xd$"hindex", 
                                          fill = author)) + 
                geom_bar(stat="identity", position = position_dodge()) +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y) + 
                ggtitle("H-Index") +
                labs(x = "Date", y = "H-Index" +
                      scale_x_continuous(labels=NULL))
              if(input$Yearly == TRUE || input$Cumulative == TRUE){
                p1+p2
              }
              else{
                p2
              }
            }
            else{
              p1
            }
          }
          else if(input$radioGT == "cit"){
            p1 <- ggplot(data = df, aes(x = df$pub_date, y = df$"sum(count)", 
                                  fill = author)) +
              ggtitle("Citations") +
              geom_bar(stat="identity", position = position_dodge()) +
              coord_cartesian(xlim = ranges$x, ylim = ranges$y) + 
              labs(x = "Date", y = "Citations" +
                     scale_x_continuous(labels=NULL))
            if(input$hindex == TRUE){
              xd <- dplyr::filter(culmH, !grepl("(cumulative)",author))
              p2 <- ggplot(data = xd, aes(x = xd$pub_date, y = xd$"hindex", 
                                          fill = author)) + 
                geom_bar(stat="identity", position = position_dodge()) +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y) + 
                ggtitle("H-Index") +
                labs(x = "Date", y = "H-Index" +
                       scale_x_continuous(labels=NULL))
              if(input$Yearly == TRUE || input$Cumulative == TRUE){
                p1+p2
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
        else if(input$radioG == "Line"){
          if(input$radioGT == "pubs"){
            ggplot(data = df, aes(x = df$pub_date, y = df$"count(distinct title)", 
                                  group = author)) +
              ggtitle("Publications") +
              geom_line(aes(color = author)) +
              geom_point(aes(color = author)) +
              coord_cartesian(xlim = ranges$x, ylim = ranges$y) + 
              labs(x = "Date", y = "Citations" +
                    scale_x_continuous(labels=NULL))
          }
          else if(input$radioGT == "cit"){
            ggplot(data = df, aes(x = df$pub_date, y = df$"sum(count)", 
                                  group = author)) +
              ggtitle("Citations") +
              geom_line(aes(color = author)) +
              geom_point(aes(color = author)) +
              coord_cartesian(xlim = ranges$x, ylim = ranges$y) + 
              labs(x = "Date", y = "Citations" +
                     scale_x_continuous(labels=NULL))
          }
        }
      })
    }
  })
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Graphing", tabName="m1", icon = icon("database")),
      menuItem("Author Search", tabName="m2", icon = icon("database")),
      menuItem("Records Search", tabName="m3", icon = icon("database"))
    )
  })
  
  isolate({updateTabItems(session, "tabs", "m1")})
  
  output$auth <- renderTable({
    #author list for search
      sql <- paste0("Select distinct(author), link from citations where author like '%",input$ID,"%';")
      query <- sqlInterpolate(pool, sql, id = input$ID)
      dbGetQuery(pool, query)
      
  })
  #updating records table
  observe({
    query <- paste0("select min(pub_date), max(pub_date) from citations where link='",
                    input$link,"';")
    df <- dbGetQuery(pool, query)
    updateSliderInput(session, "rangeR", min=df$"min(pub_date)", max=df$"max(pub_date)",
                      value=c(min,max))
    output$records <- renderTable({
        sql <- paste0("Select title, author, description, pub_date, year ",
                      "from citations where link = ?id",
                      " and pub_date > ",
                      as.integer(input$rangeR)[1],
                      " and pub_date < ",
                      as.integer(input$rangeR)[2],
                      " limit ", as.integer(input$maxR),
                      ";")
        query <- sqlInterpolate(pool, sql, id = input$link)
        dbGetQuery(pool, query)
      
    })
  })
}