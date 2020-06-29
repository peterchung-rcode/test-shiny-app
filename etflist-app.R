## app.R ##
# https://github.com/sol-eng/db-dashboard/blob/master/database/db_app.R

library(shiny)
library(shinydashboard)
library(data.table)
library(purrr)
library(gdata)
library(readxl)
library(plotly)
library(quantmod)
library(rvest)

options("getSymbol.warning4.0"=FALSE)
options("getSymbol.auto.assign"=FALSE)

files <- list.files(path = "data",pattern = "etflist-*",recursive = T,full.names = T)
temp <- lapply(files, fread, sep=",")

header <- dashboardHeader(title = "ETF List")

sidebar <- dashboardSidebar(
  selectInput(
    inputId = "airline",
    label = "Dataset:",
    choices = files,
    selected = NULL,
    selectize = FALSE
  ),
  sidebarMenu(
    selectInput(
      inputId = "month",
      label = "Ticker:",
      choices = NULL,
      selected = NULL,
      size = 13,
      selectize = FALSE
    )
  )
)

body <- dashboardBody(
  box(height = 0,width = 2, 
      sliderInput("slider1", h3("Sliders"),
                  min = 0, max = 10, value = 3.5,step = 0.5),
      radioButtons("radio", h3("Periodicity"),
                   choices = list("daily" = "daily", "weekly" = "weekly",
                                  "monthly" = "monthly"),selected = "daily"),
      h3("Log scale"), 
      checkboxInput("log","Linear Regression", value = FALSE)
  ),
  box(width = 10, plotlyOutput('plot1')),
  box(width=10, DT::dataTableOutput("mytable")),
  box(width=10, DT::dataTableOutput("mytable1"))
)

ui <- dashboardPage(header, sidebar, body,skin = "purple")

server <- function(input, output, session) {
  #updateSelectInput(session, 'airline', choices = files)
  
  etftracker <- function(etf) {
    try({
      etfdb.url <- paste0("https://etfdb.com/etf/",etf,"/#etf-ticker-profile")
      name.xpath <- "/html/body/div[2]/div[6]/h1"
      name <- etfdb.url %>% read_html() %>% html_nodes(xpath = name.xpath) %>% html_text()
      name <- strsplit(name,"\n")
      ticker <- name[[1]][2]
      name1 <- name[[1]][3]
      cat.xpath <- "/html/body/div[2]/div[6]/div[1]/div[1]/div[4]/span[2]/a"
      category <- read_html(etfdb.url) %>% html_nodes(xpath = cat.xpath) %>% html_text()
      index.xpath <- "//*[@id='overview']/div[1]/div/div/div[1]/ul[1]/li[7]/span[2]/a"
      index <- read_html(etfdb.url) %>% html_nodes(xpath = index.xpath) %>% html_text()
      expense.xpath <- "//*[@id='overview']/div[1]/div/div/div[1]/ul[1]/li[3]/span[2]"
      expense <- read_html(etfdb.url) %>% html_nodes(xpath = expense.xpath) %>% html_text()
      div.url <- paste0("https://etfdb.com/etf/",ticker,"/#etf-ticker-valuation-dividend")
      div.xpath <- "//*[@id='dividend-collapse']/div/div[4]/div/div[1]/div[2]/div[1]/span"
      dividend <- read_html(div.url) %>% html_nodes(xpath = div.xpath) %>% html_text()
      x <- cbind(ticker,name1,category,index,expense,dividend)
      #colnames(x) <- c("Ticker","Name","Category","Index","Expense","Dividend")
      #x <- data.frame(x)
      return(x)
    })
  }
  
  etfreturn <- function(ticker) {
    try({
      url <- paste0("https://etfdb.com/etf/",ticker,"/#performance")
      xpath <- "//*[@id='performance-collapse']/div/div[3]"
      name <- read_html(url) %>% html_nodes(xpath = xpath) %>% html_text()
      name <- strsplit(name,"\n")
      name1 <- name[[1]][name[[1]] != ""]
      name1 <- name1[1:16]
      name2 <- matrix(name1,ncol=8)
      name3 <- data.frame(t(name2[1,]))
      names(name3) <- name2[2,]
      return(name3)
    })
  }
  
  datasetInput <- reactive({
    x <- fread(input$airline)
    return(x)
  }) 
  
  observeEvent(input$airline, 
               updateSelectInput(session, 'month',label = "Ticker:", 
                                 choices = datasetInput()$Symbol)
               )
  
  tickerInput <- reactive({
    validate(
      need(input$month != "", "Please select a ticker")
    )
    df <- datasetInput()
    tabl <- subset(df, df$Symbol==input$month)
    table1 <- etftracker(tabl$Symbol)
    return(table1)
  })
  
  returnInput <- reactive({
    validate(
      need(input$month != "", "Please select a ticker")
    )
    table2 <- etfreturn(input$month)
    return(table2)
  })
  
  output$mytable <- DT::renderDataTable({
    tickerInput()
  })
  
  output$mytable1 <- DT::renderDataTable({
    returnInput()
  })
  
  dl <- function(ticker,periodi="daily") {
    stk <- suppressWarnings(getSymbols(ticker, auto.assign = FALSE,src = "yahoo", periodicity = periodi))
    stk <- na.approx(stk)
    stk %>% head()
    names(stk) <- gsub(".*\\.", "", names(stk))
    return(stk)
  }
  
  get.data <- reactive({
    validate(
      need(input$month != "", "Please select a ticker")
    )
    stk <- dl(input$month,input$radio)
    stk1 <- xts::last(stk,paste0(input$slider1, " years"))
    return(stk1)
  })
  
  get.ticker <- reactive({
    stk1 <- get.data()
    if(input$log == FALSE) {
      stk.lm1 <- lm(Cl(stk1) ~ c(1:nrow(stk1)), data = stk1)
      stk1$TL <- coefficients(stk.lm1)[1] + c(1:nrow(stk1))*coefficients(stk.lm1)[2]
      stk.sd <- sd(Cl(stk1)-stk1$TL)
      stk1$up2sd <- stk1$TL + 2*stk.sd
      stk1$up1sd <- stk1$TL + stk.sd
      stk1$dn1sd <- stk1$TL - stk.sd
      stk1$dn2sd <- stk1$TL - 2*stk.sd
      stk1 <- stk1[,c("Close","up2sd","up1sd","TL","dn1sd","dn2sd")]
      return(stk1)
    } else {
      stk.lm1 <- lm(log(Cl(stk1)) ~ c(1:nrow(stk1)), data = stk1)
      stk1$TL <- coefficients(stk.lm1)[1] + c(1:nrow(stk1))*coefficients(stk.lm1)[2]
      stk.sd <- sd(log(Cl(stk1))-stk1$TL)
      stk1$Close <- log(Cl(stk1))
      stk1$up2sd <- qnorm(0.975,stk1$TL,stk.sd)
      stk1$up1sd <- qnorm(0.875,stk1$TL,stk.sd)
      stk1$dn1sd <- qnorm(0.125,stk1$TL,stk.sd)
      stk1$dn2sd <- qnorm(0.025,stk1$TL,stk.sd)
      stk1 <- stk1[,c("Close","up2sd","up1sd","TL","dn1sd","dn2sd")]
      return(stk1)
    }
  })
  
  output$plot1 <- renderPlotly({
    p <- plot_ly(x=index(get.ticker()),y=as.numeric(get.ticker()$Close),type = "scatter", mode = "lines",name="Close") %>% 
      add_trace(y = format(as.numeric(get.ticker()$TL),digits=4),mode = 'lines',name = 'TL', type = 'scatter',color=I('purple')) %>% 
      add_trace(y = format(as.numeric(get.ticker()$up2sd),digits=4),mode = 'lines',name = 'up2sd', type = 'scatter',color=I('red')) %>% 
      add_trace(y = format(as.numeric(get.ticker()$up1sd),digits=4),mode = 'lines',name = 'up1sd', type = 'scatter',color=I('orange')) %>% 
      add_trace(y = format(as.numeric(get.ticker()$dn1sd),digits=4),mode = 'lines',name = 'dn1sd', type = 'scatter',color=I('blue')) %>% 
      add_trace(y = format(as.numeric(get.ticker()$dn2sd),digits=4),mode = 'lines',name = 'dn2sd', type = 'scatter',color=I('green'))
    p
  })
}

shinyApp(ui, server)