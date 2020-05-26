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


options("getSymbol.warning4.0"=FALSE)
options("getSymbol.auto.assign"=FALSE)

files <- list.files(path = "data",pattern = "world-resitlist",recursive = T,full.names = T)
sheets <- excel_sheets(files)
# rl <- lapply(sheets, function(x) read_excel(files, sheet = x))

header <- dashboardHeader(title = "REITs List")

sidebar <- dashboardSidebar(
  selectInput(
    inputId = "ss",
    label = "Spreadsheets:",
    choices = sheets,
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
  box(width=10, DT::dataTableOutput("mytable"))
)

ui <- dashboardPage(header, sidebar, body,skin = "purple")

server <- function(input, output, session) {
  #updateSelectInput(session, 'airline', choices = files)
  
  datasetInput <- reactive({
    x <- read_excel(files,sheet = input$ss)
    return(x)
  }) 
  
  observeEvent(input$ss, 
               updateSelectInput(session, 'month',label = "Ticker:", 
                                 choices = datasetInput()$Symbol)
  )
  
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
      layout(title = input$month) %>%
      add_trace(y = format(as.numeric(get.ticker()$TL),digits=4),mode = 'lines',name = 'TL', type = 'scatter',color=I('purple')) %>% 
      add_trace(y = format(as.numeric(get.ticker()$up2sd),digits=4),mode = 'lines',name = 'up2sd', type = 'scatter',color=I('red')) %>% 
      add_trace(y = format(as.numeric(get.ticker()$up1sd),digits=4),mode = 'lines',name = 'up1sd', type = 'scatter',color=I('orange')) %>% 
      add_trace(y = format(as.numeric(get.ticker()$dn1sd),digits=4),mode = 'lines',name = 'dn1sd', type = 'scatter',color=I('blue')) %>% 
      add_trace(y = format(as.numeric(get.ticker()$dn2sd),digits=4),mode = 'lines',name = 'dn2sd', type = 'scatter',color=I('green'))
    p
  })
  
  reitsinfo <- function(ticker) {
    try({
      url <- paste0("https://finance.yahoo.com/quote/",ticker,"/")
      xpath <- "//*[@id='quote-summary']/div[2]/table"
      info <- url %>% read_html() %>% html_nodes(xpath = xpath) %>% html_table()
      info.df <- data.frame(t(info[[1]]),stringsAsFactors = F)
      names(info.df) <- info.df[1,]
      info.df <- info.df[-1,]
      name.xpath <- "//*[@id='quote-header-info']/div[2]/div[1]/div[1]/h1"
      Name <- url %>% read_html() %>% html_nodes(xpath = name.xpath) %>% html_text()
      df <- cbind(Name,info.df)
      rownames(df) <- NULL
      return(df)
    })
  }
  
  returnInput <- reactive({
    validate(
      need(input$month != "", "Please select a ticker")
    )
    table2 <- reitsinfo(input$month)
    return(table2)
  })
  
  output$mytable <- DT::renderDataTable({
    returnInput()
  })
}

shinyApp(ui, server)