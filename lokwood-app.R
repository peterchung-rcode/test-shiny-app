# http://enhancedatascience.com/2017/07/10/the-packages-you-need-for-your-r-shiny-application/
# http://www.worldgovernmentbonds.com/country/hong-kong/

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

pkg<-c("shiny","shinydashboard","quantmod","rvest","dplyr","googleVis","lubridate","zoo","DT","readxl","data.table","httr",
       "gdata","dygraphs","highcharter","plotly")

check.packages(pkg)
enddate <- Sys.Date()
startdate <- rev(seq(Sys.Date(),length=105,by="-1 day"))[1]

options(digits = 4)

header <- dashboardHeader(title = "Stocki")

sidebar <- dashboardSidebar(
  width = 200,
  #sidebarSearchForm(textId = "searchText", buttonId = "searchButton"),
  textInput("ticker", h3("Ticker Symbol"), 
            value = "2800.HK"),
  sidebarMenu(
    menuItem("Lokwood",tabName = "lokwood", icon = icon("dashboard")),
    menuItem("Plotly",tabName = "plotly", icon = icon("dashboard")),
    menuItem("PE",tabName = "pe", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "lokwood",
            fluidRow(
              box(height = 0,width = 2, 
                  sliderInput("slider1", h3("Sliders"),
                              min = 0, max = 10, value = 3.5,step = 0.5),
                  radioButtons("radio", h3("Periodicity"),
                               choices = list("daily" = "daily", "weekly" = "weekly",
                                              "monthly" = "monthly"),selected = "daily"),
                  h3("Log scale"), 
                  checkboxInput("log","Linear Regression", value = FALSE)
                  ),
              box(width = 9, dygraphOutput("allanplot"))
            )
    ),
    tabItem(tabName = "plotly",
            h1("Hello"),
            box(width = 11, plotlyOutput('plot1'))
            )
  )
)

ui <- dashboardPage(header, sidebar, body,skin = "purple")

server <- function(input, output) {
  
  dl <- function(ticker,periodi="daily") {
    stk <- suppressWarnings(getSymbols(ticker, auto.assign = FALSE,src = "yahoo", periodicity = periodi))
    stk <- na.approx(stk)
    stk %>% head()
    names(stk) <- gsub(".*\\.", "", names(stk))
    return(stk)
  }
  
  get.data <- reactive({
    if(is.null(input$ticker))
      return(NULL)
    validate(
      need(input$ticker != "", "Please select a ticker")
    )
    stk <- dl(input$ticker,input$radio)
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
  
  output$allanplot <- renderDygraph({
    dygraph(get.ticker(),main = input$ticker,group="stock") %>%
      dyLegend(show = "follow",width = 100,hideOnMouseOut = FALSE) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = "Price", drawGrid = FALSE) %>%
      dyOptions(axisLineColor = "navy", 
                gridLineColor = "lightblue") %>%
      dyGroup(c("up2sd","up1sd","dn1sd","dn2sd"), color = c("red", "orange", "blue", "green")) %>%
      dyRangeSelector()
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