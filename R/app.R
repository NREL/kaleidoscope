# 
library(shiny)

#source('plot.ergis.R')
    
ui <- fluidPage(
  headerPanel('ERGIS Prototype'),
  sidebarPanel(
      column(6,
      checkboxGroupInput("uiGenerators", "Generators",
                         choices=list("Wind"="Wind",
                                      "PV"="PV",
                                      "Hydro"="Hydro",
                                      "Coal"="Coal",
                                      "Gas CC" ="Gas CC",
                                      "CT/Gas boiler"="CT/Gas boiler",
                                      "Nuclear"="Nuclear",
                                      "Pumped Storage"=7,
                                      "Other"="Other"),
                         selected=list("Wind", "PV", "Hydro", "Coal", "Nuclear", "Gas CC", "CT/Gas boiler"))),
      column(6,
      radioButtons("uiDensity", "Density",
                   choices=c("None"="None",
                             "Wind"="Wind",
                             "PV"="PV",
                             "Hydro"="Hydro",
                             "Coal"="Coal",
                             "Gas CC" ="Gas CC",
                             "CT/Gas boiler"="CT/Gas boiler",
                             "Nuclear"="Nuclear"),
                         selected="Wind")),
      hr(),
      fluidRow(column(12,
                      dateInput('uiTime', 'Analysis date:', value = "2006-04-01"))),
      fluidRow(column(12,
                      sliderInput("uiOffset", "Time:", 0, 1440, 0, step=5)))),
  mainPanel(
      plotOutput("uiMapPlot", height='640px'))
)

compute_date <- function(date, offset)
{
    as.POSIXlt(date, tz="GMT") + offset * 60;
}

server <- function(input, output)
{    
    output$uiMapPlot <- renderPlot(
    {
        t <- compute_date(input$uiTime, input$uiOffset)
        
        par(mar=c(0,0,0,0),oma=c(0,0,0,0),bg='#656565')

        draw_ergis(t, density=input$uiDensity, generators=input$uiGenerators)
    })
}

shinyApp(ui = ui, server = server)
