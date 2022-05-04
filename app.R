library(shiny)
  
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region", choices = c("Global", "USA", "UK", "Australia", "Brazil", "Canada")),
      selectInput("time", "Select Time", choices = c("Daily","Weekly", "Monthly")),
      #actionButton("Update View", icon("refresh")),
      actionButton("go", "Generate Plots")),
    mainPanel(
      plotOutput("plot"),
      plotOutput("plot2"))
  )
)
server <- function(input, output, session) {
  
  v <- reactiveValues(doText = FALSE)
  
  observeEvent(input$go, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doText <- input$go
  })

  
  output$plot <- renderPlot({
    if (v$doText == FALSE) return()
    isolate({main(input$region, input$time)})
  })
  
  output$plot2 <- renderPlot({
    if (v$doText == FALSE) return()
    isolate({ art_plot })
    
  })
  
  
}

plott <- function(input){
  hist(100)

  }

shinyApp(ui = ui, server = server)
  