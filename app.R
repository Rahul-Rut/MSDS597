library(shiny)
  

region_choices = c("Global", "USA", "UK", "Australia", "Brazil", "Canada") #used in dropdown for region

ui <- fluidPage(
  sidebarLayout(   #defining side bar 
    sidebarPanel(    #sidebar panel
      selectInput("region", "Select Region", choices = region_choices ),  #Defining dropdown with choices
      selectInput("time", "Select Time", choices = c("Daily","Weekly", "Monthly")),  #same as last but for time
      actionButton("go", "Generate Plots")),  #a button, which when pressed, prints the plots! Think of it as buffer
    mainPanel(  #main panel for output
      plotOutput("plot"),    #plotting plot 1
      plotOutput("plot2"))   #plotting for plot 2
  )
)
server <- function(input, output, session) {
  
  v <- reactiveValues(doText = FALSE)   #this will be used to store reactive value or keep checking whether a button was pressed or not
  
  observeEvent(input$go, {  #we observe changes in the button "Generate Plots" since "go" is id for that button

    v$doText <- input$go    #and store its output in the reactive value!
  })

  
  output$plot <- renderPlot({   #need render plot to, well, render plots!
    if (v$doText == FALSE) return()   #do nothing if the "Plot" button has not been touched
    
    
    isolate({main(input$region, input$time)})   #but when it is pressed by the user, run the "main" function whose return value is a ggplot object andd hence can be printed!
  })
  
  output$plot2 <- renderPlot({   #same but for the other plot!
    if (v$doText == FALSE) return() 
    isolate({ art_plot })
    
  })
  
  
}


shinyApp(ui = ui, server = server)
  