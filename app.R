library(shiny)
library(readr)
library(ggplot2)
library(base)

#read the data 


# Define UI ----
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      h2("Exploratory Data Analysis"),
      fileInput("file", "Choose CSV File"),
      p("Select the analysis you want to perform"),
      #In the actionButton function the first argument of the function is the ID of the button
      #and the second argument is the label that will be displayed on the button.
      actionButton("univariate", "Unidimensional"),
      br(),
      br(),
      actionButton("multivariate", "Multidimensional")

    ),
    mainPanel(
      h1("Analysis"),
      br(),
      uiOutput("varSelect"),
      plotOutput("univariatePlot"), 

    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  #reading the data
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  observeEvent(input$univariate, {
  #code for when univariate button is pressed
    
  output$varSelect <- renderUI({
    selectInput("variable", "Select a variable", choices = colnames(data()))
  })
  

  output$univariatePlot <- renderPlot({
    #histograms are plotted for continuous variables only
    if(input$variable %in% c("age","trestbps", "chol", "thalac", "oldpeak")) { 
    req(input$variable)
    ggplot(data(), aes_string(x = input$variable)) +
      geom_histogram(binwidth = 1, color = "white", fill = "blue", boundary = 0, bins = input$bins) +
      scale_x_continuous(limits = c(min(data()[[input$variable]]), max(data()[[input$variable]])))
    }
    else if(input$variable %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "ca", "thal", "target")) {
      ggplot(data(), aes(x = factor(input$variable))) +
        geom_bar(aes(group=factor(input$variable)), stat = "count", fill = "blue")
      
    }
  })
  
  output$barplot <- renderPlot({
    #histograms are plotted for continuous variables only
    if(input$variable %in% c("age","trestbps", "chol", "thalac", "oldpeak")) return(NULL) 
    #ggplot(data(), aes(x = input$variable)) +
     #geom_bar(stat = "count")
    barplot(input$variable, main = paste("Bar chart of",toString(input$variable)), xlab = toString(input$variable), ylab = "Count", col = "blue")
  })
  
  })
  
  observeEvent(input$multivariate, {
    # bivariate analysis code
  })
}


  


# Run the app ----
shinyApp(ui = ui, server = server)