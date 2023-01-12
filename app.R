library(shiny)
library(readr)
library(ggplot2)

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
      plotOutput("histogram")
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
  
  output$histogram <- renderPlot({
    req(input$variable)
    ggplot(data(), aes_string(x = input$variable)) +
      geom_histogram()
  })
  
  })
  
  observeEvent(input$multivariate, {
    # bivariate analysis code
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)