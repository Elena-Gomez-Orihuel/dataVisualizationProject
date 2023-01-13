library(shiny)
library(readr)
library(ggplot2)
library(shinydashboard)
#read the data 
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      fileInput("file", "Upload your dataset"),
      menuItem("Univariate", tabName = "univariate", icon = icon("table")),
      menuItem("Multivariate", tabName = "multivariate", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "univariate", 
              h1("This is the Univariate Analysis Tab :D"),
              selectInput("uni_var_select", "Select Variable to analyze", choices = NULL, selected = NULL),
              plotOutput("unianalysis")
      ),
      
      tabItem(tabName = "multivariate",
              h1("This is the Multivariate Analysis Tab :D"),
              checkboxInput("checkbox", "Compare to the target variable Heart Attack?", value = FALSE),
              plotOutput("multianalysis")
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath)
  })
  # UNIVARIATE
  #update the variable select input
  observe({
    updateSelectInput(session, "uni_var_select", choices = names(data()), selected = names(data())[1])
  })
  output$unianalysis <- renderPlot({
    #this is the code i saw that you had maria, i just changed the name of the input variable 
    #so we can differentiate our variables better <3
    if(!is.null(data())){
      req(input$uni_var_select)
        ggplot(data(), aes_string(x = input$uni_var_select)) +
          geom_histogram()
    }
  })
  #MULTIVARIATE
  output$multianalysis <- renderPlot({
    if(input$checkbox){
      #Elena's code goes here
      #hist(data()$column_name)
    }
    else{
      #Leo's code goes here :)
      #hist(rnorm(1000))
    }
  })
}

shinyApp(ui, server)

# Define UI ----
#ui <- fluidPage(
#  titlePanel("My Shiny App"),
# sidebarLayout(
#   sidebarPanel(
#     h2("Exploratory Data Analysis"),
#     fileInput("file", "Choose CSV File"),
#      p("Select the analysis you want to perform"),
      #In the actionButton function the first argument of the function is the ID of the button
      #and the second argument is the label that will be displayed on the button.
#      actionButton("univariate", "Unidimensional"),
 #     br(),
#      br(),
#      actionButton("multivariate", "Multidimensional")

#    ),
#    mainPanel(
#      h1("Analysis"),
#      br(),
#      uiOutput("varSelect"),
#      plotOutput("histogram")
#    )
#  )
#)

# Define server logic ----
#server <- function(input, output) {
  
  #reading the data
#  data <- reactive({
#    req(input$file)
#    read_csv(input$file$datapath)
#  })
#  
#  observeEvent(input$univariate, {
  #code for when univariate button is pressed
#  output$varSelect <- renderUI({
#    selectInput("variable", "Select a variable", choices = colnames(data()))
#  })
  
#  output$histogram <- renderPlot({
#    req(input$variable)
#    ggplot(data(), aes_string(x = input$variable)) +
#      geom_histogram()
#  })
  
#  }) 
  
#  observeEvent(input$multivariate, {
    # bivariate analysis code
#  })
#}

# Run the app ----
#shinyApp(ui = ui, server = server)