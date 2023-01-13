library(shiny)
library(readr)
library(ggplot2)
<<<<<<< HEAD
library(shinydashboard)
=======
library(base)

>>>>>>> adea3543096985ec057003e646847a9b51a2f57a
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

<<<<<<< HEAD
#    ),
#    mainPanel(
#      h1("Analysis"),
#      br(),
#      uiOutput("varSelect"),
#      plotOutput("histogram")
#    )
#  )
#)
=======
    ),
    mainPanel(
      h1("Analysis"),
      br(),
      uiOutput("varSelect"),
      plotOutput("univariatePlot"), 

    )
  )
)
>>>>>>> adea3543096985ec057003e646847a9b51a2f57a

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
<<<<<<< HEAD
#  output$varSelect <- renderUI({
#    selectInput("variable", "Select a variable", choices = colnames(data()))
#  })
  
#  output$histogram <- renderPlot({
#    req(input$variable)
#    ggplot(data(), aes_string(x = input$variable)) +
#      geom_histogram()
#  })
=======
    
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
>>>>>>> adea3543096985ec057003e646847a9b51a2f57a
  
#  }) 
  
#  observeEvent(input$multivariate, {
    # bivariate analysis code
#  })
#}


  


# Run the app ----
#shinyApp(ui = ui, server = server)