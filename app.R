library(shiny)
library(readr)
library(ggplot2)
library(shinydashboard)
library(forcats)
library(GGally)
#read the data 
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      fileInput("file", "Upload your dataset"),
      menuItem("Univariate", tabName = "univariate", icon = icon("table")),
      menuItem("Multivariate", tabName = "multivariate", icon = icon("bar-chart")),
      menuItem("Dummy", tabName = "dummy", icon = icon("bar-chart"))
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
      ),
      tabItem(tabName = "dummy",
              h1("This is the Dummy Analysis Tab :D"),
              #This is going tobe oriented to have at least three variables as this is a 
              #multivariate analysis, so i find doing univariate or bivariate useless
              #as we already have the two other tabs
              selectizeInput("variables", "Select Variables:", choices = NULL, multiple = TRUE
                             ),
              plotOutput("dummy")
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
    req(input$uni_var_select)
    if(input$uni_var_select %in% c("age","trestbps", "chol", "thalac", "oldpeak")) {
      ggplot(data(), aes_string(x = input$uni_var_select)) +
        geom_histogram(fill = "blue")
    }
    
    else if(input$uni_var_select %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "ca", "thal", "target")) {
      ggplot(data(), aes_string(x = input$uni_var_select, fill = as.factor(data()[,input$uni_var_select]))) +
        geom_bar(stat = "count") +
        scale_x_discrete(limits = levels(as.factor(data()[,input$uni_var_select]))) + 
        labs(y = "Count") + 
        scale_fill_brewer(palette = "Set1")
      
      
      
      #ggplot(data(), aes_string(x = input$uni_var_select, fill = as.factor(data()[,input$uni_var_select]))) +
      #  geom_bar(stat = "count") + 
      #  scale_x_discrete(labels = as.factor(data()[,input$uni_var_select])) +
      #  labs(y = "Count") + 
      #  scale_fill_brewer(palette = "Set1")
    }
  })
  #MULTIVARIATE
  output$multianalysis <- renderPlot({
    if(input$checkbox){
      #okey so i would do the following: select a variable and then calculate correlations
      #with all of the other variables. Plot the three strongest correlations
    }
    else{
      #Leo's code goes here :)
      #hist(rnorm(1000))
    }
  })
  #Dummy
  observeEvent(input$file, {
    # Read the data from the uploaded file
    data <- read.csv(input$file$datapath, header = TRUE)
    
    # Update the selectize input widget with the column names of the new data
    updateSelectizeInput(session, "variables", choices = colnames(data))
  })
  output$dummy <- renderPlot({
    selected_vars <- reactive({input$variables})
    req(selected_vars)
    if (length(selected_vars) < 3) {
      return("You must select at least 3 variables")
    }
    
    # Define the variable groups
    group1 <- c("age", "trestbps", "chol", "thalac", "oldpeak")
    group2 <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal", "target")
    
    # Count the number of variables in each group
    n_group1 <- length(intersect(input$variables, group1))
    n_group2 <- length(intersect(input$variables, group2))
    
    
    # Subset the data to only include the selected variables
    data_subset <- data[, selected_vars, drop = FALSE]
    
    # Create the parallel coordinates plot
    ggparcoord(data_subset, columns = NULL, groupColumn = NULL)
    
    
  
    
  })
}

shinyApp(ui, server)