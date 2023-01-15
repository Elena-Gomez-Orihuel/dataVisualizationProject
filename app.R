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
              h1("Non-Target variable analysis"),
              h5("In this tab, you will be able to visualize multiple variables that are not the target variable, and 
                 make a comparison between them"),
              selectizeInput("variables", "Select Variables:",
                             choices = NULL,
                             multiple = TRUE),
              plotOutput("scatterplot_matrix", height = "300px"),
              plotOutput("heatmap", height = "300px"),
              plotOutput("mosaic_plot"),
              plotOutput("plot_output", height = "300px")
      )
    )
  )
)

server <- function(input, output, session) {
  #read the slides
  #clustering heatmap
  #depending on quant or categ
  
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
    data <- read.csv(input$file$datapath, header = TRUE)
    updateSelectizeInput(session, "variables", choices = colnames(data))
  })
  
  data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath)
  })
  
  #output$dummy <- renderPlot({
    #selected_vars <- input$variables
    #req(selected_vars)
    #if (length(selected_vars) < 3) {
    #  return("You must select at least 3 variables")
    #}
    #data <- data()
    #heatmap(data[, selected_vars], scale = "column", Colv = NA)
  
  #})
  
  output$scatterplot_matrix <- renderPlot({
    selected_vars <- input$variables
    req(selected_vars)
    if (length(selected_vars) < 2) {
      return("You must select at least 2 variables")
    }
    ggpairs(data()[, selected_vars])
  })
  
  observeEvent(input$variables, {
    selected_vars <- input$variables
    if (!is.null(selected_vars)) {
      # Determine if only quantitative variables were selected
      if (all(selected_vars %in% c("age","trestbps", "chol", "thalac", "oldpeak"))) {
        print("Heatmap")
        output$plot <- renderPlot({
          heatmap(data()[, selected_vars], scale = "column", Colv = NA)
        })
      }
      # Determine if only categorical variables were selected
      else if (all(selected_vars %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "ca", "thal", "target"))) {
        print("Mosaic plot")
        output$plot <- renderPlot({
          mosaicplot(table(data()[, selected_vars]))
        })
      }
      # Determine if both quantitative and categorical variables were selected
      else if (any(selected_vars %in% c("age","trestbps", "chol", "thalac", "oldpeak")) && 
               any(selected_vars %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "ca", "thal", "target"))) {
        print("Scatterplot matrix")
        output$plot <- renderPlot({
          ggpairs(data()[, selected_vars])
        })
      }
      else{
        print("None")
      }
    }
  })
  
  
  
    
    
    # Create the parallel coordinates plot
    #ggparcoord(data_subset, columns = NULL, groupColumn = NULL)
    
    
  
    
  #})
}

shinyApp(ui, server)