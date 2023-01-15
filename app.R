library(shiny)
library(readr)
library(ggplot2)
library(shinydashboard)
library(forcats)
library(GGally)
library(RColorBrewer)
library(dplyr)
library(tidyr)
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
              plotOutput("unianalysis"),
              #sliderInput("bins", "Number of bins", min = 1, max = 30, value = 10, step = 1)
              
              #if (input$uni_var_select %in% c('sex','cp', 'fbs', 'restecg', 'exang', 'slope', 'thal', 'target')){ 
              #sliderInput("bins", "Number of bins", min = 1, max = 30, value = 10, step = 1)
              #}
              
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
              plotOutput("plot"),
              #plotOutput("scatterplot_matrix", height = "300px"),
              #plotOutput("heatmap", height = "300px"),
              #plotOutput("mosaic_plot"),
              #plotOutput("plot_output", height = "300px")
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
  
  #Adding and removing the slideBar depending on the type of the selected variable (ordered or categorical)
  observeEvent(input$uni_var_select,{
    #with categorical variables we won't have a slideBar
    if(input$uni_var_select %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "thal", "target")) {
      removeUI(selector = "div:has(> #bins)")
      
    }
    
    else if(input$uni_var_select %in% c("age","trestbps", "chol", "thalac", "oldpeak", "ca")) {
      removeUI(selector = "div:has(> #bins)")
      insertUI(
        selector = "#unianalysis", 
        where = "afterEnd", 
        ui = sliderInput("bins", "Number of bins", min = 1, max = 30, value = 10, step = 1)
      )
    }
  })
  
  output$unianalysis <- renderPlot({
    if(!is.null(data())){
      req(input$uni_var_select)
          if(input$uni_var_select %in% c("age","trestbps", "chol", "thalac", "oldpeak", "ca")) {
            ggplot(data(), aes_string(x = input$uni_var_select)) +
              geom_histogram(fill = "blue", binwidth = 1, boundary = 0, breaks = seq(min(data()[,input$uni_var_select]), max(data()[,input$uni_var_select]), (max(data()[,input$uni_var_select]) - min(data()[,input$uni_var_select]))/input$bins))}
          
        else if(input$uni_var_select %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "thal", "target")) {
          ggplot(data(), aes_string(x = input$uni_var_select, fill = as.factor(data()[,input$uni_var_select]))) +
            geom_bar(stat = "count") + 
            scale_x_discrete() +
            labs(y = "Count") + 
            scale_fill_brewer(palette = "Set1")
          }
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
  
  #output$scatterplot_matrix <- renderPlot({
  #  selected_vars <- input$variables
  #  req(selected_vars)
  #  if (length(selected_vars) < 2) {
  #    return("You must select at least 2 variables")
  #  }
  #  ggpairs(data()[, selected_vars])
  #})
  
  observeEvent(input$variables, {
    selected_vars <- input$variables
    if (!is.null(selected_vars)) {
      # Determine if only quantitative variables were selected
      if (all(selected_vars %in% c("age","trestbps", "chol", "thalac", "oldpeak"))) {
        print("Heatmap")
        output$plot <- renderPlot({
          ggpairs(data()[, selected_vars])#heatmap(data()[, selected_vars], scale = "column", Colv = NA)
        })
        #output$mosaic_plot <- renderPlot({})
        #output$scatterplot_matrix <- renderPlot({})
      }
      # Determine if only categorical variables were selected
      else if (all(selected_vars %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "thal", "ca"))) {
        print("Mosaic plot")
        output$plot <- renderPlot({
          #mycolors <- brewer.pal(8, "Dark2")
          #mosaicplot(table(data()[, selected_vars]), main="My mosaic plot", col=mycolors)
          #legend("topright",legend=colnames(data()[, selected_vars]),fill=mycolors)
          data() %>% 
            select(selected_vars) %>% 
            gather() %>% 
            group_by(key, value) %>% 
            summarise(count = n()) %>% 
            ggplot(aes(x = key, y = value, fill = count)) +
            geom_tile() +
            scale_fill_gradient(low = "white", high = "blue") +
            theme_void() + 
            labs(title = "Heatmap of selected variables", x = "Variables", y = "Values", fill = "Count") +
            guides(fill = guide_colorbar(title = "Count"))
        
        })
        #output$heatmap <- renderPlot({})
        #output$scatterplot_matrix <- renderPlot({})
      }
      # Determine if both quantitative and categorical variables were selected
      else if (any(selected_vars %in% c("age","trestbps", "chol", "thalac", "oldpeak")) && 
               any(selected_vars %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "thal", "ca"))) {
        print("Scatterplot matrix")
        output$plot <- renderPlot({
          ggpairs(data()[, selected_vars])
        })
        #output$heatmap <- renderPlot({})
        #output$mosaic_plot <- renderPlot({})
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