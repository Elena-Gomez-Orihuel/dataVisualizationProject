library(shiny)
library(readr)
library(ggplot2)
library(shinydashboard)
library(forcats)
library(GGally)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggthemes)
library(psych)
#read the data 
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      fileInput("file", "Upload your dataset"),
      menuItem("Univariate", tabName = "univariate", icon = icon("table")),
      menuItem("Multivariate", tabName = "multivariate", icon = icon("bar-chart")),
      menuItem("Dummy", tabName = "dummy", icon = icon("bar-chart")),
      menuItem("Dummy2", tabName = "dummy2", icon = icon("circle-dot"))
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
              
              actionButton("help", "help", icon = icon("question-circle"), width = "10%"),
              tags$div(id = "help-modal", class = "modal", 
                       tags$div(class = "modal-content",
                                tags$h4("Help"),
                                tags$p("This is some help text that can be closed by clicking the 'x' button or the background."),
                                tags$div(class = "modal-footer", 
                                         actionButton("close", "Close", class = "btn-flat")
                                )
                       )
              ),
              
              #selectizeInput("variables", "Select Variables:",
            #               choices = NULL,
              #               multiple = TRUE),
              selectizeInput("quant_vars", "Select quantitative variables", choices =NULL, multiple = TRUE),
              selectizeInput("cat_vars", "Select categorical variables", choices = NULL, multiple = TRUE),
              plotOutput("plot")
      ),
      tabItem(tabName = "dummy2",
              h1("This is the Multivariate Analysis Tab focused on the target"),
              h5("In this tab, you will be able to visualize multiple variables using the target variable, and 
                 make a comparison between them"),
              selectizeInput("variablesForTarget", "Select Variables:",
                             choices = NULL,
                             multiple = TRUE),
              plotOutput("plotT")
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
    if(input$uni_var_select %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "ca", "thal", "target")) {
      removeUI(selector = "div:has(> #bins)")
      
    }
    
    else if(input$uni_var_select %in% c("age","trestbps", "chol", "thalac", "oldpeak")) {
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
          if(input$uni_var_select %in% c("age","trestbps", "chol", "thalach", "oldpeak")) {
            ggplot(data(), aes_string(x = input$uni_var_select)) +
              geom_histogram(color = "yellow", fill = "blue", binwidth = 1, boundary = 0, breaks = seq(min(data()[,input$uni_var_select]), max(data()[,input$uni_var_select]), (max(data()[,input$uni_var_select]) - min(data()[,input$uni_var_select]))/input$bins))}
          
        else if(input$uni_var_select %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "ca", "thal", "target")) {
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
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Help",
      "This is some help text that can be closed by clicking the 'x' button or the background.",
      footer = tagList(
        modalButton("Close")
      )
    ))
  })
  #Dummy
  observeEvent(input$file, {
    data <- read.csv(input$file$datapath, header = TRUE)

    updateSelectizeInput(session, "quant_vars", choices = c("age","trestbps", "chol", "thalach", "oldpeak", "ca"))
    updateSelectizeInput(session, "cat_vars", choices = c("sex","cp", "fbs", "restecg", "exang", "slope", "thal"))

    updateSelectizeInput(session, "variablesForTarget", choices = colnames(data))

  })
  
  
  data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath)
  })
  
  #dummy  OLD
  observeEvent(input$variables, {
    selected_vars <- input$variables
    if (!is.null(selected_vars)) {
      # Determine if only quantitative variables were selected
      if (all(selected_vars %in% c("age","trestbps", "chol", "thalac", "oldpeak", "ca"))) {
        print("Scatterplot quantitative")
        output$plot <- renderPlot({
          #my_cols <- c("#00AFBB", "#E7B800", "#FC4E07", "#FF5050","#00FF7F" , "#660099")  
          ggpairs(data()[, selected_vars])#heatmap(data()[, selected_vars], scale = "column", Colv = NA)
        })
        #output$mosaic_plot <- renderPlot({})
        #output$scatterplot_matrix <- renderPlot({})
      }
      # Determine if only categorical variables were selected
      else if (all(selected_vars %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "thal"))) {
        print("Mosaic plot")
        output$plot <- renderPlot({
          mycolors <- brewer.pal(8, "Dark2")
          mosaicplot(table(data()[, selected_vars]), main="My mosaic plot", col=mycolors)
          legend("topright",legend=colnames(data()[, selected_vars]),fill=mycolors)
          #data() %>% 
          #  select(selected_vars) %>% 
          # gather() %>% 
          # group_by(key, value) %>% 
          # summarise(count = n()) %>% 
          # ggplot(aes(x = key, y = value, fill = count)) +
          # geom_tile() +
          # scale_fill_gradient(low = "white", high = "blue") +
          # theme_void() + 
          # labs(title = "Mosaic plot of selected variables", x = "Variables", y = "Values", fill = "Count") +
          # guides(fill = guide_colorbar(title = "Count"))
        
        })
        #output$heatmap <- renderPlot({})
        #output$scatterplot_matrix <- renderPlot({})
      }
      # Determine if both quantitative and categorical variables were selected
      else if (any(selected_vars %in% c("age","trestbps", "chol", "thalac", "oldpeak", "ca")) && 
               any(selected_vars %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "thal"))) {
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
  
  data_subset_quant <- reactive({
    data()[, input$quant_vars]
  })
  #dummy NEW
  observeEvent(c(input$quant_vars, input$cat_vars), {
    #ONLY QUANTITATIVE BIVARIATE
    print("Hello")
    print(length(input$quant_vars))
    print(length(input$cat_vars))
    if ((length(input$quant_vars) == 2)&&(length(input$cat_vars) == 0)) {
      #if (length(input$cat_vars) == 0) {
        print("ONLY QUANTITATIVE BIVARIATE")
        output$plot <- renderPlot({
          selected_vars <- strsplit(input$quant_vars, ",")[[1]]
          selected_vars2 <- strsplit(input$quant_vars, ",")[[2]]
          #print("selected vars")
          #print(selected_vars)
          #print(selected_vars2)
          x_var <- selected_vars
          y_var <- selected_vars2
          
          ggplot(data(), aes(x = data()[,x_var], y = data()[,y_var])) +
            geom_point() +
            geom_smooth() +
            ggtitle(paste("Comparison of", x_var, "and", y_var)) +
            xlab(x_var) +
            ylab(y_var)
        })
      #}
    }
    #ONLY CATEGORICAL BIVARIATE
    else if ((length(input$quant_vars) == 0)&&(length(input$cat_vars) == 2)) {
      
      #if (length(input$cat_vars) == 2) {
        print("ONLY CATEGORICAL BIVARIATE")
        #barplot
        output$plot <- renderPlot({
          # Split the selected variables into x and y
          x_var <- strsplit(input$cat_vars, ",")[[1]]
          y_var <- strsplit(input$cat_vars, ",")[[2]]
          
          mycolors <- brewer.pal(8, "Dark2")
          mosaicplot(table(data()[, input$cat_vars]), main="Mosaic plot", col=mycolors)
          legend("topright",legend=colnames(data()[, input$cat_vars]),fill=mycolors)
        })
      #}
    }
    #ONLY QUANTITATIVE MULTIPLE
    else if ((length(input$quant_vars) > 2)&&(length(input$cat_vars) == 0)) {
      #if (length(input$cat_vars) == 0) {
        print("ONLY QUANTITATIVE MULTIPLE")
        output$plot <- renderPlot({
          ggpairs(data()[, input$quant_vars])
        })
      #}
    }
    #ONE QUANT ONE CAT
    else if ((length(input$quant_vars) == 1)&&(length(input$cat_vars) == 1)) {
      #if (length(input$cat_vars) == 1) {
        print("ONE QUANT ONE CAT")
        #Plot(Salary, Dept)
        output$plot <- renderPlot({
          x_var <- strsplit(input$quant_vars, ",")[[1]]
          y_var <- strsplit(input$cat_vars, ",")[[1]]
          ggplot(data(), aes(x=data()[,x_var], fill=data()[,y_var])) + 
            geom_histogram() + 
            facet_wrap(data()[,y_var]) + 
            xlab(x_var) +
            ylab(y_var)
        })
      #}
    }
    #TWO QUANT ONE CAT
    
    else if ((length(input$quant_vars) == 2)&&(length(input$cat_vars) == 1)) {
      #if (length(input$cat_vars) == 1) {
        #Plot(Salary, Dept)
        print("INSIDE")
        output$plot <- renderPlot({
          x_var <- strsplit(input$quant_vars, ",")[[1]]
          y_var <- strsplit(input$quant_vars, ",")[[2]]
          z_var <- strsplit(input$cat_vars, ",")[[1]]
          print(x_var)
          print(y_var)
          print(z_var)
          ggplot(data(), aes(x=data()[,x_var], y=data()[,y_var], color=data()[,z_var])) + 
            geom_point() + 
            facet_wrap(~data()[,z_var], scales = "free") + 
            xlab(x_var) +
            ylab(y_var)
        })
      #}
    }
    #ONE QUANT TWO CAT
    else if ((length(input$quant_vars) == 1)&&(length(input$cat_vars) == 2)) {
        #Plot(Salary, Dept)
      output$plot <- renderPlot({
        x_var <- strsplit(input$cat_vars, ",")[[1]]
        y_var <- strsplit(input$cat_vars, ",")[[2]]
        z_var <- strsplit(input$quant_vars, ",")[[1]]
        
        ggplot(data(), aes(x = as.factor(data()[,x_var]), y = data()[,z_var],fill = data()[,y_var])) + 
          geom_boxplot() + 
          facet_wrap(~data()[,y_var]) + 
          #facet_grid(cols = vars(as.character(y_var))) + 
          xlab(x_var) + 
          ylab(z_var)
      })
    }
    
    #THREE CAT
    else if ((length(input$quant_vars) == 0)&&(length(input$cat_vars) == 3) ) {
      x_var <- strsplit(input$cat_vars, ",")[[1]]
      y_var <- strsplit(input$cat_vars, ",")[[2]]
      z_var <- strsplit(input$cat_vars, ",")[[3]]
      output$plot <- renderPlot({
        ggplot(data(), aes(x = data()[,x_var], fill = y_var)) + 
          geom_bar(position = "stack") + 
          facet_wrap(~data()[,z_var]) + 
          xlab(x_var) + 
          ylab(y_var) + 
          scale_fill_brewer(palette = "Set1")
      })
      
    }
    #THREE CAT
    else if ((length(input$quant_vars) == 0)&&(length(input$cat_vars) == 4)) {
      
        print("BAD4")
        #ggplot(data_subset(), aes_string(x = input$cat_vars[1], fill = input$cat_vars[2])) + 
        # geom_bar(position = "fill") + 
        # facet_grid(.~input$cat_vars[3], input$cat_vars[4])
      
    }
    else if (length(input$quant_vars) + length(input$cat_vars) < 1) {
      print("Select at least two variables")
    }
    else{
      output$plot <- renderPlot({
        
        selected_vars <- c(input$quant_vars, input$cat_vars)
        ggpairs(data()[, selected_vars])
      })
    }
    
  })
    
    # Create the parallel coordinates plot
    #ggparcoord(data_subset, columns = NULL, groupColumn = NULL)
    
  
  
  
  #dummy2
  observeEvent(input$variablesForTarget, {
    selected_vars <- input$variablesForTarget
    #TODO: lock "target" inside the selected_vars
    if (!is.null(selected_vars)) {
      # Determine if only numerical variables were selected
      if (all(selected_vars %in% c("age","trestbps", "chol", "thalac", "oldpeak", "ca"))) {
        print("only numerical variables are selected")
        #TODO - from box plot to nested box plot
      }
      # Determine if only categorical variables were selected
      else if (all(selected_vars %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "thal"))) {
        print("only numerical categorical are selected")
        #TODO - from Strip plot to swarn plot
      }
      # Determine if both quantitative and categorical variables were selected
      else if (any(selected_vars %in% c("age","trestbps", "chol", "thalac", "oldpeak", "ca")) && 
               any(selected_vars %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "thal"))) {
        print("both numerical and categorical variables are selected")
        #TODO - from bar chart to stacked bar chart (divide the numerical bins of range)
      }
      else{
        ggpairs(data()[, input$quant_vars])
      }
    }
  })
}

shinyApp(ui, server)