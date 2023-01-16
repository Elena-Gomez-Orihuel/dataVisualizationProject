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
      #menuItem("Multivariate", tabName = "multivariate", icon = icon("bar-chart")),
      menuItem("Target Analysis", tabName = "dummy2", icon = icon("circle-dot")),
      menuItem("Non-target Analysis", tabName = "dummy", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "univariate", 
              h1("Univariate Analysis"),
              selectInput("uni_var_select", "Select Variable to analyze", choices = NULL, selected = NULL),
              plotOutput("unianalysis")
              #sliderInput("bins", "Number of bins", min = 1, max = 30, value = 10, step = 1)
              
              #if (input$uni_var_select %in% c('sex','cp', 'fbs', 'restecg', 'exang', 'slope', 'thal', 'target')){ 
              #sliderInput("bins", "Number of bins", min = 1, max = 30, value = 10, step = 1)
              #}
              
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
            textOutput("text"),  
            plotOutput("plot")
      ),
      tabItem(tabName = "dummy2",
              h1("This is the Multivariate Analysis Tab focused on the target"),
              h5("In this tab, you will be able to visualize multiple variables, and use them for interact with the target variable"),
              selectInput("variable1", "Select a column:", choices = names(data)),
              selectInput("variable2", "Select a column:", choices = names(data)),
              checkboxInput("isTarget", label = "Do you wants to include the target variable?", value = FALSE),
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

  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Help",
      HTML("This is a help section where you can have in mind all of the 
      different types of plots that you can do in this tab by selecting 
      different combinations of quantitative and categorical variables. 
      You can select the variables in the two selectors that are just below
      this button. If you want to delete any of the variables, click at the 
      right side of the name of the variable, then press the delete key on 
      you keyboard. These are the plots: <br>
           <strong>·Quantitative Bivariate Analysis:</strong><br>
           Select two quantitative variables.<br>
           <strong>·Categorical Bivariate Analysis.</strong><br>
           Select two quantitative variables.<br>
           <strong>·Multiple Quantitative variable Analysis.</strong><br>
           Select more than 2 quantitative variables.<br>
           <strong>·Bivariate analysis One Quantitative One Categorical.</strong><br>
           Select one quantitative variable and one categorical variable.<br>
           <strong>·Multianalysis Two Quantitative One Categorical.</strong><br>
           Select two quantitative variables and one categorical variable<br>
           <strong>·Multianalysis One Quantitative Two Categorical.</strong><br>
           Select one quantitative variable and two categorical variables.<br>
           <strong>·Multianalysis other combinations.</strong><br>
           Select any other combination of quantitative and categorical variables.<br>
           "),
      footer = tagList(
        modalButton("Close")
      )
    ))
  })
  #Dummy
  observeEvent(input$file, {
    data <- read.csv(input$file$datapath, header = TRUE)

    updateSelectizeInput(session, "variables", choices = colnames(data))
    updateSelectInput(session, "variable1", choices = setdiff(colnames(data), "target"))
    updateSelectInput(session, "variable2", choices = setdiff(colnames(data), "target"))
    
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
  
  data_subset_quant <- reactive({
    data()[, input$quant_vars]
  })
  #Non-target
  observeEvent(c(input$quant_vars, input$cat_vars), {
    #ONLY QUANTITATIVE BIVARIATE
    #print("Hello")
    #print(length(input$quant_vars))
    #print(length(input$cat_vars))
    if (length(input$quant_vars) + length(input$cat_vars) < 2) {
      output$text <- renderText({
        selected_vars <- c(input$quant_vars, input$cat_vars)
        if (length(selected_vars) < 2) {
          return("Select at least two variables")
        } else {
          return(NULL)
        }
      })
    }
    else if ((length(input$quant_vars) == 2)&&(length(input$cat_vars) == 0)) {
      
        #print("ONLY QUANTITATIVE BIVARIATE")
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
    }
    #ONLY CATEGORICAL BIVARIATE
    else if ((length(input$quant_vars) == 0)&&(length(input$cat_vars) == 2)) {
        #print("ONLY CATEGORICAL BIVARIATE")
        output$plot <- renderPlot({
          # Split the selected variables into x and y
          x_var <- strsplit(input$cat_vars, ",")[[1]]
          y_var <- strsplit(input$cat_vars, ",")[[2]]
          
          mycolors <- brewer.pal(8, "Dark2")
          mosaicplot(table(data()[, input$cat_vars]), main="Mosaic plot", col=mycolors)
          legend("topright",legend=colnames(data()[, input$cat_vars]),fill=mycolors)
        })
    }
    #ONLY QUANTITATIVE MULTIPLE
    else if ((length(input$quant_vars) > 2)&&(length(input$cat_vars) == 0)) {
        #print("ONLY QUANTITATIVE MULTIPLE")
        output$plot <- renderPlot({
          ggpairs(data()[, input$quant_vars])
        })
    }
    #ONE QUANT ONE CAT
    else if ((length(input$quant_vars) == 1)&&(length(input$cat_vars) == 1)) {
        #print("ONE QUANT ONE CAT")
        output$plot <- renderPlot({
          x_var <- strsplit(input$quant_vars, ",")[[1]]
          y_var <- strsplit(input$cat_vars, ",")[[1]]
          ggplot(data(), aes(x=data()[,x_var], fill=data()[,y_var])) + 
            geom_histogram() + 
            facet_wrap(data()[,y_var]) + 
            xlab(x_var) +
            ylab(y_var)
        })
    }
    #TWO QUANT ONE CAT
    else if ((length(input$quant_vars) == 2)&&(length(input$cat_vars) == 1)) {
        #print("INSIDE")
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
    }
    #ONE QUANT TWO CAT
    else if ((length(input$quant_vars) == 1)&&(length(input$cat_vars) == 2)) {
      output$plot <- renderPlot({
        x_var <- strsplit(input$cat_vars, ",")[[1]]
        y_var <- strsplit(input$cat_vars, ",")[[2]]
        z_var <- strsplit(input$quant_vars, ",")[[1]]
        
        ggplot(data(), aes(x = as.factor(data()[,x_var]), y = data()[,z_var],)) + 
          geom_boxplot() + 
          facet_wrap(~data()[,y_var]) + 
          #facet_grid(cols = vars(as.character(y_var))) + 
          xlab(x_var) + 
          ylab(z_var)
      })
    }
    
    #THREE CAT
    #else if ((length(input$quant_vars) == 0)&&(length(input$cat_vars) == 3) ) {
    #  x_var <- strsplit(input$cat_vars, ",")[[1]]
    #  y_var <- strsplit(input$cat_vars, ",")[[2]]
    #  z_var <- strsplit(input$cat_vars, ",")[[3]]
    #  output$plot <- renderPlot({
        #data_table <- table(data[, c(x_var, y_var, z_var)])
        #ggplot(data(), aes(x = data()[,x_var], y = y_var, fill = Freq)) + 
        #  geom_tile() + 
        # facet_wrap(~z_var) + 
         # xlab(x_var) + 
          #ylab(y_var) + 
        #  scale_fill_gradient(low = "white", high = "red")
        
     #     ggpairs(data()[, input$cat_vars])
        
        
    #  })
    #}
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
  
  #dummy2
  observeEvent(c(input$variable1, input$variable2, input$isTarget), {
    col1 <- input$variable1
    col2 <- input$variable2 
    target <- data()[, "target"]
    
    if (!is.null(col1) && !is.null(col2)) {
      
      # Determine if only numerical variables were selected
      if ((col1 %in% c("age","trestbps", "chol", "thalac", "oldpeak", "ca")) && (col2 %in% c("age","trestbps", "chol", "thalac", "oldpeak", "ca"))) {
        print("only numerical variables are selected")
        # from Strip plot to swarn plot
        
        # strip plot
        output$plotT <- renderPlot({
          ggplot(data(), aes_string(x = col1, y = col2)) +
            geom_jitter()
        })
        
        # interaction
        if(input$isTarget) {
          
          # swarn plot
          print("swarn plot")
          output$plotT <- renderPlot({
            ggplot(data(), aes_string(x = col1, y = col2, color = factor(target))) +
              geom_jitter(alpha = 0.3)
          })
        }
      }
      
      # Determine if only categorical variables were selected
      else if ((col1 %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "thal")) && (col2 %in% c("sex","cp", "fbs", "restecg", "exang", "slope", "thal"))) {
        print("only categorical variables are selected")
        # from mosaic plot
        
        #mosaic plot
        output$plotT <- renderPlot({
          mycolors <- brewer.pal(8, "Dark2")
          mosaicplot(table(data()[, c(col1, col2)]), main="Mosaic plot", col=mycolors)
          legend("topright",legend=colnames(data()[, c(col1, col2)]),fill=mycolors)
        })
        
        #scatterplot
        
      }
      
      # Determine if both quantitative and categorical variables were selected
      else  {
        print("both numerical and categorical variables are selected")
        # from bar chart to grouped bar chart
        
        #search for the numerical
        if (!col2 %in% c("age","trestbps", "chol", "thalac", "oldpeak", "ca")) {
          app <- col1
          col1 <- col2
          col2 <- app
        }
        output$plotT <- renderPlot({  
          ggplot(data(), aes(x=data()[,col2], fill=data()[,col1])) + 
            geom_histogram() + 
            facet_wrap(data()[,col1]) + 
            xlab(col2) +
            ylab(col1) 
        })
        
        #interaction
        #TODO: set in box: "do you want see the graphical representation in another way?" - for different plots, we have different actions, we have to change dinamicly the botton
        if(input$isTarget) {
          
          #grouped bar chart
          print("grouped bar chart")
          output$plotT <- renderPlot({
            ggplot(data(), aes(x = data()[,col1], y = data()[,col2], fill = factor(target))) +
              geom_bar(stat = "identity", position = position_dodge()) + 
              xlab(col1) +
              ylab(col2) #TODO: add the legend part
          })
        }
      }
    } 
    else{
      print("None")
    }
  })
}

shinyApp(ui, server)