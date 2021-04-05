##NEW APP 

library(shiny)
library(dplyr)
library(tidyr)
library(data.table)

dataset<-mtcars

str(dataset)## DATAFRAME





ui <- fluidPage(
  
  # App title ----
  titlePanel("MtCars Analysis Application- Exploratory Approach!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      uiOutput(outputId = "Column_selected"),
      uiOutput(outputId = "Column_selected_2")
      
    ),
    ###
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel(p(strong("Distribution Chart")),
                           plotOutput(outputId = "distPlot")),
                  tabPanel(p(strong("Subsetted Table")),
                           dataTableOutput(outputId = "Subsetted_Table")),
                  tabPanel(p(strong("Source Table")),
                           dataTableOutput(outputId = "Source_Table")),
                  tabPanel(p(strong("Regression Analysis")),
                           dataTableOutput(outputId = "Regression_analysis")))
      
    )
    
    
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$Column_selected<-renderUI({
    
    column_list=names(dataset)
    
    selectInput(inputId = 'Column_selected', label = "Select Column for data distribution [Tab #1]", choices = column_list, selected = column_list[1], multiple = FALSE)
    
  })
  
  output$Column_selected_2<-renderUI({
    
    column_list=names(dataset)
    
    selectInput(inputId = 'Column_selected_2', label = "Select Column for Summary Stats [Tab #2] ", choices = column_list, selected = column_list[1], multiple = TRUE)
    
  })
  output$distPlot <- renderPlot({
    
    dataset<-dataset
    x<-dataset[, names(dataset)[names(dataset)%in%input$Column_selected]]
    
    main= paste("Histogram describing the" , input$Column_selected, "of MTCARS dataset")
    xlab= input$Column_selected
    
    hist(x, breaks = input$bins, col = "#75AADB", border = "white",
         xlab = xlab,
         main = main)
    
  })
  
  output$Subsetted_Table<-renderDataTable({
    dataset<-dataset
    dataset_Rendered<-dataset[, names(dataset)[names(dataset)%in%input$Column_selected_2]]
    dataset_Rendered<-data.frame(dataset_Rendered)
    names(dataset_Rendered)<-input$Column_selected_2
    dataset_Rendered<-summary(dataset_Rendered)
    return(dataset_Rendered)
  })
 output$Source_Table<-renderDataTable({
   dataset<-dataset
   return(dataset)
   
 })
 output$Regression_analysis<-renderDataTable(
   {
     
     linmod<-lm(mpg~., data = dataset)
     class(summary(linmod))
     
     #linmod$coefficients
     #linmod$residuals
     #linmod$effects
     
     output_df<-data.frame("coefficients"=linmod$coefficients)
     output_df$features=row.names(output_df)
     return(output_df) 
 
  })
   
}

shinyApp(ui, server)
