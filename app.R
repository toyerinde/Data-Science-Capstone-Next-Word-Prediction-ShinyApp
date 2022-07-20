

library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
  setBackgroundColor(
    color=c("lightblue","white"),
    gradient = "linear",
    direction= "bottom"
  ),
  navlistPanel("App Menu",
    tabPanel("Documentation",
             p("This documentation provides guidance on how to use",
             tags$strong("the Next Word Prediction Shiny App.")),
             p("This App predicts the next word in a user input phrase. The App's output is a predicted word with a table of a list of 5 most likely words that could be the next word in the input phrase. Here are the steps required to successfully run the App:"),
             br(),
             p("

1. Select the",
             tags$strong("Next Word Prediction App "),
             "tab"),
p("
2. Input the phrase requiring next word prediction in the appropraite input text box"),  

p("
3. Click the",
             tags$strong("Predict next word"),
             "button to generate the predicted word,table of possible words and App's statistics"),
p("
4. Once done,exit the app via the",
             tags$strong("Exit App"),
             "button"),

p("A table of 5 possible words ranked from the most probable to the least probable are provided in Step 3 with a summary on
the App's accuracy and uncertainty in the words predicted. The App's accuracy is based on having one of the 5 predictions correct.",
"The",
tags$strong("Caption"),
"section of the App is an optional input that changes the title of the prediction table generated.")
    ),
     
     tabPanel("Next Word Prediction App",
                    fluidRow(
                      column(10,offset = 2,
                      titlePanel("User Inputs"),
                      textInput(inputId= "caption",
                              label="Caption:(Optional Input)",
                              value= " Predicted Next Word List"))),
                    
                    fluidRow(
                      column(10,offset = 2,
                      textInput(inputId= "phrase",
                              label="Enter a phrase:",
                              value= ""))),
                    fluidRow(
                      column(10,offset = 2,
                             actionButton("Run", "Predict next word"))),
                    
                
            hr()  ,
  
        # Display List of Predicted Words
        fluidRow(
          column(10,offset = 2,
         h3(textOutput("prediction")))),
        
        fluidRow(
          column(10, offset =2,
         h4(textOutput("caption", container = span)),
         tableOutput("pred"),
         textOutput("accu"),
         textOutput("uncert"))),
        br(),
        
        fluidRow(
          column(10,offset = 2,
                 actionButton("Exit","Exit App")))
          
        ),

)

    )
    








# Define server logic required to predict the next words list
library(sbo)
library(dplyr)

#setwd("/Users/toyerinde/Desktop/DataScience")


modnew_pred<-readRDS(file="model.rds")
prednew<-sbo_predictor(modnew_pred)

server <- function(input, output) {
  observeEvent(input$Run,{
    input_phrase<-input$phrase
    Prediction<-predict(prednew,input_phrase)
    Pred_List<-as.data.frame(Prediction)
    Rank<-c("1","2","3","4","5")
    Pred_df<-cbind(Rank,Pred_List)
    output$prediction<-renderText({
      paste("Predicted Word:",Pred_df[1,2],sep = " ")
    })
    output$caption <- renderText({
      input$caption
    })
    output$pred<-renderTable({
      Pred_df
    })
    output$accu<-renderText({
      paste("App Prediction Accuracy = 0.36")
    })  
    
    output$uncert<-renderText({
      paste("App Prediction Uncertainty = 0.00679")
    })
    })
  # Exit App 
  observeEvent(input$Exit,{
    stopApp()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
