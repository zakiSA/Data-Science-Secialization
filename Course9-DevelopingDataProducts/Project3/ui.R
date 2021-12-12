#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that calculates BMI
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Body Mass Index Predictor"),
    
    # Sidebar with a sldebar input 
    sidebarLayout(
        sidebarPanel(
            h3("App Documentation"),
            h6("This App predicts BMI based on Weight and Gender and reports the user's Health Status."),
            h6("To get a prediction for BMI the correct weight MUST be selected."),
            h6("Main panel displays Health Status (Underweight,Normal Weight, Overweight, Obese) based on predicted BMI"),
            selectInput("gender","Choose Your Gender",choices=c("Female","Male"),
                        selected="Female"),
            h3("Select"),
            h3("Your Weight"),
            sliderInput("sliderWt","Slide to Select Weight in kgs",0,200,value=45),
            h3("Enter"),
            h3("Your Height"),
            em("Enter value to see Height displayed in Health Stats"),
            textInput("height","Enter Your Height in cms (0-200)")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h3("Predict My BMI"),       
            plotOutput("plot1"),
            h3("Your Health Stats"),
            h4("Predicted BMI"),
            textOutput("pred"),
            h4("Status"),
            textOutput("health"),
            h4("You are"),
            textOutput("my_gender"),
            h4("Weight in kgs"),
            textOutput("weight"),
            h4("Height in cms"),
            textOutput("Height")
            
            
        )
    )
))
