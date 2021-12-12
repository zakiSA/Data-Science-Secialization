#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input,output) {
    
    output$Height <- renderText(input$height)
    
    #Read dataset using read.csv
    bmi_data <- read.csv("./500_Person_Gender_Height_Weight_Index.csv")
    library(dplyr)
    #Convert bmi_data to tibble
    bmi_tbl <- as_tibble(bmi_data)
    my_tbl <- mutate(bmi_tbl,BMI=Weight/Height^2 * 10000)
    #Group by Gender
    fem_group <- group_by(my_tbl,Gender) %>% filter(Gender=="Female")
    m_group <- group_by(my_tbl,Gender) %>% filter(Gender=="Male")
    #Convert Weight into numeric
    my_tbl$Weight <- as.numeric(my_tbl$Weight)
    my_tbl$Height <- as.numeric(my_tbl$Height)
    
    model1 <- lm(BMI~Weight,data=fem_group)
    model2 <- lm(BMI~Weight,data=m_group)
    
    model1pred <- reactive ({
        wtInput <- input$sliderWt
        predict(model1,newdata = data.frame(Weight=wtInput))
    })
    
    model2pred <- reactive ({
        wtInput <- input$sliderWt
        predict(model2,newdata = data.frame(Weight=wtInput))    
    })
    
    output$plot1 <- renderPlot ({
        wtInput <- input$sliderWt
        #mod_points <- switch(input$gender,"Female"=model1pred(),"Male"=model2pred(),col=c("red","blue"))
        #title <- switch(input$title,"Female"="BMI for Females","Male"="BMI for Males")
        plot(my_tbl$Weight,my_tbl$BMI,xlab = "Weight in kgs", ylab = "BMI", bty = "n", col="grey",pch = 16,xlim = c(50, 200), ylim = c(0, 80))
        if(input$gender=="Female") {
            points(wtInput,model1pred(),col="red",pch=16,cex=2,lwd=2)
            abline(model1,col="red",lwd=3)
        }
        else if(input$gender=="Male") {
            points(wtInput,model2pred() ,col="blue",pch=16,cex=2,lwd=2)
            abline(model2,col="blue",lwd=3)
        }    
        
    })   
    output$pred <- renderText({
        if(input$gender=="Female"){
            model1pred()
            
        }
        else if (input$gender=="Male") {
            model2pred()
            
        }
    })
    output$health <- renderText({
        if (model1pred()<=18.5|model2pred()<=18.5){
            "Underweight"
        }
        else if(model1pred()>18.5 & model1pred()<25){
            "Normal Weight"
        }
        else if(model1pred()>25 & model1pred()<30){
            "Overweight"
        }
        else if(model1pred()>=30){
            "Obese"
        }
        else if (model2pred()>18.5 & model2pred()<25){
            "Normal Weight" 
        }    
        else if (model2pred()>25 & model2pred()<30){
            "Overweight" 
        }    
        else if(model2pred()>=30){
            "Obese"
        }
    })
    output$my_gender <- renderText ({
        if (input$gender=="Female"){
            "Female"
        }
        else if(input$gender=="Male"){
            "Male"
        }
    })
    output$weight <- renderText({
        input$sliderWt
    })
})  