                                                                                                                                                                             #
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(
    wellPanel(style = "background:lightblue",
    h2("Next Word Predict")
   )  
  ),
  fluidRow(
      tabsetPanel(
        tabPanel(
          "My App", column(6,textInput(inputId = "my_sentence",label = "Enter words or phrase:",value ="I love"),actionButton(inputId ="go",label = "Click to Predict"),
                           column(6,offset = 0,h3("Top Prediction: "),
                                  tags$head(tags$style("#first_choice{color: black;
                                                       font-size: 25px;
                                                       font-family:Tahoma;
                                                       }"
                         )
                                  ),
                         
                         textOutput("first_choice"))
                         ), 
                  column(3,radioButtons(inputId = "discount",label = "Choose Discount", choices = c("0.5","0.7"))),
                  column(3,numericInput(inputId = "num_choices",label = "Number of predictions",value = 3, min = 3,max = 10)),

      fluidRow(
            wellPanel(style = "background:lightblue",#testing
                fluidRow(
                    column(6,h4("Top 3 Predictions:"),
                        column(2,tags$head(tags$style("#one_choice{color: black;
                                                     font-size: 20px;
                                                     font-family:Tahoma;
                                                     }"
                         )
                                ),
                         textOutput("one_choice")),
                        column(2,tags$head(tags$style("#second_choice{color: black;
                                                font-size: 20px;
                                               font-family:Tahoma;
                                               }"
                         )
                          ),
                          textOutput("second_choice")),
                        column(2,tags$head(tags$style("#third_choice{color: black;
                                                font-size: 20px;
                                               font-family:Tahoma;
                                               }"
                         )
                          ),
                          textOutput("third_choice"))
                      )
                  )
                )
   ), 
   fluidRow(
     column(4,offset = 1,
            h3("Plot of predictions"),     
            plotOutput("plot")), 
     column(6, offset = 1,
            h3("N-gram Table"),     
            tableOutput("my_choices")) 
     
    )
  ),  
      
  tabPanel("Instructions",
      fluidPage(
        wellPanel(style = "background:lightblue",
           h3(style = "font-family:Trebuchet MS Bold","Getting Started"),
           h4("User Inputs:"),
           "Step 1: Enter some", tags$strong("words"),"or",tags$strong("phrase"),"in the text input box. If no text is entered nothing will be done.",
           tags$br(),
           "Step 2: Choose", tags$strong("discount: 0.5 or 0.7"),". The default value is 0.5. This discount is applied at all N-gram levels.",
           tags$br(),
           "Step 3: Select the", tags$strong("number of predictions"), "to display for the",tags$strong("next word"),". 
           Default value is a minimum of 3 choices, and the maximum is 10 choices.",
           tags$br(),
           "Step 4: Now", tags$strong("click"), "the",tags$strong('"Click to Predict"'), "button to get predictions for the", 
            tags$strong("next word"),". The user", tags$strong("must click"), "this button after making any 
            changes to update the predictions.",
           
           h4("Predictions:"),
           tags$p(style = "font-family:Trebuchet MS Bold",
                  "When the user enters text input and clicks the"
                  ,tags$strong('"Click to Predict"'), "button, the App displays:", 
                  tags$br(),
                  "1) The", tags$strong("Top Prediction"),", the phrase or words entered by the user followed by the prediction for the", tags$strong("next word"), "with 
                  that the highest probability.",
                  tags$br(),
                  "2) The", tags$strong("Top  3 Predictions"), ", the top three choices for",tags$strong("next word"), "that have the highest probability.",
                  tags$br(),
                  "3) A ",tags$strong("Bar plot"),"displaying the number of" ,tags$strong("next word"), "choices the user selected.",
                  tags$br(),
                  "4) A", tags$strong("table"),"showing the N-grams and the Katz-backoff probability for each" ,tags$strong("next word"),"prediction."
                  ),
           tags$br(),
           tags$p(style = "font-family:Trebuchet MS Bold",
            h4("Definitions: "),
           tags$strong("Corpus:"),"A corpus is a collection of written texts, like entire works of a particular author or a body of writing on a particular subject.",
           " Here the corpus consists of text collected from publicly available sources like personal blogs, newspapers and twitter.",
           tags$br(),
           tags$strong("N-grams: "), "In the fields of computational linguistics, an n-gram is a contiguous sequence of n items from a given sample of text or speech. Here the items are words,",
           "The n-grams typically are collected from a text or corpus. An n-gram of size 1 (for example:'I') is referred to as a 'unigram'; size 2, (for example: 'I love') is a 'bigram' ",
           "; size 3, (for example:'I love to') is a 'trigram'; size 4, (for example:'I love to go') is a 'four-gram' or 'quadgram'. We will refer to it as a 'quadgram' for this project.",
           tags$br(),
           tags$strong("Katz's back-off language model: "), "As per wikipedia, it is a generative n-gram language model that estimates the conditional probability of a word given its history in the n-gram. 
            It accomplishes this estimation by backing off through progressively shorter history models under certain conditions. 
            By doing so, the model with the most reliable information about a given history is used to provide the better results."
           ),
           tags$p(style = "font-family:Trebuchet MS Bold",
                  h3(style = "font-family:Trebuchet MS Bold","How it Works: "),
                  #tags$h4("Katz's back-off Model"),
                  "This App uses the Katz's back-off language Model which uses a discounting method to make predictions for the", tags$strong("next word"),"using N-grams.", 
                  "The entire text from the en_US blogs, news and twitter from the en_US locale from the corpus HC Corpora", 
                  tags$a("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"), "was used to train the model for this project.",
                  tags$br(),
                  tags$strong("Building the Next Word Prediction Model:"),
                  tags$br(),
                  tags$strong("Step1) Preprocessing texts:"), "The text was preprocessed and cleaned, and N-gram tables with counts for observed N-grams were then generated using the tidytext package.",
                  "The bigrams, trigrams and quadgrams that occured one time (had a count of 1) were dropped in order to reduce the memory used by the App.",
                  tags$br(),
                  tags$strong("Step 2)The Model:"), "This model is a recursive model which uses bigram, trigram and quadgram models to handle diffrent lengths of inputs efficiently. The bigram model takes a single word and discount as input and generates all seen and unseen bigrams,",
                  "the trigram model takes two words and a discount as input and generates all seen and unseen trigrams, and the quadgram model that takes three words and discount and generates all the seen and unseen quadgrams.",
                  tags$br(),
                  tags$strong("Step 3) helper functions to handle input:"), "Functions that handle the input from text box, convert the input words or phrase to lowercase, strip off punctuation and white space and split it into individual words. 
                  Based on the number of words the function chooses one of the three N-gram models to get next word predictions.",
                  "If the user enters 1 word the bigram model is used, if the user enters 2 words the trigram model is used to make the prediction ,",
                  "if a user enters 3 or more words the quadgram model is used."),
                  "For details about each model click the Back-off Model tab."
                  
                  
       )         
      )
  ),#instruction tab panel
  tabPanel("Back-off Model",
      fluidPage(
        wellPanel(style = "background:lightblue",
           tags$p(style = "font-family:Trebuchet MS Bold",
            tags$h4("Katz back-off Model for N-grams (N=2,3,4):"),
            "Given a discount value, and input words or a phrase, the Katz N-gram model looks at the last N-1 words of the input, to compute the Katz back-off probabilities for both seen and unseen N-grams.",
            "For inputs of three words or more, we only use a quadgram model (N=4), for inputs of exactly 2 words the trigram (N=3) model is used, and for single word input the bigram model (N=2) is used.",
            tags$br(),tags$br(),
            tags$strong("Seen N-grams:"),"N-grams that are observed in our corpus and therefore have a count > 0. These N-grams are generated from the corpus using the tidytext package and stored in the respective seen N-gram table along with their counts.",
            tags$br(),tags$br(),
            tags$strong("Discounting: "),"Take counts of seen N-grams for 'N_minus_1_gram = (word 1, word 2, ..., word N-1)' from the seen N-gram table, and subtract the selected discount to get the discounted count for all the seen N-grams for the given set of N-1 words, and call it delta_seen_Ngram", 
            "(delta_seen_Ngram = count_seen_Ngram  –  discount)",
            tags$br(),tags$br(),
            tags$strong("Katz back-off probability for Seen N-grams"),
           "Katz back-off probability =  delta_seen_Ngram/N_minus_1_gram_count",
            tags$br(),tags$br(),
            tags$strong("Discounted Probability Mass"),
           "Sum up delta for all seen N-grams to compute the discounted probability mass, given as 
           disc_prob_mass = sum(delta_seen_Ngram)",
           tags$br(),tags$br(),
           tags$strong("Missing probability Mass, alpha_seen_Ngram:"),"We compute disc_prob_mass/N_minus_1_gram_count and subtract it from 1 to get the missing probability mass and call it alpha_seen_Ngrams.",
           tags$br(),
           "alpha_seen_Ngrams = 1 – (disc_prob_mass/N_minus_1_gram_count)",
           tags$br(),
           "alpha_seen_Ngrams is then distributed to the unseen N-grams.",
           tags$br(),tags$br(),
           tags$strong("Unseen N-grams:"),"N-grams that are not observed in the corpus and hence have count = 0.  
           To get the unseen N-grams we back-off to the (N-1)gram level and get all the words and their counts from the (N-1)gram table for word_N that were not seen in the seen N-gram table for N_minus_1_gram=(word 1, word 2, ..., word N-1). 
           These unseen candidates for word_N will constitute the unseen Ngram tails for the given N_minus_1_gram.",
           tags$br(),tags$br(),
           tags$strong("Katz back-off probability for Unseen N-grams: "),
           tags$br(),
           " 1) We take the counts of unseen word_N from above and deduct the selected discount from it.",
           tags$br(),
           " 2) We sum up the discounted counts for all unseen word_Ns.",
           tags$br(),
           " 3) We take the count of the word_N that was not seen to follow N_minus_1_gram",
           tags$br(),
           " 4) Then we compute (count of unseen word_N /sum of discounted counts of all unseen word_Ns).",
           tags$br(),
           " 5) To compute the Katz back-off probability for the unseen N-grams, we then multiply the above value with alpha_seen_Ngram.
             This model computes probabilities for all the seen and unseen Ngrams on the fly."
          )#tags$p
        )#well panel
      )#fluidpage       
   )#model tab panel
  )#tabset panel
 )
))  

  
 
   

  




    

