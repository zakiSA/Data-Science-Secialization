#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic 
library(shiny)
library(dplyr)
library(tidytext)
library(tidyr)
library(tm)
library(ggplot2)
library(stringr)
library(hunspell)

#Ungrouped Ngram tables for entire corpus(blog,news,twitter) and train and test set combined
#df1-unigram, and singles removed from bigrams-df2, trigrams - df3 and quadgrams -df4
######################
#Full usdocs files without singles,spellchecked and bad words and numbers removed

#N-gram tables from Optimizing.R file
#load("clean_rmSing_ngrams.RData")

#All ngram functions

sentence_kbo <- function(my_sentence,discount) { 
  sentence_rmNum <- removeNumbers(my_sentence)
  #print("HERE")
  sentence_tolow <- tolower(sentence_rmNum)
  sentence_rmpunc <- removePunctuation(sentence_tolow)
  sentence_noWsp <- stripWhitespace(sentence_rmpunc)
  #Count the number of words in the sentence
  #length <- sapply(gregexpr("\\S+", sentence_noWsp),length)
  sentence <- unlist(strsplit(sentence_noWsp," "))
  len_in <- length(sentence)
  
  if (len_in >= 4){
    #print(my_sentence)
    #sentence_kbo_quad(my_sentence,discount)
    choices4 <- sentence_kbo_quad(sentence_noWsp,discount)
    choices4[1:10, ]
    
  }
  else if(len_in == 3) {
    #print(my_sentence)
    #sentence <- strsplit(my_sentence, " ")
    sentence <- strsplit(sentence_noWsp," ")
    my_word1 <- sentence[[1]][1]
    my_word2 <- sentence[[1]][2]
    my_word3 <- sentence[[1]][3]
    choices3 <- kbo_us_quadgram(my_word1,my_word2,my_word3,discount)
    choices3[1:10, ]
  }
  else if(len_in == 2){
    #sentence <- strsplit(my_sentence, " ")
    sentence <- strsplit(sentence_noWsp, " ")
    my_word1 <- sentence[[1]][1]
    my_word2 <- sentence[[1]][2]
    choices2 <- kbo_us_trigram(my_word1,my_word2,discount)
    choices2[1:10, ]
  }
  else if(len_in == 1){
    #print(sentence_noWsp)
    #bigrams <- kbo_us_bigram(my_sentence,discount)
    bigrams <- kbo_us_bigram(sentence_noWsp,discount)
    my_bigrams <- bigrams[1:10, ]
    my_bigrams
  }
}

sentence_kbo_quad <- function(sentence_noWsp,discount) {
  # sentence_rmNum <- removeNumbers(my_sentence)
  # sentence_tolow <- tolower(sentence_rmNum)
  # sentence_rmpunc <- removePunctuation(sentence_tolow)
  # sentence_noWsp <- stripWhitespace(sentence_rmpunc)
  length <- length(sentence_noWsp)
  my_word1 = word(sentence_noWsp,-3)
  my_word2 = word(sentence_noWsp,-2)
  my_word3 = word(sentence_noWsp,-1)
  all_pred <- kbo_us_quadgram(my_word1,my_word2,my_word3,discount)
  preds <- all_pred[1:100, ]
  my_pred <- preds %>% group_by(word1,word2,word3,word4)
  my_pred <- subset(my_pred, !word4 %in% stopwords("en"))
  my_pred <- my_pred %>% arrange(desc(kboQuadgram))
  best_pred <- my_pred[1:10, ] 
  return(best_pred)
} 

kbo_us_quadgram <- function(my_word1,my_word2,my_word3,discount) {
  #get count for seen quadgrams 
  df4_alpha <- subset(df4, word1==my_word1 & word2 == my_word2 & word3 == my_word3) %>% group_by(word1,word2,word3)
  if (nrow(df4_alpha) > 0)  {
    #Get counts for trigrams 
    seen_trigram_count <- df3$ng3count[df3$word1 == my_word1 & df3$word2 == my_word2 & df3$word3 == my_word3]
    #Calcualte the kbo for seen quadgrams
    df4_alpha <- df4_alpha %>% mutate(delta = ng4count - discount)
    df4_alpha <- df4_alpha %>% mutate(kboQuadgram = delta/seen_trigram_count)
    #Calculate the backoff prob of seen quadgrams
    disc_probMass <- sum(df4_alpha$delta)
    #Calculate missing Probability Mass (alpha(wi-2,wi-1))
    us_alpha_word1word2word3 <- 1 - (disc_probMass/seen_trigram_count)
    #Distribute missing probability mass among unseen word3 trigrams
    #Get all possible bigrams starting with my_word2 from the function kbo_us_bigram
    #Use my_word2 as input for function kbo_us_bigram to get all the bigrams(seen/unseen from bigram_tbl)
    all_word3_trigrams <- kbo_us_trigram(my_word2,my_word3,discount)
    #Select only the trigrams that are not seen as word4 in seen quadgrams in df4_alpha
    unseen_word3_trigrams <- subset(all_word3_trigrams,!word3 %in% df4_alpha$word4)
    unseen_trigram_kbo_sum <- sum(unseen_word3_trigrams$kboTrigram)
    #Rearrange table
    colnames(unseen_word3_trigrams)[colnames(unseen_word3_trigrams) == "word3"] <- "word4"
    colnames(unseen_word3_trigrams)[colnames(unseen_word3_trigrams) == "word2"] <- "word3"
    colnames(unseen_word3_trigrams)[colnames(unseen_word3_trigrams) == "word1"] <- "word2"
    unseen_word3_trigrams$word1 <- my_word1
    #Create unseen quadgram table
    unseen_quad <- unseen_word3_trigrams %>% select(word1,word2,word3,word4)
    #Calculate backoff prob for unseen quadgrams
    unseen_quad$kboQuadgram <- us_alpha_word1word2word3 * (unseen_word3_trigrams$kboTrigram/unseen_trigram_kbo_sum)
    quadgram_kbosum <- sum(unseen_quad$kboQuadgram)
    #Create seen quadgram table
    seen_quad <- df4_alpha %>% select(word1,word2,word3,word4,kboQuadgram)
    #Join seen and unseen quadgram tables to get quadgram tbl
    quad_tbl <- bind_rows(seen_quad,unseen_quad)
    #Arrange in desc order of kbo
    quad_tbl <- quad_tbl %>% arrange(desc(kboQuadgram))
  }
  else {
    #When there are no seen quadgrams use my_word2,my_word3 as input for function kbo_us_trigram to get all the trigrams
    all_word3_trigrams <- kbo_us_trigram(my_word2,my_word3,discount)
    unseen_word3_trigrams <- subset(all_word3_trigrams,!word3 %in% df4_alpha$word4)
    unseen_trigram_kbo_sum <- sum(unseen_word3_trigrams$kboTrigram)
    colnames(unseen_word3_trigrams)[colnames(unseen_word3_trigrams) == "word3"] <- "word4"
    colnames(unseen_word3_trigrams)[colnames(unseen_word3_trigrams) == "word2"] <- "word3"
    colnames(unseen_word3_trigrams)[colnames(unseen_word3_trigrams) == "word1"] <- "word2"
    unseen_word3_trigrams$word1 <- my_word1
    unseen_quad <- unseen_word3_trigrams %>% select(word1,word2,word3,word4)
    #There were no seen quadgrams so alpha is 1
    us_alpha_word1word2word3 <- 1
    unseen_quad$kboQuadgram <- unseen_word3_trigrams$kboTrigram/unseen_trigram_kbo_sum
    unseen_quad$kboQuadgram <- us_alpha_word1word2word3 * (unseen_word3_trigrams$kboTrigram/unseen_trigram_kbo_sum)
    quadgram_kbosum <- sum(unseen_quad$kboQuadgram)
    quad_tbl <- unseen_quad %>% arrange(desc(kboQuadgram))
  }
  return(quad_tbl)
}

kbo_us_trigram <- function(my_word1,my_word2,discount) {
  #get seen trigrams 
  df3_alpha <- subset(df3, word1==my_word1 & word2 == my_word2) %>% group_by(word1,word2)
  if (nrow(df3_alpha) > 0) {
    #Get seen bigram counts
    seen_bigram_count <- df2$ng2count[df2$word1 == my_word1 & df2$word2 == my_word2]
    #Calcualte kbo for seen trigrams
    df3_alpha <- df3_alpha %>% mutate(delta = ng3count - discount)
    df3_alpha <- df3_alpha %>% mutate(kboTrigram = delta/seen_bigram_count)
    #Sum of discounts from seen bigrams
    disc_probMass <- sum(df3_alpha$delta)
    #Calculate missing probability Mass alpha
    us_alpha_word1word2 <- 1 - (disc_probMass/seen_bigram_count)
    #Distribute missing probability mass among unseen word3 trigrams
    #Get all possible bigrams starting with my_word2 from the function kbo_us_bigram
    #Use my_word2 as input for function kbo_us_bigram to get all the bigrams(seen/unseen from bigram_tbl)
    all_word2_bigrams <- kbo_us_bigram(my_word2,discount)
    #Select only the bigrams that are not seen as word2 and word3 in seen trigrams in df3_alpha
    unseen_word2_bigrams <- subset(all_word2_bigrams,!word2 %in% df3_alpha$word3)
    #get the sum of all the backoff prob of all unseen bigrams in the above
    unseen_bigram_kbo_sum <- sum(unseen_word2_bigrams$kboBigram)
    #Rearrange tables
    colnames(unseen_word2_bigrams)[colnames(unseen_word2_bigrams) == "word2"] <- "word3"
    colnames(unseen_word2_bigrams)[colnames(unseen_word2_bigrams) == "word1"] <- "word2"
    unseen_word2_bigrams$word1 <- my_word1
    unseen_trigrams <- unseen_word2_bigrams
    #Calcualte backoff prob of unseen trigrams 
    unseen_trigrams$kboTrigram <- us_alpha_word1word2 * (unseen_trigrams$kboBigram/unseen_bigram_kbo_sum)
    #Create unseen trigram table
    unseen_trigrams <- unseen_trigrams %>% select(word1,word2,word3,kboTrigram)
    #Create seen trigram table
    seen_trigrams <- df3_alpha %>% select(word1,word2,word3,kboTrigram)
    #Join the seen and unseen trigram tables
    trigram_tbl <- bind_rows(seen_trigrams,unseen_trigrams)
    #Arrange in desc order of kbo
    trigram_tbl <- trigram_tbl %>% arrange(desc(kboTrigram))
    #sum_all <- sum(trigram_tbl$kboTrigram)
  }
  else {
    all_word2_bigrams <- kbo_us_bigram(my_word2,discount)
    #Select only the bigrams that are not seen as word2 and word3 in seen trigrams in df3_alpha
    unseen_word2_bigrams <- subset(all_word2_bigrams,!word2 %in% df3_alpha$word3)
    #get the sum of all the backoff prob of all unseen bigrams in the above
    unseen_bigram_kbo_sum <- sum(unseen_word2_bigrams$kboBigram)
    #Rearrange tables
    colnames(unseen_word2_bigrams)[colnames(unseen_word2_bigrams) == "word2"] <- "word3"
    colnames(unseen_word2_bigrams)[colnames(unseen_word2_bigrams) == "word1"] <- "word2"
    unseen_word2_bigrams$word1 <- my_word1
    unseen_trigrams <- unseen_word2_bigrams
    #Calcualte backoff prob of unseen trigrams 
    unseen_trigrams$kboTrigram <- unseen_trigrams$kboBigram/unseen_bigram_kbo_sum
    #there are no seen trigrams so alpha is 1
    us_alpha_word1word2 <- 1
    unseen_trigrams$kboTrigram <- us_alpha_word1word2 * (unseen_trigrams$kboBigram/unseen_bigram_kbo_sum)
    #create unseen trigram table
    unseen_trigrams <- unseen_trigrams %>% select(word1,word2,word3,kboTrigram)
    trigram_tbl <- unseen_trigrams %>% arrange(desc(kboTrigram))
  }
  return(trigram_tbl)
}

kbo_us_bigram <- function(my_word1,discount) {
  #get seen bigrams 
  usdf_alpha <- subset(df2, word1==my_word1) %>% group_by(word1)
  if (nrow(usdf_alpha) > 0) {
    #get count of word1 from unigram table
    word1_count <- df1$ng1count[df1$word == my_word1]
    #Calcualte kbo for bigram
    usdf_alpha <- usdf_alpha %>% mutate(delta = ng2count - discount)
    #print(usdf_alpha)
    usdf_alpha <- usdf_alpha %>% mutate(kboBigram = delta/word1_count)
    #get disc probMass of seen bigrams 
    disc_probMass <- sum(usdf_alpha$delta)
    #Calculate missing probability Mass
    us_alpha_word1 <- 1 - (disc_probMass/word1_count)
    #Distribute missing probability mass among unseen word2
    #Unseen bigrams
    unseen_word2_bigrams <- subset(df1,!word %in% usdf_alpha$word2)
    #Get count of all words not seen as word2 in bigrams with word1 of intrest
    word2_unseen_count <- sum(unseen_word2_bigrams$ng1count)
    #Calculate backoff prob of all words above 
    unseen_word2_bigrams$kboBigram <- us_alpha_word1 * (unseen_word2_bigrams$ng1count/word2_unseen_count)
    colnames(unseen_word2_bigrams)[colnames(unseen_word2_bigrams) == "word"] <- "word2"
    #Add word1 column
    unseen_word2_bigrams$word1 <- my_word1
    #make table for all seen bigrams
    seen_tbl <- usdf_alpha %>% select(word1,word2,kboBigram)
    #Make table for all unseen bigrams
    unseen_tbl <- unseen_word2_bigrams %>% select(word1,word2,kboBigram) 
    #Join seen and unseen bigrams in final 
    bigram_tbl <- bind_rows(seen_tbl,unseen_tbl)
    bigram_tbl <- bigram_tbl %>% arrange(desc(kboBigram))
  }    
  else {   
    #Unseen bigrams when there are no seen bigrams
    unseen_word2_bigrams <-subset(df1,!word %in% usdf_alpha$word2)
    #Get count of all words not seen as word2 in bigrams with word1 of intrest
    word2_unseen_count <- sum(unseen_word2_bigrams$ng1count)
    #Calculate backoff prob of all words above 
    #Here alpha is 1 since there are no seen bigrams at all for my_word1
    us_alpha_word1 <- 1
    unseen_word2_bigrams$kboBigram <- us_alpha_word1 * (unseen_word2_bigrams$ng1count/word2_unseen_count)
    colnames(unseen_word2_bigrams)[colnames(unseen_word2_bigrams) == "word"] <- "word2"
    #Add word1 column
    unseen_word2_bigrams$word1 <- my_word1
    #Make table for all seen bigrams
    unseen_tbl <- unseen_word2_bigrams %>% select(word1,word2,kboBigram) 
    #Create bigram table for unseen bigrams
    bigram_tbl <- unseen_tbl %>% arrange(desc(kboBigram))
    
  } 
  
  return(bigram_tbl)
}

# Define server logic 
shinyServer(function(input, output){

  prediction <- eventReactive (input$go, {
    req(input$my_sentence)
    my_sentence <- input$my_sentence
    discount <- isolate(as.numeric(input$discount)) #testing
    my_tbl <- sentence_kbo(my_sentence,discount)
  })
  
  output$my_choices <- renderTable({
    #my_sentence <- input$my_sentence #testing
    #discount <- as.numeric(input$discount) #testing
    count <- isolate(input$num_choices) #testing
    my_tbl <- prediction()
    
    colnum <- length(my_tbl)
    #my_tbl <- my_tbl[1:count ,1:(colnum -1)]
    colnames(my_tbl)[colnum]<-"Probability"
    my_tbl <- my_tbl[1:count , ]
    })
  
  output$plot <- renderPlot({
    #my_sentence <- input$my_sentence #testing
    #discount <- as.numeric(input$discount)#testing
    count <- isolate(input$num_choices) #testing
    #result <- sentence_kbo(input$my_sentence,discount)
    result <- prediction()
    
    len <- length(result)
    if (len == 3) {
      result <- result %>% ungroup() 
      result <- result[1:count, ]
      result %>% mutate(word2 = reorder(word2,kboBigram))%>% ggplot(aes(word2,kboBigram)) + geom_col(fill="lightgreen") +  theme(text = element_text(size=20)) + ggtitle("Words") + xlab(NULL) + ylab("Probability") + coord_flip()  
    }
    else if (len == 4) {
      result <- result %>% ungroup() 
      result <- result[1:count, ]
      result %>% mutate(word3 = reorder(word3,kboTrigram)) %>% ggplot(aes(word3,kboTrigram)) + geom_col(fill="lightblue2") +  theme(text = element_text(size=20)) + ggtitle("Words") + xlab(NULL) + ylab("Probability") + coord_flip()
    }
    else if (len == 5) {
      result <- result %>% ungroup()
      result <- result[1:count, ]
      result %>% mutate(word4 = reorder(word4,kboQuadgram)) %>% ggplot(aes(word4,kboQuadgram)) + geom_col(fill="lightyellow") +  theme(text = element_text(size=20)) + ggtitle("Words") + xlab(NULL) + ylab("Probability") + coord_flip() 
    }
    })
  
  output$first_choice <- renderText({
    tmp_sen1 <- prediction()
    
    len1 <- length(tmp_sen1)
    my_choice1 <- unlist(tmp_sen1[1,(len1 - 1)])
    choice_one <- isolate(paste(input$my_sentence,my_choice1)) #testing
  })
  
  output$all_choices <- renderText({
    choices <- prediction()
  
    len4 <- length(choices)
    count <- input$num_choices
    show_choices <- unlist(choices[1:count,(len4-1)])
  })
  output$one_choice <- renderText({
    tmp_sen <- prediction()
    
    len2 <- length(tmp_sen)
    sen1 <- unlist(tmp_sen[1,(len2 - 1)])
    
  })
  
  output$second_choice <- renderText({
    tmp_sen2 <- prediction()
    
    len2 <- length(tmp_sen2)
    sen2 <- unlist(tmp_sen2[2,(len2 - 1)])
    
  })
  output$third_choice <- renderText({
    tmp_sen3 <- prediction()
    
    len3 <- length(tmp_sen3)
    sen3 <- unlist(tmp_sen3[3,(len3 - 1)])
    
  })

  
}) 



