#========================================================================#

## Project	: A Shiny Text Predictive Product

## Author	: Aliakbar Safilian

## Last Updated : 2019-06-06

## What         : Given a sequence of words, this function returns 5 words
#                 as the top-5 words that might be the next word. 

#========================================================================#


#==========================Loading Packages==========================#
library(shiny)
require(quanteda)
require(data.table)
require(stringr)
require(markdown)



#==========================Loading Data==========================#


unigrams <- readRDS("unigrams.rsd")
setkey(unigrams, word)

bigrams <- readRDS("bigrams.rsd")
setkey(bigrams, word1, word2)

trigrams <- readRDS("trigrams.rsd")
setkey(trigrams, word1, word2, word3)

quadgrams <- readRDS("quadgrams.rsd")
setkey(quadgrams, word1, word2, word3, word4)

profanities <- readRDS("profanities.rds")




#==========================N-Gram Model==========================#

getWords_4grams <- function(str, n = 5){
        if(str == ""){ return(NULL) }
        toks <-  tokens(x = char_tolower(str),
                        remove_hyphens = TRUE,
                        remove_url = TRUE,
                        remove_symbols = TRUE,
                        remove_separators = TRUE,
                        remove_punct = TRUE,
                        remove_twitter = TRUE,
                        remove_numbers = TRUE)
        
        toks <- tokens_remove(toks, pattern = profanities)
        
        toks <- tokens_remove(toks,
                              pattern = "[A-z]*[^\x01-\x7F]+[A-z]*", 
                              valuetype = "regex")
        
        
        toks <- tokens_remove(toks, 
                              pattern = "\\w*[0-9]+\\w*\\s*", 
                              valuetype = "regex")
        
        toks <- rev(rev(unlist(toks))[1:3])
        
        words <- predict_quad(toks[1], toks[2], toks[3],  n)
        data.frame(Words = unique(words))
}



getWords_3grams <- function(str, n = 5){
        if(str == ""){ return(NULL) }
        toks <-  tokens(x = char_tolower(str),
                        remove_hyphens = TRUE,
                        remove_url = TRUE,
                        remove_symbols = TRUE,
                        remove_separators = TRUE,
                        remove_punct = TRUE,
                        remove_twitter = TRUE,
                        remove_numbers = TRUE)
        
        toks <- tokens_remove(toks, pattern = profanities)
        
        toks <- tokens_remove(toks,
                              pattern = "[A-z]*[^\x01-\x7F]+[A-z]*", 
                              valuetype = "regex")
        
        
        toks <- tokens_remove(toks, 
                              pattern = "\\w*[0-9]+\\w*\\s*", 
                              valuetype = "regex")
        
        toks <- rev(rev(unlist(toks))[1:2])
        
        words <- predict_tri(toks[1], toks[2], n)
        data.frame(Words = unique(words))
}




# Quint-Gram Model
predict_quint <- function(w1, w2, w3, w4, n = 5){
        # extract the most likely word after w1w2w3w4 from the quintgram model
        sel_quint <- quintgrams[.(w1, w2, w3, w4)][order(-Prob)][1:n]
        # left_num: there may not be enough terms in quintgrams
        left_num <- sum(is.na(sel_quint$word5))
        
        
        # The case in which all n words are got from quintgrams
        if(left_num == 0) 
                return(sel_quint$word5)
        
        # The case where nothing is found in the quintgram model
        if(left_num == n)
                return(predict_quad(w2, w3, w4, n))
        
        
        # The case where some (not all) suggestions has been made from quintgrams
        return(c(
                sel_quint$word5[1:(n - left_num)], 
                predict_quad(w2, w3, w4, left_num)
        ))    
        
}

# Trying the Quad-Gram Model
predict_quad <- function(w1, w2, w3, n = 5){
        # extract the most likely word after w1w2w3 from the quadgram model
        sel_quad <- quadgrams[.(w1, w2, w3)][order(-Prob)][1:n]
        # left_num: there may not be enough terms in quadgrams
        left_num <- sum(is.na(sel_quad$word4))
        
        
        # The case in which all n words are got from quadgrams
        if(left_num == 0) 
                return(sel_quad$word4)
        
        # The case where nothing is found in the quadgram model
        if(left_num == n)
                return(predict_tri(w2, w3, n))
        
        
        # The case where some (not all) suggestions has been made from quadgrams
        return(c(
                sel_quad$word4[1:(n - left_num)], 
                predict_tri(w2, w3, left_num)
        ))    
        
}





# Trying the Tri-Gram Model
predict_tri <- function(w1, w2, n = 5){
        # extract the most likely word after w1w2 from the trigram model
        sel_tri <- trigrams[.(w1, w2)][order(-Prob)][1:n]
        # left_num: there may not be enough terms in trigrams
        left_num <- sum(is.na(sel_tri$word3))
        
        
        # The case in which all n words are got from trigrams
        if(left_num == 0) 
                return(sel_tri$word3)
        
        # The case where nothing is found in the trigram model
        if(left_num == n)
                return(predict_bi(w2, n))
        
        
        # The case where some (not all) suggestions has been made from trigrams
        return(c(
                sel_tri$word3[1:(n - left_num)], 
                predict_bi(w2, left_num)
        ))
        
        
}




# Trying the Bi-Gram Model
predict_bi <- function(w1, n = 5){
        # extract the most likely word after w1w2 from the bigram model
        sel_bi <- bigrams[w1][order(-Prob)][1:n]
        # left_num: there may not be enough terms in bigrams
        left_num <- sum(is.na(sel_bi$word2))
        
        
        # The case in which all n words are got from bigrams
        if(left_num == 0) 
                return(sel_bi$word2)
        
        # The case where nothing is found in the bigram model
        if(left_num == n)
                return(predict_uni(n))
        
        
        # The case where some (not all) suggestions has been made from bigrams
        return(c(
                sel_bi$word2[1:(n - left_num)], 
                predict_uni(left_num)
        ))
}


# Trying the Uni-Gram Model
predict_uni <- function(n = 5) {  
        return(sample(unigrams$word, size = n))
}



#==========================User Interface==========================#



# Define UI for application 
ui <- fluidPage(tabsetPanel(
            
        tabPanel("App",
                 
                 titlePanel("Predictive Text App"),
                 
                 sidebarLayout(
                         sidebarPanel(
                                 radioButtons("grams", "N-Gram Model:",
                                              c("Tri-Grams" = "tri",
                                                "Quad-Grams" = "quad"
                                                #,
                                                #"Quint-Grams" = "quint"
                                                ),
                                              selected = "quad"), 
                                 hr(),
                                 radioButtons("nums", "Number of Suggestions:",
                                              c("One" = "1",
                                                "Two" = "2",
                                                "Three" = "3",
                                                "Four" = "4",
                                                "Five" = "5"),
                                              selected = "5")
                                 
                         ), # End of SliderbarPaner
                         mainPanel(
                                 textInput("txt",
                                           label = h4("Type your input phrase below:"),
                                           placeholder="Enter text...",
                                           width = "100%"),
                                 htmlOutput("textOut"), 
                                 actionButton("clear",label="Clear"),
                                 actionButton("auto",label="Auto Next"),
                                 hr(),
                                 h4("Top Suggestions:"),
                                 tableOutput("options")
                         ) # End of mainPanel
                 ) # End of SliderLayout
                 
                         
                 
                 ), # End of tabPaner: App
        
        
        tabPanel("Help",
                 
                 hr(),
                 
                 includeMarkdown("help.md")
                 
        ),
        
        tabPanel("About",
                 
                 hr(),
                 
                 includeMarkdown("about.md")
                 
        )
        
)
)


#==========================Server==========================#


# Define server logic 
server <- function(input, output, session) {
        session$onSessionEnded(stopApp)
        observe({
                grm <- input$grams
                n <- as.numeric(input$nums)
                if(grm == "tri"){
                        sug <- getWords_3grams(input$txt, n)
                }
                else{
                        sug <- getWords_4grams(input$txt, n)
                }
                #else{
                 #       sug <- getWords_5grams(input$txt, n)
                #}
                
                output$textOut <- renderText({HTML(
                        paste0("<div style='background-color:#E8E8E8'>",
                               str_squish(input$txt),
                               " ", "<mark>",
                               sug$Words[1],"</mark></body>"
                               )
                        )
                })
                output$options <- renderTable({ sug }) 
        })
        
        observeEvent(
                
                # Clear button 
                input$clear, { 
                        updateTextInput(session, "txt", value = "")
                }
                
                
                
       )
        
        observeEvent(
                
                # Auto button
                
                input$auto, { 
                        grm <- input$grams 
                        n <- as.numeric(input$nums)
                        if(grm == "tri"){
                                sug <- getWords_3grams(input$txt, n)
                        }
                        else if(grm == "quad"){
                                sug <- getWords_4grams(input$txt, n)
                        }
                        else{
                                sug <- getWords_5grams(input$txt, n)
                        }
                        
                        updateTextInput(session, "txt", 
                                        value = paste(str_squish(input$txt),
                                                      sug$Words[1])
                        )
                }
        )
        
}

# Run the application 
shinyApp(ui = ui, server = server)

