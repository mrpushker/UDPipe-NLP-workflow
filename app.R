##                            UDPipe NLP workflow
##                          TABA Assignment Question 1
##
##      Pushker(11810006) , Kushal Bhalla (11810022) , Sai Hemanth Prakhya(11810108)
##
library("shiny")
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
library(readtext)
library(plyr)

# UI starts

ui <-shinyUI(
  fluidPage(
    
    titlePanel("Co-Occurence"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        fileInput("file", "Upload data (Text File) for analysis"),
        
        fileInput("udpipemodel","Upload udipipe Model"),
        
        checkboxGroupInput('checkGroup', h3("Choose the Parts of Speech"),
                           choices = list("Adjective"        = "JJ", 
                                          "Noun"             = "NN", 
                                          "Proper Noun(NNP)" = "NNP",
                                          "Adverb(RB)"       = "RB",
                                          "Verb(VB)"         = "VB"),
                           selected = c("JJ","NN","NNP")
                           ) # check box ends
        ),# sidebarpanel ends
      
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview",
                             
                             h3('How to use this App'),
                             p('To use this app, click on', 
                               span(strong("Upload data (Text file)")),
                               'and upload the txt data file.',br(), 'Then click on  ', 
                               span(strong('Upload udipipe Model')),'and upload a trained udpipe model of a language.',
                               br(), 
                               'You can also choose different parts of speech for co-occurrences plot and word cloud map '),
                             
                             h3(p("Data input")),
                             p("This app supports Text files (.txt) with UTF-8 encoding ",align="justify"),
                             br(),
                             h3(p("Sample Data Sources")),
                             h4(p(a(href="https://github.com/mrpushker/UDPipe-NLP-workflow/blob/master/English.txt",
                                    "You can see the Story of Aryabhatta"))),
                             br(),
                             h4(p(a(href="https://github.com/mrpushker/UDPipe-NLP-workflow/blob/master/Hindi.txt",
                                    "And also  the History of Nalanda in Hindi"))),
                             br(),
                             h4(p(a(href="https://github.com/mrpushker/UDPipe-NLP-workflow/blob/master/Spanish.txt",
                                    "Too much history!! checkout the Children stories in Spanish")))
                             ),
                    tabPanel("CO-occurrences Plot", 
                             plotOutput('plot1')),
                    
                    tabPanel("Word Cloud",
                             plotOutput('plot2')),
                    
                    tabPanel("Data From the File",
                             textOutput('File_Data'))
                    
                    
                    
                    
        ) # tabsetPanel ends
      )# main panel ends
    ) # sidebarLayout ends
  )  # fluidPage ends
) # UI ends

# Server function

options(shiny.maxRequestSize=30*1024^2)

server <- shinyServer(function(input, output) {

# Data from uploaded file  
  Dataset <- reactive({
    if (is.null(input$file)) { return("Upload a file") }
    else{
      
      Data <- readLines(input$file$datapath,encoding = "UTF-8")
      Data  =  str_replace_all(Data, "<.*?>", "")
      return(Data)
    }
  })
  
# Ouput from the udpipe model
  model_df <- reactive({
    txt <- as.character(Dataset())
    model <- udpipe_load_model(file=input$udpipemodel$datapath)
    x <- udpipe_annotate(model, x = txt)
    x <- as.data.frame(x)
    return(x)
    
  })
  
### Co- Occurence Plot ###  
  output$plot1 = renderPlot({ 
    
    x <- model_df()

# try Co occurence with XPOS    
    cooc <- try(
      { 
      cooccurrence(     
      x = subset(x, x$xpos %in% input$checkGroup), 
      term = "lemma", 
      group = c("doc_id", "paragraph_id", "sentence_id")
                  )
        })

# if XPOS fails use UPOS    
      if (inherits(cooc,"try-error"))
        {
        POS_list <- mapvalues(input$checkGroup , from = c("JJ","NN","NNP","RB","VB"), to = c("ADJ","NOUN","PROPN","ADV","VB"))
        cooc <- cooccurrence(     
          x = subset(x, x$upos %in% POS_list), 
          term = "lemma", 
          group = c("doc_id", "paragraph_id", "sentence_id")
                            )
              
        }

# generate the word network        
    wordnetwork <- head(cooc, 75)
    wordnetwork <- igraph::graph_from_data_frame(wordnetwork) # needs edgelist in first 2 colms.
    
# Plot the Co occurence plot    
    suppressWarnings(ggraph(wordnetwork, layout = "fr") +  
                       
                       geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
                       geom_node_text(aes(label = name), col = "darkgreen", size = 6) +
                       
                       theme_graph(base_family = "Arial Unicode MS") +  
                       theme(legend.position = "none") +
                       
                       labs(title = "Cooccurrence Plot"))


      
  })# Co-occurence Plot ends

### Word Cloud ###
  
  output$plot2 = renderPlot({
    
    x <- model_df()

# Subsetting the XPOS and getting most frequent wors    
    all_words <- subset(x, xpos %in% input$checkGroup)
    top_words = txt_freq(all_words$lemma)

# Try block to check XPOS with wordcloud        
    chck <- try( 
                  wordcloud(words = top_words$key, 
                           freq = top_words$freq, 
                           min.freq = 2, 
                           max.words = 100,
                           random.order = FALSE, 
                           colors = brewer.pal(6, "Dark2"))
                 )

# if TRY block fails use POS        
    if(inherits(chck,'try-error'))
      {
      POS_list <- mapvalues(input$checkGroup , from = c("JJ","NN","NNP","RB","VB"), to = c("ADJ","NOUN","PROPN","ADV","VB"))
      
      all_words =  subset(x, upos %in% POS_list)
      top_words = txt_freq(all_words$lemma)
      
      wordcloud(words = top_words$key, 
                freq = top_words$freq, 
                min.freq = 2, 
                max.words = 100,
                random.order = FALSE, 
                colors = brewer.pal(6, "Dark2"))
        }
    
    
  })# Word Cloud Plot ends

### Data Displayed ###    
  output$File_Data = renderText({
    inputText <-  as.character(Dataset())
    inputText
    
  })
  
  
  
}) # Server ends

# Now call shinyApp function
shinyApp(ui = ui, server = server)
