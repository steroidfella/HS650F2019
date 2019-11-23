library(shiny);library(jsonlite);library(rJava);library(shinythemes);library(rvest)
library(tm); library(SnowballC); library(cld2);library(httr);library(XML); library(ggplot2)
library(V8); library(dplyr); library(DT); library(stringi); library(repmis)

# Data
jel_data <- read.csv("http://github.com/steroidfella/HS650F2019/raw/master/Data/jel_data.csv", stringsAsFactors = FALSE)
jel_data <- jel_data[,-1]

# Define UI -----------
# ---------------------

ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # header
                titlePanel("Economics Research Topic Classification"),

                fluidRow(
                  
                  column(4,
                         wellPanel(
                           textInput("title_id", "Title",""),
                           textAreaInput("abstract_id", "Abstract","", height="100%"),
                           textInput("keywords_id", "Keywords",""),
                           
                           actionButton("update", "Find"),
                           helpText("Click to find classification"))
                  ),
                  
                  column(4,
                         h4(textOutput("title_d")),
                         textOutput("title"),
                         h4(textOutput("abstract_d")),
                         textOutput("abstract"),
                         h4(textOutput("keywords_d")),
                         textOutput("keywords")
                  ),
                  column(4,
                         plotOutput(outputId = "barchart"))
                  ),
                fluidRow(
                  column(12,
                    tableOutput("table")
                  )
                )
)


# Define server logic ------
# --------------------------

server <- function(input, output) {
  
  # process the textinput
  new_research_table <- reactive({  
    
    
    # creating table
    
    aniRoi2 <- data.frame(eprintid = input$res_id, 
                          keywords = input$keywords_id,
                          abstract = input$abstract_id,
                          title = input$title_id,
                          stringsAsFactors = FALSE)
    return(aniRoi2)
  })
  
  # process the text file
  
  cleaning <- function(x){
    temp <- Corpus(VectorSource(eval(parse(text=paste("input$", x, "_id",sep = "")))))
    temp <- tm_map(temp, tolower)
    temp <- tm_map(temp, removePunctuation)
    temp <- tm_map(temp, removeNumbers)
    temp <- tm_map(temp, removeWords, stopwords("english"))
    temp <- tm_map(temp, stripWhitespace)
    return(temp)
  }
  
  textdata <- reactive(
    {
      abstract <- cleaning("abstract")
      title <- cleaning("title")
      keywords <- cleaning("keywords")
      
      # abstract 3 important words
      abstract_dtm <- DocumentTermMatrix(abstract)
      mfa<-findMostFreqTerms(abstract_dtm, n=3)
      term_a <- matrix(0, ncol = 3)
        for(j in 1:3){
          term_a[1,j] <- unlist(paste(names(mfa[[1]]), sep=","))[j]
        }
      # title first 10 words
      term_t <- matrix(0, ncol = 10)
        for(j in 1:10){
          term_t[1,j] <- unlist(strsplit(trimws(as.character(title[[1]]),
                                                which = "both"), " "))[j]
        }
      # keywords first 15 words
      term_k <- matrix(0, ncol = 15)
        for(j in 1:15){
          term_k[1,j] <- unlist(strsplit(as.character(keywords[[1]]), " "))[j]
        }
      wrd <- data.frame(cbind(term_a, term_t, term_k),stringsAsFactors = FALSE)
      names(wrd)[1:3] <- c(paste0("abstract_", 1:3))
      names(wrd)[4:13] <- c(paste0("title_", 1:10))
      names(wrd)[14:28] <- c(paste0("keywords_", 1:15))
      
      # create table result
      sum_store <- matrix(0, nrow = nrow(jel_data), ncol = 1)
      result_store <- matrix(0, nrow = 1, ncol = 3)
      rownames(sum_store) <- jel_data$X1
      for(i in 1:nrow(sum_store)){
        sum_store[i,1] <- sum(strsplit(jel_data$keywords_clean,' ')[[i]] %in% wrd[1,1:28])
        result_store[1,c(1:ncol(result_store))] <- 
          paste(row.names(sum_store)[order(sum_store[,1], decreasing=TRUE)][1:ncol(result_store)], sep=",")
      }
      rs <- data.frame(t(result_store))
      names(rs)[1] <- "X1"
      rs <- merge(rs, jel_data, by="X1")
      rs <- rs[,-4]
      colnames(rs) <- (c("Code","Classifications","Keywords"))
      return(rs)
    })
  
  # merge two function as data.frame
  title1 <- eventReactive(input$update, {
    input$title_id
  })
  output$title <- renderText({title1()})
  abstract1 <- eventReactive(input$update, {
    input$abstract_id
  })
  output$abstract <-renderText({abstract1()})
  keywords1 <- eventReactive(input$update, {
    input$keywords_id
  })
  output$keywords <- renderText({keywords1()})
  title_t <- eventReactive(input$update, {
    "Title"
  })
  output$title_d <- renderText({title_t()})
  abstract_t <- eventReactive(input$update, {
    "Abstract"
  })
  output$abstract_d <- renderText({abstract_t()})
  keywords_t <- eventReactive(input$update, {
    "Keywords"
  })
  output$keywords_d <- renderText({keywords_t()})
  
  # create bar chart      
  mybarchart <- eventReactive(input$update, {
    cmb <- paste(input$title_id, input$abstract_id,input$keywords_id)
    cmb <- Corpus(VectorSource(cmb))
    cmb <- tm_map(cmb, tolower)
    cmb <- tm_map(cmb, removePunctuation)
    cmb <- tm_map(cmb, removeNumbers)
    cmb <- tm_map(cmb, removeWords, stopwords("english"))
    cmb <- tm_map(cmb, stripWhitespace)
    cmb_dtm <- DocumentTermMatrix(cmb)
    mfc<-findMostFreqTerms(cmb_dtm, n=10)
    ggplot(data.frame(mfc[[1]]),aes(x=reorder(rownames(data.frame(mfc[[1]])),data.frame(mfc[[1]])[,1]),
                                    y=data.frame(mfc[[1]])[,1], fill=data.frame(mfc[[1]])[,1]))+
      scale_fill_gradient(low = "#7FE9FF", high = "#000333")+
      geom_bar(stat = "identity", show.legend = FALSE)+
      labs(title = "Most Frequent Words", x="Words", y="Frequency")+
      coord_flip()+guides(color="none")
  })
  output$barchart <- renderPlot(mybarchart())
  
  # create table
  mytable <- eventReactive(input$update,{
    textdata()
  })
  output$table <- renderTable({
    mytable()
  })
}
# Run the app ----------
# ----------------------

shinyApp(ui = ui, server = server)