library(shiny)
library(DT)

library(httr)
library(rvest)
library(dplyr)

options(stringsAsFactors = FALSE)

get.data.from.goodreads <- function(){
  url <- "https://www.goodreads.com/review/list?"
  param <- list(v=2,id=id,key=key,shelf='to-read')
  
  response <- GET(url,query=param)    
  content <- content(response)
  
  isbn13 <- xml_find_all(content,"//isbn13") %>% xml_text()
  titel <- xml_find_all(content,"//title_without_series") %>% xml_text()
  auteur <- xml_find_all(content,"//author") %>% html_nodes("name") %>% html_text()
  score <- content %>% html_nodes("book") %>% xml_find_all('average_rating') %>% html_text()
  # 2 stappen omdat auteur ook een average rating heeft
  
  df <- data.frame(auteur=auteur,titel=titel,isbn13=isbn13,score=score)
  return(df)
}

lookup.in.bib <- function(){
  df <- get.data.from.goodreads()
  df$aanwezig <- ''
  
  for (i in 1:nrow(df)){
    titel.str <- gsub("[[:punct:]]","",df[i,"titel"])
    auteur.str <- gsub("[[:punct:]]","",df[i,"auteur"])
    search.str <- gsub(" ","+",paste(auteur.str,titel.str))
    
    book.url <- paste0("http://zoeken.hasseltdusart.bibliotheek.be/?q=",search.str)
    book.html <- read_html(book.url)
    
    book.link <- (book.html %>% html_nodes(".content"))[1] %>% xml_find_all("a") %>% html_attr("href")
    
    if (length(book.link) > 0){
      book.detail.url <- paste0("http://zoeken.hasseltdusart.bibliotheek.be",book.link)
      book.detail.html <- read_html(book.detail.url)
      
      availability <- book.detail.html %>% html_nodes(xpath = '//*[@data-ga-branch="Dusart"]') %>% html_attr("data-ga-avail")
      
      if (availability == 0){
        df[i,"aanwezig"] <- 1
      } else {
        df[i,"aanwezig"] <- 0
      }
    } else {
      df[i,"aanwezig"] <- 0
    }
    Sys.sleep(1)
  }
  
  df.to.show <- df[,c("auteur","titel","score")]
  
  return(df.to.show)
}

ui <- dashboardPage(
  dashboardHeader(title = "Wat is er in de bib?"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      dataTableOutput("table")
    )
  )
)

server <- function(input,output){
  output$table <- renderDataTable({
    df <- lookup.in.bib()
    DT::datatable(df, rownames = FALSE, options = list(dom='t'))
  })
}

shinyApp(ui = ui, server = server)