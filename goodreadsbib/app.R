# intro -------------------------------------------------------------
# This script creates an R Shiny dashboard. The dashboard only shows one table
# The table contains books that are on my Goodreads to-read list and are also currently
# available at my favorite library.

# setup -------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(DT)

library(httr)
library(rvest)
library(dplyr)

options(stringsAsFactors = FALSE)

# functions ---------------------------------------------------------------
get.data.from.goodreads <- function(){
  # Get all books that are on my to-read list and keep relevant information in a dataframe.
  # The to-read list is obtained through Goodreads API
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

lookup.in.bib <- function(df){
  # the dataframe parameter expects a df with book info
  # currently, this df is the same as the get.data.from.goodreads() output
  # through web scraping the availability at my library of every book in the dataframe gets checked
  # return original dataframe + availability column
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

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Wat is er in de bib?"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      dataTableOutput("table")
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input,output){
  output$table <- renderDataTable({
    df <- get.data.from.goodreads()
    df <- lookup.in.bib(df)
    DT::datatable(df, rownames = FALSE, options = list(dom='t'))
  })
}


# app ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)