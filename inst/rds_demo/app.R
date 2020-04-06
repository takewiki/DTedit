library(shiny)
library(tsda)
library(DTedit)

#建立链接
conn <-conn_rds('test')
#测试链接
# test_conn()


getBooks <- function() {
  books <- sql_select(conn, "SELECT * FROM books")
  books$Date <- as.Date(books$Date)
  books$Publisher <- as.factor(books$Publisher)
  return(books)
}

##### Callback functions.
books.insert.callback <- function(data, row ,table='books',f=getBooks) {
  query <- paste0("INSERT INTO   ",table,"  (id, Authors, Date, Title, Publisher) VALUES (",
                  "", max(getBooks()$id) + 1, ", ",
                  "'", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
                  "'", as.character(data[row,]$Date), "', ",
                  "'", data[row,]$Title, "', ",
                  "'", as.character(data[row,]$Publisher), "' ",
                  ")")
  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}

books.update.callback <- function(data, olddata, row,table='books',f=getBooks) {
  query <- paste0("UPDATE ",table , "  SET ",
                  "Authors = '", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
                  "Date = '", as.character(data[row,]$Date), "', ",
                  "Title = '", data[row,]$Title, "', ",
                  "Publisher = '", as.character(data[row,]$Publisher), "' ",
                  "WHERE id = ", data[row,]$id)
  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}

books.delete.callback <- function(data, row ,table ='books',f=getBooks) {
  query <- paste0("DELETE FROM  ",table,"  WHERE id = ", data[row,]$id)
  sql_update(conn, query)
  return(f())
}

##### Create the Shiny server
server <- function(input, output) {
       books <- getBooks()
       print(books)
  dtedit2(input, output,
         name = 'books',
         thedata = books,
         edit.cols = c('Title', 'Authors', 'Date', 'Publisher'),
         edit.label.cols = c('Book Title', 'Authors', 'Publication Date', 'Publisher'),
         input.types = c(Title='textAreaInput'),
         input.choices = list(Authors = unique(unlist(books$Authors))),
         view.cols = names(books)[c(1,2,4)],
         view.captions = c('序号','作者','书名'),
         callback.update = books.update.callback,
         callback.insert = books.insert.callback,
         callback.delete = books.delete.callback)
  

}

##### Create the shiny UI
ui <- fluidPage(
  h3('Books'),
  uiOutput('books'),
  hr()
)

shinyApp(ui = ui, server = server)