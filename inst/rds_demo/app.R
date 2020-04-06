library(shiny)
library(tsda)
library(DTedit)

#建立链接
conn <-conn_rds('test')


#测试链接
# test_conn()


getBooks <- function(table='books') {
  sql <- sql_gen_select(conn,table = table)
  books <-sql_select(conn,sql)
  #针对进行格式化处理
  #如果出来新的数据类型，需要添加格式化函数
  #请修改format_to_dtedit  --formatter.R
  fieldList <-sql_fieldInfo(conn,table)
  for (i in 1:ncol(books)){
    type <-fieldList[i,'FTypeName']
    books[,i] <-format_to_dtedit(type)(books[,i])
    
  }

  return(books)
}

getMax_id <-function(conn,table='books',id_var='id'){
  sql <- sql_gen_select(conn,table,id_var)
  #print(sql)
  r <-sql_select(conn,sql)
  res <- max(as.integer(r[,id_var]))+1
  return(res)
}

##### Callback functions.
books.insert.callback <- function(data, row ,table='books',f=getBooks,id_var='id') {
  sql_header <- sql_gen_insert(conn,table)
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-nrow(fieldList)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Insert <-fieldList[i,'FFieldName']
    type <-fieldList[i,'FTypeName']
    if(col_Insert == id_var){
      res[i] <-paste0(' ',getMax_id(conn,table,id_var),' ')
    }else{
      res[i] <- format_to_sqlInsert(type)(data[row,col_Insert])
    }

  }
  sql_body <- paste0(res,collapse = ',')
  query <-paste0(sql_header,sql_body,")")

  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}

books.update.callback <- function(data, olddata, row,table='books',f=getBooks) {
  query <- paste0("UPDATE ",table , "  SET ",
                  "Authors = '", data[row,'Authors'], "', ",
                  "Date = '", as.character(data[row,'Date']), "', ",
                  "Title = '", data[row,'Title'], "', ",
                  "Publisher = '", as.character(data[row,'Publisher']), "' ",
                  "WHERE id = ", data[row,'id'])
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