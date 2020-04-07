library(shiny)
library(tsda)
library(DTedit)

#建立链接
conn <-conn_rds('nsic')


#测试链接
# test_conn()


getBooks <- function(table='t_tsp_ques') {
  sql <- sql_gen_select(conn,table = table)
  #print(sql)
  books <-sql_select(conn,sql)
  #print(books)
  #针对进行格式化处理
  #如果出来新的数据类型，需要添加格式化函数
  #请修改format_to_dtedit  --formatter.R
  fieldList <-sql_fieldInfo(conn,table)
  print(fieldList)
  for (i in 1:ncol(books)){
    type <-fieldList[i,'FTypeName']
    print(type)
    books[,i] <-format_to_dtedit(type)(books[,i])
    
  }

  return(books)
}

getMax_id <-function(conn,table='t_tsp_ques',id_var='FId'){
  sql <- sql_gen_select(conn,table,id_var)
  #print(sql)
  r <-sql_select(conn,sql)
  res <- max(as.integer(r[,id_var]))+1
  return(res)
}

##### Callback functions.
books.insert.callback <- function(data, row ,table='t_tsp_ques',f=getBooks,id_var='FId') {
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

books.update.callback <- function(data, olddata, row,
                                  table='t_tsp_ques',
                                  f=getBooks,
                                  edit.cols = c('FQues','FAnsw'),
                                  id_var='FId') 
  {
  sql_header <- sql_gen_update(table);
  print(sql_header)
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-length(edit.cols)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Update <-edit.cols[i]
    #col_Insert <-fieldList[fieldList$,'FFieldName']
    type <-fieldList[fieldList$FFieldName == col_Update,'FTypeName']
    res[i] <- paste0(' ',col_Update,' = ',format_to_sqlUpdate(type)(data[row,col_Update]))
   
    
  }
  sql_body <- paste0(res,collapse = ',')
  print(sql_body)
  sql_tail <-paste0(' where ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_body,sql_tail)

  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}

books.delete.callback <- function(data, row ,table ='t_tsp_ques',f=getBooks,id_var='FId') {
  sql_header <- sql_gen_delete(table);
  sql_tail <-paste0('  ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_tail)
  
  #query <- paste0("DELETE FROM  ",table,"  WHERE id = ", data[row,]$id)
  print(query)
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
         edit.cols = c('FQues','FAnsw'),
         edit.label.cols = c('问题','答案'),
         input.types = c(FAnsw='textAreaInput'),
         #input.choices = list(FNUMBER = unique(unlist(books$FNUMBER))),
         view.cols = c('FId','FQues','FAnsw'),
         view.captions = c('序号','问题','答案'),
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