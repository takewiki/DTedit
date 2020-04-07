#' 定义格式化函数为DTedit
#'
#' @param dbType 数据类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' format_to_dtedit()
format_to_dtedit <-function(dbType){
  f <- switch (dbType,
               'date'=as.Date,
               'int'=as.integer,
               'varchar'=as.character,
               'nvarchar'=as.character,
               'char'=as.character,
               'datetime'=as.character,
               'smallint'=as.integer,
               'sysname'=as.character
               
               )
  
  return(f)
}


#' 处理格式化函数
#'
#' @param dbType  类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' format_to_sqlInsert()
format_to_sqlInsert <-function(dbType){
  f <-switch (dbType,
    'date' = function(x){paste0(" '",as.character(x),"' ")},
    
    'int' = function(x){paste0(" ",as.character(x)," ")},
   
    'varchar' = function(x){paste0(" '",as.character(x),"' ")},
    'Date' = function(x){paste0(" '",as.character(x),"' ")},
    'numeric' = function(x){paste0(" ",as.character(x)," ")},
    'character' = function(x){paste0(" '",as.character(x),"' ")},
    'nvarchar'=function(x){paste0(" '",as.character(x),"' ")},
    'char'=function(x){paste0(" '",as.character(x),"' ")},
    'datetime'=function(x){paste0(" '",as.character(x),"' ")},
    'smallint'=function(x){paste0(" ",as.character(x)," ")},
    'sysname' = function(x){paste0(" '",as.character(x),"' ")}
    
  )
  
}



#' 创建更新格式化函数
#'
#' @param dbType 数据类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' format_to_sqlUpdate()
format_to_sqlUpdate <-function(dbType){
  f <-switch (dbType,
              'date' = function(x){paste0(" '",as.character(x),"' ")},
              'int' = function(x){paste0(" ",as.character(x)," ")},
              'varchar' = function(x){paste0(" '",as.character(x),"' ")},
              'Date' = function(x){paste0(" '",as.character(x),"' ")},
              'numeric' = function(x){paste0(" ",as.character(x)," ")},
              'character' = function(x){paste0(" '",as.character(x),"' ")},
              'nvarchar'=function(x){paste0(" '",as.character(x),"' ")},
              'char'=function(x){paste0(" '",as.character(x),"' ")},
              'datetime'=function(x){paste0(" '",as.character(x),"' ")},
              'smallint'=function(x){paste0(" ",as.character(x)," ")},
              'sysname'=function(x){paste0(" '",as.character(x),"' ")}
              
  )
  
}