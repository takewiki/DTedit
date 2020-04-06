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
               'varchar'=as.character
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
    'varchar' = function(x){paste0(" '",as.character(x),"' ")}
    
  )
  
}