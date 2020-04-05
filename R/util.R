#' 设置DT为中文
#'
#' @return 返回值
#'
#' @examples setDTtoCn();
setDTtoCn <- function(){
  options(DT.options = list(
    searchHighlight = TRUE,
    language = list(
      info = '显示第_START_ 至 _END_ 项结果，共 _TOTAL_ 项',
      search = '搜索:',
      paginate = list(previous = '上页', `next` = '下页'),
      lengthMenu = '显示 _MENU_ 项结果')))
  
}


#' 定义合格的输入控件类型
#'
#' @return 返回结果
#' @export
#'
#' @examples 
#' getInputTypes()
getInputTypes <- function() {
  res <-c('dateInput', 'selectInput', 'numericInput',
          'textInput', 'textAreaInput', 'passwordInput', 'selectInputMultiple')
  return(res)
}



#' 匹配输入字段数据
#'
#' @param x 原始数据数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mapInputType()
mapInputType <- function(x) {
  switch(class(x),
         list = 'selectInputMultiple',
         character = 'textInput',
         Date = 'dateInput',
         factor = 'selectInput',
         integer = 'numericInput',
         numeric = 'numericInput')
}


#' 数据类型处理
#'
#' @param data 数据框
#' @param cols 字段名
#'
#' @return 返回值
#' @export 
#'
#' @examples
#' mapInputTypes()
mapInputTypes <- function(data,cols){
  res <-sapply(data[,cols], FUN=mapInputType)
  return(res)
}


