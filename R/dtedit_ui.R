#' 定义按纽
#'
#' @param name 对象名称
#' @param btn_title 按纽标题
#' @param btn_name 按续名称
#'
#' @return 返回值
#' @import shiny
#' @export
#'
#' @examples
#' dt_btn()
dt_btn <- function(name,btn_name='_add',btn_title='新增'){
  res <- actionButton(paste0(name, btn_name), btn_title)
  return(res)
}


