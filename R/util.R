#' 设置DT为中文
#' https://datatables.net/reference/option/language
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
      lengthMenu = '显示 _MENU_ 项结果',
      infoFiltered='(从 _MAX_ 记录过滤结果)',
      loadingRecords='数据加载中...',
      processing='处理中,请稍候...',
      infoEmpty='没有找到匹配记录',
      emptyTable='没有可显示数据',
      zeroRecords='没有找到匹配记录'
      )
    ))
  
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



#' 检查输入类型
#'
#' @param data 数据
#' @param edit.cols 编辑框
#' @param input.types 输入类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' checkInputTypes()
checkInputTypes <- function(data,edit.cols,input.types) {
  valid.input.types <- getInputTypes()
  inputTypes <- mapInputTypes(data,edit.cols)
  if(!missing(input.types)) {
    if(!all(names(input.types) %in% edit.cols)) {
      stop('输入界面的控件名称有误，没有在输入数据框中',
           paste0(names(input.types)[!names(input.types) %in% edit.cols]))
    }
    if(!all(input.types %in% valid.input.types)) {
      stop(paste0('输入界面的控件类型指定有误: ',
                  paste0(valid.input.types, collapse = ', ')))
    }
    #设置指定字段的数据类型
    inputTypes[names(input.types)] <- input.types
  }
  return(inputTypes)
  
}






#' 针对控件元素进行处理
#'
#' @param name  名称
#' @param inputTypes  输入类型
#' @param input.choices 输入选项
#' @param edit.cols  编辑字段名
#' @param edit.label.cols 编辑字段标题
#' @param text.width   文件宽度
#' @param textarea.width   文件区域宽度
#' @param textarea.height 文本区域调试
#' @param date.width 日期宽度
#' @param numeric.width 数值宽度
#' @param select.width 选择赛诺
#' @param typeName  类型名
#' @param values  类型值
#' @param selectInputMultiple 传递自定义函数
#' @param result 添加数据缓存对象
#'
#' @return 返回字段列表
#' @export
#'
#' @examples
#' getFields()
getFields <- function( typeName, 
                       values,
                      name,
                      inputTypes,
                      input.choices,
                      edit.cols,
                      edit.label.cols,
                      text.width,
                      textarea.width ,
                      textarea.height ,
                      date.width ,
                      numeric.width,
                      select.width ,
                      selectInputMultiple,
                      result
                     
                      ) {
  #定义结果
  fields <- list()
  for(i in seq_along(edit.cols)) {
    #针对日期控件进行处理-------
    if(inputTypes[i] == 'dateInput') {
      value <- ifelse(missing(values),
                      as.character(Sys.Date()),
                      as.character(values[,edit.cols[i]]))
      fields[[i]] <- dateInput(paste0(name, typeName, edit.cols[i]),
                               label=edit.label.cols[i],
                               value=value,
                               width=date.width)
    } else if(inputTypes[i] == 'selectInputMultiple') {
      #针对选择控件进行处理------
      value <- ifelse(missing(values), '', values[,edit.cols[i]])
      if(is.list(value)) {
        value <- value[[1]]
      }
      choices <- ''
      if(!missing(values)) {
        choices <- unique(unlist(values[,edit.cols[i]]))
      }
      if(!is.null(input.choices)) {
        if(edit.cols[i] %in% names(input.choices)) {
          choices <- input.choices[[edit.cols[i]]]
        }
      }
      if(length(choices) == 1 & choices == '') {
        warning(paste0('No choices available for ', edit.cols[i],
                       '. Specify them using the input.choices parameter'))
      }
      fields[[i]] <- selectInputMultiple(paste0(name, typeName, edit.cols[i]),
                                         label=edit.label.cols[i],
                                         choices=choices,
                                         selected=value,
                                         width=select.width)
      
    } else if(inputTypes[i] == 'selectInput') {
      #针对选择控件进行处理-------
      value <- ifelse(missing(values), '', as.character(values[,edit.cols[i]]))
      fields[[i]] <- shiny::selectInput(paste0(name, typeName, edit.cols[i]),
                                        label=edit.label.cols[i],
                                        choices=levels(result$thedata[,edit.cols[i]]),
                                        selected=value,
                                        width=select.width)
    } else if(inputTypes[i] == 'numericInput') {
      #针对数值控件进行处理-------
      value <- ifelse(missing(values), 0, values[,edit.cols[i]])
      fields[[i]] <- shiny::numericInput(paste0(name, typeName, edit.cols[i]),
                                         label=edit.label.cols[i],
                                         value=value,
                                         width=numeric.width)
    } else if(inputTypes[i] == 'textAreaInput') {
      #针对文件区域进行处理-------
      value <- ifelse(missing(values), '', values[,edit.cols[i]])
      fields[[i]] <- shiny::textAreaInput(paste0(name, typeName, edit.cols[i]),
                                          label=edit.label.cols[i],
                                          value=value,
                                          width=textarea.width, height=textarea.height)
    } else if(inputTypes[i] == 'textInput') {
      #针对文本控件进行处理----------
      value <- ifelse(missing(values), '', values[,edit.cols[i]])
      fields[[i]] <- shiny::textInput(paste0(name, typeName, edit.cols[i]),
                                      label=edit.label.cols[i],
                                      value=value,
                                      width=text.width)
    } else if(inputTypes[i] == 'passwordInput') {
      #针对密码控件进行处理--------
      value <- ifelse(missing(values), '', values[,edit.cols[i]])
      fields[[i]] <- shiny::passwordInput(paste0(name, typeName, edit.cols[i]),
                                          label=edit.label.cols[i],
                                          value=value,
                                          width=text.width)
    } else {
      stop('输入内容不合规')
    }
  }
  return(fields)
} 




#' 更新数据
#'
#' @param proxy 代理对象，支持更新 
#' @param data 数据
#' @param ... 其他选英
#'
#' @return 返回值
#' @export
#'
#' @examples
#' updateData()
updateData <- function(proxy, data, ...) {
  # Convert any list columns to characters before displaying
  for(i in 1:ncol(data)) {
    if(is.list(data[,i])) {
      data[,i] <- sapply(data[,i], FUN = function(x) { paste0(x, collapse = ', ') })
    }
  }
  DT::replaceData(proxy, data, ...)
}
