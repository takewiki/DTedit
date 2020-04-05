#' Title
#'
#' @param input 输入
#' @param output 输出
#' @param name   outputid 对应uiOutput中的id
#' @param thedata  data部分
#' @param view.cols 列表标题，默认取数据的列名
#' @param edit.cols 编辑列,与thedata严格对应
#' @param edit.label.cols  编码前台显示的名称
#' @param input.types 控件类型key=value形式,value来源于shiny*Input
#' @param input.choices 输入控件的选择值,适用于选择框的内容
#' @param selectize 默认使用即可，效果更好一点
#' @param modal.size 窗口大小,m=median,l=large,s=small
#' @param text.width    文本框大小
#' @param textarea.width   文本区域控件宽
#' @param textarea.height  文件区域控制高
#' @param date.width     日期控件宽
#' @param numeric.width  数值控件宽
#' @param select.width   选择控件宽
#' @param defaultPageLength  每页行数
#' @param title.delete   删除页标题
#' @param title.edit     编辑页标题
#' @param title.add      新增页标题
#' @param label.delete   删除按纽名称
#' @param label.edit     编辑按纽名称
#' @param label.add      新增按纽名称
#' @param label.copy     复制按纽名称
#' @param show.delete    是否显示删除按纽
#' @param show.update    是否显示编辑按纽
#' @param show.insert    是否显示新增按纽
#' @param show.copy      是否显示复制按纽
#' @param callback.delete 删除回调函数
#' @param callback.update  更新回调函数
#' @param callback.insert  新增回调函数
#' @param click.time.threshold 点击按纽时间阈值默认2秒
#' @param datatable.options  datatable额外可传递的选项https://rstudio.github.io/DT/options.html
#' @param view.captions   列表标题显示名称,新增功能
#'
#' @return 返回值
#' @include util.R
#' @import tsdo
#' @export
#'
#' @examples
#' dtedit2()
dtedit2 <- function(input, output, name, thedata,
                   view.cols = names(thedata),
                   view.captions=view.cols,
                   edit.cols = names(thedata),
                   edit.label.cols = edit.cols,
                   input.types,
                   input.choices = NULL,
                   selectize = TRUE,
                   modal.size = 'l',
                   text.width = '100%',
                   textarea.width = '570px',
                   textarea.height = '200px',
                   date.width = '100px',
                   numeric.width = '100px',
                   select.width = '100%',
                   defaultPageLength = 6,
                   title.delete = 'Delete',
                   title.edit = 'Edit',
                   title.add = 'New',
                   label.delete = '删除',
                   label.edit = '修改',
                   label.add = '新增',
                   label.copy = '复制',
                   show.delete = TRUE,
                   show.update = TRUE,
                   show.insert = TRUE,
                   show.copy = TRUE,
                   callback.delete = function(data, row) { },
                   callback.update = function(data, olddata, row) { },
                   callback.insert = function(data, row) { },
                   click.time.threshold = 2, # in seconds
                   datatable.options = list(pageLength=defaultPageLength)
                   
) {
  #一、全局变量设置-------
  #    1.1、设置全局变量为中文-------
  setDTtoCn()
  #code here
  
  #二、函数入口参数检查--------
  #2.1、判断 是否为数据框-------
  if(!is.data.frame(thedata) | ncol(thedata) < 1) {
    stop('输入数据必须是数据框且至少包含一列数据.')
   
  } else if(length(edit.cols) != length(edit.label.cols)) {
    #2.2、编辑字段数与标题数必须一致-------
    stop('编辑窗口的字段名与标题名数量必须一致')
  } else if(!all(view.cols %in% names(thedata))) {
    #         2.3、列表字段必须的数据列中选择-------
    stop('列表字段必须在输入数据的列名中')
  } else if(!all(edit.cols %in% names(thedata))) {
    #2.4、编辑字段必须的数据框内-------
    stop('编辑窗口部分字段名错误')
  }

   # 三、代码主体部分-------
  
    #3.1、添加后缀DataTableName-------
  
  DataTableName <- paste0(name, 'dt')
    #3.2、添加持续化数据result---------
  
  result <- shiny::reactiveValues()
  result$thedata <- thedata
  result$view.cols <- view.cols
  result$edit.cols <- edit.cols
  
    #3.3、添加dt代理对象dt.proxy--------
  
  dt.proxy <- DT::dataTableProxy(DataTableName)
    #3.4定义多选择-----------
  
  selectInputMultiple <- function(...) {
    shiny::selectInput(multiple = TRUE, selectize = selectize, ...)
  }
   #3.5定义输入类型的规范----------
  
  valid.input.types <- getInputTypes()
  
  #3.6自动匹配输入控件类型--------
  #inputTypes <- sapply(thedata[,edit.cols], FUN=mapInputType)
   inputTypes <- mapInputTypes(thedata,edit.cols)
  
  
  if(!missing(input.types)) {
    if(!all(names(input.types) %in% edit.cols)) {
      stop('input.types column not a valid editting column: ',
           paste0(names(input.types)[!names(input.types) %in% edit.cols]))
    }
    if(!all(input.types %in% valid.input.types)) {
      stop(paste0('input.types must only contain values of: ',
                  paste0(valid.input.types, collapse = ', ')))
    }
    inputTypes[names(input.types)] <- input.types
  }
  
  # Convert any list columns to characters before displaying
  for(i in 1:ncol(thedata)) {
    if(nrow(thedata) == 0) {
      thedata[,i] <- character()
    } else if(is.list(thedata[,i])) {
      thedata[,i] <- sapply(thedata[,i], FUN = function(x) { paste0(x, collapse = ', ') })
    }
  }
  
  output[[DataTableName]] <- DT::renderDataTable({
    #添加标题显示功能
    df_setColCaption(thedata,view.cols,view.captions)
  }, options = datatable.options, server=TRUE, selection='single', rownames=FALSE)
  
  getFields <- function(typeName, values) {
    fields <- list()
    for(i in seq_along(edit.cols)) {
      if(inputTypes[i] == 'dateInput') {
        value <- ifelse(missing(values),
                        as.character(Sys.Date()),
                        as.character(values[,edit.cols[i]]))
        fields[[i]] <- dateInput(paste0(name, typeName, edit.cols[i]),
                                 label=edit.label.cols[i],
                                 value=value,
                                 width=date.width)
      } else if(inputTypes[i] == 'selectInputMultiple') {
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
        value <- ifelse(missing(values), '', as.character(values[,edit.cols[i]]))
        fields[[i]] <- shiny::selectInput(paste0(name, typeName, edit.cols[i]),
                                          label=edit.label.cols[i],
                                          choices=levels(result$thedata[,edit.cols[i]]),
                                          selected=value,
                                          width=select.width)
      } else if(inputTypes[i] == 'numericInput') {
        value <- ifelse(missing(values), 0, values[,edit.cols[i]])
        fields[[i]] <- shiny::numericInput(paste0(name, typeName, edit.cols[i]),
                                           label=edit.label.cols[i],
                                           value=value,
                                           width=numeric.width)
      } else if(inputTypes[i] == 'textAreaInput') {
        value <- ifelse(missing(values), '', values[,edit.cols[i]])
        fields[[i]] <- shiny::textAreaInput(paste0(name, typeName, edit.cols[i]),
                                            label=edit.label.cols[i],
                                            value=value,
                                            width=textarea.width, height=textarea.height)
      } else if(inputTypes[i] == 'textInput') {
        value <- ifelse(missing(values), '', values[,edit.cols[i]])
        fields[[i]] <- shiny::textInput(paste0(name, typeName, edit.cols[i]),
                                        label=edit.label.cols[i],
                                        value=value,
                                        width=text.width)
      } else if(inputTypes[i] == 'passwordInput') {
        value <- ifelse(missing(values), '', values[,edit.cols[i]])
        fields[[i]] <- shiny::passwordInput(paste0(name, typeName, edit.cols[i]),
                                            label=edit.label.cols[i],
                                            value=value,
                                            width=text.width)
      } else {
        stop('Invalid input type!')
      }
    }
    return(fields)
  }
  
  output[[paste0(name, '_message')]] <- shiny::renderText('')
  
  updateData <- function(proxy, data, ...) {
    # Convert any list columns to characters before displaying
    for(i in 1:ncol(data)) {
      if(is.list(data[,i])) {
        data[,i] <- sapply(data[,i], FUN = function(x) { paste0(x, collapse = ', ') })
      }
    }
    DT::replaceData(proxy, data, ...)
  }
  
  ##### Insert functions #####################################################
  
  observeEvent(input[[paste0(name, '_add')]], {
    if(!is.null(row)) {
      shiny::showModal(addModal())
    }
  })
  
  insert.click <- NA
  
  observeEvent(input[[paste0(name, '_insert')]], {
    if(!is.na(insert.click)) {
      lastclick <- as.numeric(Sys.time() - insert.click, units = 'secs')
      if(lastclick < click.time.threshold) {
        warning(paste0('Double click detected. Ignoring insert call for ', name, '.'))
        return()
      }
    }
    insert.click <<- Sys.time()
    
    newdata <- result$thedata
    row <- nrow(newdata) + 1
    newdata[row,] <- NA
    for(i in edit.cols) {
      if(inputTypes[i] %in% c('selectInputMultiple')) {
        newdata[[i]][row] <- list(input[[paste0(name, '_add_', i)]])
      } else {
        newdata[row,i] <- input[[paste0(name, '_add_', i)]]
      }
    }
    tryCatch({
      callback.data <- callback.insert(data = newdata, row = row)
      if(!is.null(callback.data) & is.data.frame(callback.data)) {
        result$thedata <- callback.data
      } else {
        result$thedata <- newdata
      }
      updateData(dt.proxy,
                 result$thedata[,view.cols],
                 rownames = FALSE)
      shiny::removeModal()
      return(TRUE)
    }, error = function(e) {
      output[[paste0(name, '_message')]] <<- shiny::renderText(geterrmessage())
      return(FALSE)
    })
  })
  
  addModal <- function(row, values) {
    output[[paste0(name, '_message')]] <- shiny::renderText('')
    fields <- getFields('_add_', values)
    shiny::modalDialog(title = title.add,
                       shiny::div(shiny::textOutput(paste0(name, '_message')), style='color:red'),
                       fields,
                       footer = shiny::column(shiny::modalButton('Cancel'),
                                              shiny::actionButton(paste0(name, '_insert'), 'Save'),
                                              width=12),
                       size = modal.size
    )
  }
  
  ##### Copy functions #######################################################
  
  observeEvent(input[[paste0(name, '_copy')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        shiny::showModal(addModal(values=result$thedata[row,]))
      }
    }
  })
  
  ##### Update functions #####################################################
  
  observeEvent(input[[paste0(name, '_edit')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        shiny::showModal(editModal(row))
      }
    }
  })
  
  update.click <- NA
  
  observeEvent(input[[paste0(name, '_update')]], {
    if(!is.na(update.click)) {
      lastclick <- as.numeric(Sys.time() - update.click, units = 'secs')
      if(lastclick < click.time.threshold) {
        warning(paste0('Double click detected. Ignoring update call for ', name, '.'))
        return()
      }
    }
    update.click <- Sys.time()
    
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        newdata <- result$thedata
        for(i in edit.cols) {
          if(inputTypes[i] %in% c('selectInputMultiple')) {
            newdata[[i]][row] <- list(input[[paste0(name, '_edit_', i)]])
          } else {
            newdata[row,i] <- input[[paste0(name, '_edit_', i)]]
          }
        }
        tryCatch({
          callback.data <- callback.update(data = newdata,
                                           olddata = result$thedata,
                                           row = row)
          if(!is.null(callback.data) & is.data.frame(callback.data)) {
            result$thedata <- callback.data
          } else {
            result$thedata <- newdata
          }
          updateData(dt.proxy,
                     result$thedata[,view.cols],
                     rownames = FALSE)
          shiny::removeModal()
          return(TRUE)
        }, error = function(e) {
          output[[paste0(name, '_message')]] <<- shiny::renderText(geterrmessage())
          return(FALSE)
        })
      }
    }
    return(FALSE)
  })
  
  editModal <- function(row) {
    output[[paste0(name, '_message')]] <- renderText('')
    fields <- getFields('_edit_', values=result$thedata[row,])
    shiny::modalDialog(title = title.edit,
                       shiny::div(shiny::textOutput(paste0(name, '_message')), style='color:red'),
                       fields,
                       footer = column(shiny::modalButton('Cancel'),
                                       shiny::actionButton(paste0(name, '_update'), 'Save'),
                                       width=12),
                       size = modal.size
    )
  }
  
  ##### Delete functions #####################################################
  
  observeEvent(input[[paste0(name, '_remove')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        shiny::showModal(deleteModal(row))
      }
    }
  })
  
  observeEvent(input[[paste0(name, '_delete')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        newdata <- callback.delete(data = result$thedata, row = row)
        if(!is.null(newdata) & is.data.frame(newdata)) {
          result$thedata <- newdata
        } else {
          result$thedata <- result$thedata[-row,]
        }
        updateData(dt.proxy,
                   result$thedata[,view.cols],
                   rownames = FALSE)
        shiny::removeModal()
        return(TRUE)
      }
    }
    return(FALSE)
  })
  
  deleteModal <- function(row) {
    fields <- list()
    for(i in view.cols) {
      fields[[i]] <- div(paste0(i, ' = ', result$thedata[row,i]))
    }
    shiny::modalDialog(title = title.delete,
                       shiny::p('Are you sure you want to delete this record?'),
                       fields,
                       footer = shiny::column(modalButton('Cancel'),
                                              shiny::actionButton(paste0(name, '_delete'), 'Delete'),
                                              width=12),
                       size = modal.size
    )
  }
  
  ##### Build the UI for the DataTable and buttons ###########################
  
  output[[name]] <- shiny::renderUI({
    shiny::div(
      if(show.insert) { shiny::actionButton(paste0(name, '_add'), label.add) },
      if(show.update) { shiny::actionButton(paste0(name, '_edit'), label.edit) },
      if(show.delete) { shiny::actionButton(paste0(name, '_remove'), label.delete) },
      if(show.copy) { shiny::actionButton(paste0(name, '_copy'), label.copy) },
      shiny::br(), shiny::br(), DT::dataTableOutput(DataTableName)
    )
  })
  
  return(result)
}