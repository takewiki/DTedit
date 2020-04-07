#' datatable编辑器,目前还存在问题，暂时不做调用发现V3-TBU
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
#' @param callback.delete 删除回调函数,规范function(data, row) { }
#' @param callback.update  更新回调函数,规范function(data, olddata, row) { }
#' @param callback.insert  新增回调函数,规范function(data, row) { }
#' @param click.time.threshold 点击按纽时间阈值默认2秒
#' @param datatable.options  datatable额外可传递的选项https://rstudio.github.io/DT/options.html
#' @param view.captions   列表标题显示名称,新增功能
#' @param title.copy 复制标题
#'
#' @return 返回值
#' @include util.R
#' @include dtedit_ui.R
#' @import tsdo
#' @import shiny
#'
#' @examples
#' dtedit2()
dtedit3 <- function(input, output, name, thedata=NULL,
                    
                   view.cols = names(thedata),
                   view.captions=view.cols,
                   edit.cols = names(thedata),
                   edit.label.cols = edit.cols,
                   input.types,
                   input.choices = NULL,
                   selectize = TRUE,
                   modal.size = 'm',
                   text.width = '100%',
                   textarea.width = '570px',
                   textarea.height = '200px',
                   date.width = '100px',
                   numeric.width = '100px',
                   select.width = '100%',
                   defaultPageLength = 6,
                   title.delete = '删除窗口',
                   title.edit = '修改窗口',
                   title.add = '新增窗口',
                   title.copy='复制窗口',
                   label.delete = '删除',
                   label.edit = '修改',
                   label.add = '新增',
                   label.copy = '复制',
                   show.delete = TRUE,
                   show.update = TRUE,
                   show.insert = TRUE,
                   show.copy = TRUE,
                   callback.delete = NULL,
                   callback.update = NULL,
                   callback.insert = NULL,
                   click.time.threshold = 2, # in seconds
                   datatable.options = list(pageLength=defaultPageLength
                                            ),
                   conn=conn_rds('test'),
                   table='t_test',
                   id_var='fid'
                   
) {
  #V3优化部分
  #针对函数进行包含处理
  getBooks <- function(table=table) {
    table <- substitute(table)
    sql <- sql_gen_select(conn,table)
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
  
  #兼容V2的模式
  if(is.null(thedata)){
    thedata = getBooks()

  }
  
  getMax_id <-function(conn,table=table,id_var=id_var){
    sql <- sql_gen_select(conn,table,id_var)
    #print(sql)
    r <-sql_select(conn,sql)
    res <- max(as.integer(r[,id_var]))+1
    return(res)
  }
  
  
  
  if(is.null(callback.insert)){
    
    callback.insert <-function(data, row ,table=table,f=getBooks,id_var=id_var) {
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
  }
  
  if(is.null(callback.update)){
    
    callback.update <-function(data, olddata, row,
                             table=table,
                             f=getBooks,
                             edit.cols = edit.cols,
                             id_var=id_var) 
    {
      sql_header <- sql_gen_update(table);
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
      sql_tail <-paste0(' where ',id_var,' = ',data[row,id_var])
      query <- paste0(sql_header,sql_body,sql_tail)
      
      print(query) # For debugging
      sql_update(conn, query)
      return(f())
    }
  }
  
  if(is.null(callback.delete)){
    callback.delete <-function(data, row ,table =table,f=getBooks,id_var=id_var) {
      sql_header <- sql_gen_delete(table);
      sql_tail <-paste0('  ',id_var,' = ',data[row,id_var])
      query <- paste0(sql_header,sql_tail)
      
      #query <- paste0("DELETE FROM  ",table,"  WHERE id = ", data[row,]$id)
      print(query)
      sql_update(conn, query)
      return(f())
    }
  }
  
#以上为V3修改部分
  
  
  
  
  
  
  
  
  
  
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
    #已经尝试，不能移出
  
  selectInputMultiple <- function(...) {
    shiny::selectInput(multiple = TRUE, selectize = selectize, ...)
  }
   #3.5定义输入类型的规范----------
  
  # valid.input.types <- getInputTypes()
  
  #3.6自动匹配输入控件类型--------

   #inputTypes <- mapInputTypes(thedata,edit.cols)
  
  #3.7检查输入类型---------

  
  inputTypes <-checkInputTypes(thedata,edit.cols,input.types)
  
  #3.8 针对每列数据进行检验，将列表转化为字符--------

   thedata <- df_setList_char(data = thedata)
  
  output[[DataTableName]] <- DT::renderDataTable({
    #3.9添加标题显示功能---------
    df_setColCaption(thedata,view.cols,view.captions)
  }, options = datatable.options, server=TRUE, selection='single', rownames=FALSE)
  
  
  output[[paste0(name, '_message')]] <- shiny::renderText('')
  #3.10更新数据-------
  # updateData
  
  #4操作按纽定义-------------
  #4.1、新增操作--------
  #显示新增窗口
  observeEvent(input[[paste0(name, '_add')]], {
    if(!is.null(row)) {
      shiny::showModal(addModal())
    }
  })
  
  insert.click <- NA
  
  addModal <- function(row, values,title=title.add) {
    output[[paste0(name, '_message')]] <- shiny::renderText('')
    fields <- getFields('_add_', values,  name,
                        inputTypes,
                        input.choices,
                        edit.cols,
                        edit.label.cols,
                        text.width,
                        textarea.width ,
                        textarea.height ,
                        date.width ,
                        numeric.width,
                        select.width,selectInputMultiple,result
    )
    shiny::modalDialog(title = title,
                       shiny::div(shiny::textOutput(paste0(name, '_message')), style='color:red'),
                       fields,
                       footer = shiny::column(shiny::modalButton('取消'),
                                              shiny::actionButton(paste0(name, '_insert'), '保存'),
                                              width=12),
                       size = modal.size
    )
  }
  
  
  observeEvent(input[[paste0(name, '_insert')]], {
    if(!is.na(insert.click)) {
      lastclick <- as.numeric(Sys.time() - insert.click, units = 'secs')
      if(lastclick < click.time.threshold) {
        warning(paste0('检测到双击操作. 取消新增操作 ', name, '.'))
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
  

  
  #4.2、复制操作---------
  
  observeEvent(input[[paste0(name, '_copy')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        shiny::showModal(addModal(values=result$thedata[row,],title = title.copy))
      }
    }
  })
  
  #4.3、编辑一级按纽 --------
  
  observeEvent(input[[paste0(name, '_edit')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        shiny::showModal(editModal(row))
      }
    }
  })
  
  update.click <- NA
  #定义修改界面-----------
  
  editModal <- function(row) {
    output[[paste0(name, '_message')]] <- renderText('')
    fields <- getFields('_edit_', values=result$thedata[row,],  name,
                        inputTypes,
                        input.choices,
                        edit.cols,
                        edit.label.cols,
                        text.width,
                        textarea.width ,
                        textarea.height ,
                        date.width ,
                        numeric.width,
                        select.width,
                        selectInputMultiple,result)
    shiny::modalDialog(title = title.edit,
                       shiny::div(shiny::textOutput(paste0(name, '_message')), style='color:red'),
                       fields,
                       footer = column(shiny::modalButton('取消'),
                                       shiny::actionButton(paste0(name, '_update'), '保存'),
                                       width=12),
                       size = modal.size
    )
  }
  #定义更新操作---------
  observeEvent(input[[paste0(name, '_update')]], {
    if(!is.na(update.click)) {
      lastclick <- as.numeric(Sys.time() - update.click, units = 'secs')
      if(lastclick < click.time.threshold) {
        warning(paste0('检测到双击操作，取消修改操作 ', name, '.'))
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
  

  
  #4.4、定义删除按纽 ----------
  
  #定义删除一级菜单的相应----------
  
  observeEvent(input[[paste0(name, '_remove')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if(!is.null(row)) {
      if(row > 0) {
        shiny::showModal(deleteModal(row))
      }
    }
  })
  
  #删除窗口界面定义-----
  deleteModal <- function(row) {
    fields <- list()
    for(i in seq_along(view.cols)) {
      fields[[view.cols[i]]] <- div(paste0(view.captions[i], ' ： ', result$thedata[row,view.cols[i]]))
    }
    shiny::modalDialog(title = title.delete,
                       shiny::p('是否删除此记录?'),
                       fields,
                       footer = shiny::column(modalButton('取消'),
                                              shiny::actionButton(paste0(name, '_delete'), '删除'),
                                              width=12),
                       size = modal.size
    )
  }
  #定义删除按纽的操作------
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

  
  ##### Build the UI for the DataTable and buttons --------
  output[[name]] <- shiny::renderUI({
    shiny::div(
      if(show.insert) {
        
        dt_btn(name,'_add',label.add)
        },
      if(show.copy) { 
        
        dt_btn(name,'_copy',label.copy)
      },
      if(show.update) { 

        dt_btn(name,'_edit',label.edit)
        },
      if(show.delete) { 
      
        dt_btn(name,'_remove',label.delete)
        },
    
      shiny::br(), shiny::br(), DT::dataTableOutput(DataTableName)
    )
  })
  
  return(result)
}