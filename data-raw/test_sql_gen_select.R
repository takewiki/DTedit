library(tsda)
library(DTedit)
conn <-conn_rds('test')
sql <- sql_gen_select()
mydata <- sql_select(conn,sql)
fieldList <-sql_fieldInfo(conn,'books')
for (i in 1:ncol(mydata)){
  type <-fieldList[i,'FTypeName']
  mydata[,i] <-format_to_dtedit(type)(mydata[,i])
  
}
str(mydata)
