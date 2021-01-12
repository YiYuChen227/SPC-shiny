###load所需要的package
library(shinyFiles)
library(shiny)
library(ggplot2)
library(plotly)
library(gridExtra)
library(RMySQL)
library(DBI)
library(dplyr)
library(pool)
library(rhandsontable)
library(DT)
library(shinyalert)
library(data.table)
library(shinyTime)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
###如果出現這行Error in .local(dbObj, ...):internal error in RS_DBI_getConnection: corrupt connection handle表示重複連線太多次，將下行#去掉跑一遍即可
#lapply(dbListConnections(MySQL()), dbDisconnect)


####SQL連線資訊
dbnametest = "spc"
hosttest = "localhost"
porttest = 3306
usernametest = "root"
passwordtest = "mpi!12345"
my_db <- dbConnect(MySQL(),
                   user = usernametest,
                   password = passwordtest,
                   host = hosttest,
                   port = porttest,
                   dbname= dbnametest)

###linux環境中文編碼
dbGetQuery(my_db, "SET NAMES 'UTF8'")
#Encoding(type[["TITLE"]])<-"UTF-8"



###load data
product <- dbGetQuery(my_db, "SELECT * FROM product")
product <- product[with(product,order(UpdateDate, decreasing=TRUE)),] #照更新時間排序
type <- dbGetQuery(my_db, "SELECT * FROM type")
spec <- dbGetQuery(my_db ,"SELECT * FROM spec")
measure_yield_cp <- dbGetQuery(my_db, "SELECT * FROM measure_yield_cp")
measure_yield_wp <- dbGetQuery(my_db, "SELECT * FROM measure_yield_wp")
measure_detail <- dbGetQuery(my_db, "SELECT * FROM measure_detail")
measure_all <- dbGetQuery(my_db, "SELECT * FROM measure_all")
measure_total_yield <- dbGetQuery(my_db,"SELECT * FROM measure_total_yield")
measure_info <- dbGetQuery(my_db, "SELECT * FROM measure_info")
cpkTable <- read.csv("CPK.csv", header = TRUE, sep = ",")
update_log <- read.csv("/srv/shiny-server/spc/updatelog.csv", header = TRUE, sep = ",",fileEncoding='big5')

dbDisconnect(my_db)



###總良率預警

yield_all_Warning <- function(type,measure_total_yield){
  warning_all_yield <- measure_total_yield[,c("OperateDate","FlatYield","FlipYield","TotalYield")]
  return(warning_all_yield)
}

###Cpk,良率,目標值預警table與ui連結(type,sProduct,sMold)

SQL_warning <- function(type, sProduct, sMold){
  my_db <- dbConnect(MySQL(),
                     user = usernametest,
                     password = passwordtest,
                     host = hosttest,
                     port = porttest,
                     dbname= dbnametest)
  dbGetQuery(my_db, "SET NAMES 'UTF8'")
  t <- dbGetQuery(my_db, paste0("SELECT TITLE FROM type WHERE Type = '", type,"'"))
  w <- dbGetQuery(my_db, paste0("SELECT OperateDate, TITLE, Cpk, round(Cp,2) as Cp , concat(Yield,'%') as Yield, mean FROM measure_all WHERE Product = '", sProduct, "' AND MoldVersion = '", sMold,"'"))
  s <- dbGetQuery(my_db, paste0("SELECT TITLE, Grade, UCL, LCL, sCpk, sYield, chosen FROM spec WHERE Product = '", sProduct, "'"))
  s <- filter(s,chosen==1)
  i <- dbGetQuery(my_db, paste0("SELECT Product,MoldVersion,Operator,OperateDate, `ignore` FROM measure_info WHERE Product = '", sProduct, "' AND MoldVersion = '", sMold,"'"))
  d <- dbGetQuery(my_db, paste0("SELECT Product,MoldVersion,OperateDate, TITLE, number, value, Note FROM measure_detail WHERE Product = '", sProduct, "' AND MoldVersion = '", sMold,"'"))
  y <- dbGetQuery(my_db, paste0("SELECT OperateDate, FlatYield, FlipYield, TotalYield FROM measure_total_yield WHERE Product = '", sProduct, "' AND MoldVersion = '", sMold,"'"))
  y[y==-1]<-NA
  dbDisconnect(my_db)
  return(list(type=t, warning_all=w, spec=s, measure_info = i, measure_total_yield = y, measure_detail = d))
}

###Operator

operator <- function(type,measure_info){
  operator <- measure_info[,c("Operator")]
  return(operator)
}

###Cpk預警，使用資料表：measure_all, type, spec

CpkWarning <- function(warning_all, type ,spec){
  warning_cpk <- warning_all[,c("OperateDate","TITLE","Cpk","Cp")]
  warning_cpk <- warning_cpk[with(warning_cpk,order(OperateDate, decreasing=TRUE)),]
  warning_cpk <- warning_cpk[which(warning_cpk$OperateDate==warning_cpk$OperateDate[1]),]
  warning_cpk <- merge(x = warning_cpk, y = spec, by = "TITLE", all.y = FALSE)
  warning_cpk <- warning_cpk[,c("OperateDate","TITLE","Cpk","Cp","sCpk")]
  warning_cpk <- warning_cpk[!duplicated(warning_cpk$TITLE), ]
  warning_cpk <-na.omit(warning_cpk)
  return(warning_cpk)
}

###Cpk歷史預警，使用資料表：measure_all, type, spec

CpkHisWarning <- function(warning_all, type ,spec){
  warning_cpk <- warning_all[,c("OperateDate","TITLE","Cpk","Cp")]
  warning_cpk <- merge(x = warning_cpk, y = spec, by = "TITLE", all.y = TRUE)
  warning_cpk <- warning_cpk[,c("OperateDate","TITLE","Cpk","Cp","sCpk")]
  warning_cpk <-na.omit(warning_cpk)
  return(warning_cpk)
}

###良率預警，使用資料表：measure_all, type, spec

YieldWarning <- function(warning_all, type, spec){
  warning_yield <- warning_all[,c("OperateDate","TITLE", "Yield")]
  warning_yield <- warning_yield[with(warning_yield,order(OperateDate, decreasing=TRUE)),]
  warning_yield <- warning_yield[which(warning_yield$OperateDate==warning_yield$OperateDate[1]),]
  warning_yield <- merge(x = warning_yield, y = spec, by = "TITLE", all.y = FALSE)
  warning_yield <- warning_yield[,c("OperateDate","TITLE","Yield","sYield")]
  warning_yield <- warning_yield[!duplicated(warning_yield$TITLE), ]
  warning_yield <-na.omit(warning_yield)
  return(warning_yield)
}

###良率歷史預警，使用資料表：measure_all, type, spec

YieldHisWarning <- function(warning_all, type, spec){
  warning_yield <- warning_all[,c("OperateDate","TITLE", "Yield")]
  warning_yield <- merge(x = warning_yield, y = spec, by = "TITLE", all.y = TRUE)
  warning_yield <- warning_yield[,c("OperateDate","TITLE","Yield","sYield")]
  warning_yield <-na.omit(warning_yield)
  return(warning_yield)
}

###目標值預警，使用資料表：measure_all, spec

TargetWarning <- function(warning_all, spec){
  warning_target <- warning_all[,c("OperateDate","TITLE", "mean")]
  warning_target <- warning_target[with(warning_target,order(OperateDate, decreasing=TRUE)),]
  warning_target <- warning_target[which(warning_target$OperateDate==warning_target$OperateDate[1]),]
  warning_target <- merge(x = warning_target, y = spec, by = "TITLE",all.y=FALSE)
  warning_target <- warning_target[,c("OperateDate","Grade","TITLE","mean","UCL","LCL")]
  warning_target <- warning_target[complete.cases(warning_target), ]
  return(warning_target)
}

###目標值歷史預警，使用資料表：measure_all, spec

TargetHisWarning <- function(warning_all, spec){
  warning_target <- warning_all[,c("OperateDate","TITLE", "mean")]
  warning_target <- merge(x = warning_target, y = spec, by = "TITLE",all.y=FALSE)
  warning_target <- warning_target[,c("OperateDate","Grade","TITLE","mean","UCL","LCL")]
  warning_target <- warning_target[complete.cases(warning_target), ]
  return(warning_target)
}


###短期圖table與ui連結(sProduct,sMold)

SQL_short <- function(sProduct, sMold){
  my_db <- dbConnect(MySQL(),
                     user = usernametest,
                     password = passwordtest,
                     host = hosttest,
                     port = porttest,
                     dbname= dbnametest)
  dbGetQuery(my_db, "SET NAMES 'UTF8'")
  SQL1 <- paste0("SELECT OperateDate,TITLE,mean,std,Cpk FROM measure_all WHERE Product = '", sProduct, "' AND MoldVersion = '", sMold, "' AND mean!=0" )
  sh <- dbGetQuery(my_db, SQL1)
  sp <- dbGetQuery(my_db, paste0("SELECT * FROM spec WHERE Product = '", sProduct, "'"))
  dbDisconnect(my_db)
  return(list(short=sh, spec=sp))
}

###短期圖規格table與ui連結(sProduct,sMold,sGrade)

SQL_short_grade <- function(sProduct, sMold, sGrade){
  my_db <- dbConnect(MySQL(),
                     user = usernametest,
                     password = passwordtest,
                     host = hosttest,
                     port = porttest,
                     dbname= dbnametest)
  dbGetQuery(my_db, "SET NAMES 'UTF8'")
  SQL1 <- paste0("SELECT OperateDate,TITLE,mean,std,Cpk FROM measure_all WHERE Product = '", sProduct, "' AND MoldVersion = '", sMold, "' AND mean!=0" )
  sh <- dbGetQuery(my_db, SQL1)
  sp <- dbGetQuery(my_db, paste0("SELECT * FROM spec WHERE Product = '", sProduct, "' AND Grade ='", sGrade, "'"))
  dbDisconnect(my_db)
  return(list(short=sh, spec=sp))
}

###短期圖、長期圖、兩倍標準差、良率、Cpk折線圖table與ui連結(Product, sTitle)

SQL_spec <- function(sProduct, sTitle){
  my_db <- dbConnect(MySQL(),
                     user = usernametest,
                     password = passwordtest,
                     host = hosttest,
                     port = porttest,
                     dbname= dbnametest)
  dbGetQuery(my_db, "SET NAMES 'UTF8'")
  df<-dbGetQuery(my_db, paste0("SELECT * FROM spec WHERE Product = '", sProduct, "'"))
  df<-df[which(df$TITLE==sTitle),]
  dbDisconnect(my_db)
  return(df)
}
SQL_spec_grade <- function(sProduct, sGrade, sTitle){
  my_db <- dbConnect(MySQL(),
                     user = usernametest,
                     password = passwordtest,
                     host = hosttest,
                     port = porttest,
                     dbname= dbnametest)
  dbGetQuery(my_db, "SET NAMES 'UTF8'")
  df<-dbGetQuery(my_db, paste0("SELECT * FROM spec WHERE Product = '", sProduct, "'AND Grade = '", sGrade, "'"))
  df<-df[which(df$TITLE==sTitle),]
  dbDisconnect(my_db)
  return(df)
}

###畫短期圖，使用資料表：spec,measure_all

short_control_chart <- function(sTitle,short,spec,plot_num){
  short <- short[which(short$TITLE==sTitle),]
  short <- short[with(short,order(OperateDate, decreasing=TRUE)),]
  short <- short[!duplicated(short$OperateDate), ]
  short <- short[1:10,]
  spec <- spec[which(spec$TITLE==sTitle),]
  incProgress(1/plot_num)
  renderPlotly({
    ggplotly(ggplot(short, aes(x = OperateDate, y = mean, group = 1)) + geom_line() + geom_point() +
               theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0, size = 11)) +
               geom_hline(aes(yintercept=spec$USL), colour="#4ECDC4") +
               geom_hline(aes(yintercept=spec$LSL), colour="#4ECDC4") +
               geom_hline(aes(yintercept=spec$UCL), colour="#C44D58") +
               geom_hline(aes(yintercept=spec$LCL), colour="#C44D58") +
               ggtitle(sTitle))%>%layout(margin=list(b=200))
  })
}

###畫長期圖、兩倍標準差長期圖，table與ui連結(Product, sMold, sTitle, date)

SQL_long_mean <- function(sProduct, sMold, sTitle, date){
  my_db <- dbConnect(MySQL(),
                     user = usernametest,
                     password = passwordtest,
                     host = hosttest,
                     port = porttest,
                     dbname= dbnametest)
  dbGetQuery(my_db, "SET NAMES 'UTF8'")
  SQL1 <- paste0("SELECT OperateDate,TITLE,mean,std,Cpk,Yield FROM measure_all WHERE Product = '", sProduct, "' AND MoldVersion = '", sMold, "'")
  df <- dbGetQuery(my_db, SQL1)
  df <- df[which(df$TITLE==sTitle),]
  df <- df[(df$OperateDate > paste(as.character(date[1]),"00:00:00"))&(df$OperateDate < paste(as.character(date[2]),"23:59:59")),]
  df$mean[df$mean==0]<-NA
  df <-na.omit(df)
  dbDisconnect(my_db)
  return(df)
}

SQL_long_mean_grade <- function(sProduct, sMold, sGrade,sTitle, date){
  my_db <- dbConnect(MySQL(),
                     user = usernametest,
                     password = passwordtest,
                     host = hosttest,
                     port = porttest,
                     dbname= dbnametest)
  dbGetQuery(my_db, "SET NAMES 'UTF8'")
  SQL1 <- paste0("SELECT OperateDate,TITLE,mean,std,Cpk,Yield FROM measure_all WHERE Product = '", sProduct, "' AND MoldVersion = '", sMold, "'")
  df <- dbGetQuery(my_db, SQL1)
  df <- df[which(df$TITLE==sTitle),]
  df <- df[(df$OperateDate > paste(as.character(date[1]),"00:00:00"))&(df$OperateDate < paste(as.character(date[2]),"23:59:59")),]
  df$mean[df$mean==0]<-NA
  df <-na.omit(df)
  dbDisconnect(my_db)
  return(df)
}


###畫製程能力圖，table與ui連結(Product, sMold, sTitle, date)

SQL_long_cpk <- function(sProduct, sMold, sTitle, date){
  my_db <- dbConnect(MySQL(),
                     user = usernametest,
                     password = passwordtest,
                     host = hosttest,
                     port = porttest,
                     dbname= dbnametest)
  dbGetQuery(my_db, "SET NAMES 'UTF8'")
  SQL1 <- paste0("SELECT OperateDate,TITLE,mean,std,Cpk,Yield FROM measure_all WHERE Product = '", sProduct, "' AND MoldVersion = '", sMold, "'")
  df <- dbGetQuery(my_db, SQL1)
  df <- df[which(df$TITLE==sTitle),]
  df <- df[(df$OperateDate > paste(as.character(date[1]),"00:00:00"))&(df$OperateDate < paste(as.character(date[2]),"23:59:59")),]
  df$Cpk[df$Cpk==0.00]<-NA
  df <-na.omit(df)
  dbDisconnect(my_db)
  return(df)
}

###畫尺寸良率折線圖，table與ui連結(Product, sMold, sTitle, date)

SQL_long_yield <- function(sProduct, sMold, sTitle, date){
  my_db <- dbConnect(MySQL(),
                     user = usernametest,
                     password = passwordtest,
                     host = hosttest,
                     port = porttest,
                     dbname= dbnametest)
  dbGetQuery(my_db, "SET NAMES 'UTF8'")
  SQL1 <- paste0("SELECT OperateDate,TITLE,mean,std,Cpk,Yield FROM measure_all WHERE Product = '", sProduct, "' AND MoldVersion = '", sMold, "'")
  df <- dbGetQuery(my_db, SQL1)
  df <- df[which(df$TITLE==sTitle),]
  df <- df[(df$OperateDate > paste(as.character(date[1]),"00:00:00"))&(df$OperateDate < paste(as.character(date[2]),"23:59:59")),]
  df$Yield[df$Yield==0.0]<-NA
  df <-na.omit(df)
  dbDisconnect(my_db)
  return(df)
}

###畫cp折線圖，table與ui連結(Product, sMold, sTitle, date)

SQL_long_cp <- function(sProduct, sMold, sTitle, date){
  my_db <- dbConnect(MySQL(),
                     user = usernametest,
                     password = passwordtest,
                     host = hosttest,
                     port = porttest,
                     dbname= dbnametest)
  dbGetQuery(my_db, "SET NAMES 'UTF8'")
  SQL1 <- paste0("SELECT OperateDate,TITLE,mean,std,Cp,Yield FROM measure_all WHERE Product = '", sProduct, "' AND MoldVersion = '", sMold, "'")
  df <- dbGetQuery(my_db, SQL1)
  df <- df[which(df$TITLE==sTitle),] 
  df <- df[(df$OperateDate > paste(as.character(date[1]),"00:00:00"))&(df$OperateDate < paste(as.character(date[2]),"23:59:59")),]
  df$Cp[df$Cp==0.00]<-NA
  df <-na.omit(df)
  dbDisconnect(my_db)
  return(df)
}


###畫長期圖，使用資料表(measure_all, spec)

long_control_chart <- function(input_sProduct, input_sMold, input_sTitle1, input_date1){
  long <- SQL_long_mean(input_sProduct, input_sMold,input_sTitle1, input_date1)
  spec <- SQL_spec(input_sProduct, input_sTitle1)
  
  pl<-plot_ly(mode="lines+markers",type = "scatter",showlegend=FALSE,hoverinfo="text",
              text=paste("OperateDate:",long$OperateDate,"<br>","mean:",long$mean,"<br>","Cpk:",long$Cpk)   
  )%>%
    add_trace(x=sort(long$OperateDate),y=long$mean,line=list(color='black',width=2),marker=list(color="black",size=7))%>%
    add_lines(x=long$OperateDate,y = spec$USL, line = list(color="#4ECDC4"), name = "USL") %>%
    add_lines(x=long$OperateDate,y = spec$LSL, line = list(color="#4ECDC4"), name = "LSL")%>%
    add_lines(x=long$OperateDate,y = spec$UCL, line = list(color="#C44D58"), name = "UCL") %>%
    add_lines(x=long$OperateDate,y = spec$LCL, line = list(color="#C44D58"), name = "LCL")%>%
    layout(xaxis = list(range=c(min(long$OperateDate),max(long$OperateDate))))%>%
    layout(xaxis = list(title="OperateDate"),
           yaxis = list(title="mean"))
  return(pl)
}

long_control_chart_grade <- function(input_sProduct, input_sMold, input_sGrade, input_sTitle1, input_date1){
  long <- SQL_long_mean_grade(input_sProduct, input_sMold, input_sGrade,input_sTitle1, input_date1)
  spec <- SQL_spec_grade(input_sProduct, input_sGrade, input_sTitle1)
  
  pl<-plot_ly(mode="lines+markers",type = "scatter",showlegend=FALSE,hoverinfo="text",
              text=paste("OperateDate:",long$OperateDate,"<br>","mean:",long$mean,"<br>","Cpk:",long$Cpk)   
  )%>%
    add_trace(x=sort(long$OperateDate),y=long$mean,line=list(color='black',width=2),marker=list(color="black",size=7))%>%
    add_lines(x=long$OperateDate,y = spec$USL, line = list(color="#4ECDC4"), name = "USL") %>%
    add_lines(x=long$OperateDate,y = spec$LSL, line = list(color="#4ECDC4"), name = "LSL")%>%
    add_lines(x=long$OperateDate,y = spec$UCL, line = list(color="#C44D58"), name = "UCL") %>%
    add_lines(x=long$OperateDate,y = spec$LCL, line = list(color="#C44D58"), name = "LCL")%>%
    layout(xaxis = list(range=c(min(long$OperateDate),max(long$OperateDate))))%>%
    layout(xaxis = list(title="OperateDate"),
           yaxis = list(title="mean"))
  return(pl)
  
  
}

###畫兩倍標準差長期圖，使用資料表(measure_all, spec)

twoStd_control_chart <- function(input_sProduct, input_sMold, input_sTitle1, input_date1){
  long <- SQL_long_mean(input_sProduct, input_sMold, input_sTitle1, input_date1)
  spec <- SQL_spec(input_sProduct, input_sTitle1)
  
  ggplotly(ggplot(long, aes(x = OperateDate, y = mean, group = 1)) + geom_line() + geom_point(aes(text=sprintf('Cpk: %s', Cpk))) +
             theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
             geom_errorbar(aes(ymin=mean-2*std, ymax=mean+2*std)) +
             geom_hline(aes(yintercept=spec$USL), colour="#4ECDC4") +
             geom_hline(aes(yintercept=spec$LSL), colour="#4ECDC4") +
             geom_hline(aes(yintercept=spec$UCL), colour="#C44D58") +
             geom_hline(aes(yintercept=spec$LCL), colour="#C44D58"))%>%layout(margin=list(b=200),height = 600)
}

twoStd_control_chart_grade <- function(input_sProduct, input_sMold, input_sGrade, input_sTitle1, input_date1){
  long <- SQL_long_mean_grade(input_sProduct, input_sMold, input_sGrade, input_sTitle1, input_date1)
  spec <- SQL_spec_grade(input_sProduct, input_sGrade, input_sTitle1)
  
  ggplotly(ggplot(long, aes(x = OperateDate, y = mean, group = 1)) + geom_line() + geom_point(aes(text=sprintf('Cpk: %s', Cpk))) +
             theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
             geom_errorbar(aes(ymin=mean-2*std, ymax=mean+2*std)) +
             geom_hline(aes(yintercept=spec$USL), colour="#4ECDC4") +
             geom_hline(aes(yintercept=spec$LSL), colour="#4ECDC4") +
             geom_hline(aes(yintercept=spec$UCL), colour="#C44D58") +
             geom_hline(aes(yintercept=spec$LCL), colour="#C44D58"))%>%layout(margin=list(b=200))
}
###畫Cpk折線圖，使用資料表(measure_all, spec)

cpk_control_chart <- function(input_sProduct, input_sMold, input_sTitle1, input_date1){
  long <- SQL_long_cpk(input_sProduct, input_sMold, input_sTitle1, input_date1)
  spec <- SQL_spec(input_sProduct, input_sTitle1)
  
  pl<-plot_ly(mode="lines+markers",type = "scatter",showlegend=FALSE,hoverinfo="text",
              text=paste("OperateDate:",long$OperateDate,"<br>","Cpk:",long$Cpk)
  )%>%
    add_trace(x=sort(long$OperateDate),y=long$Cpk,line=list(color='black',width=2),marker=list(color="black",size=7))%>%
    add_lines(x=long$OperateDate,y = 1.33, line = list(color="#4ECDC4")) %>%
    layout(xaxis = list(range=c(min(long$OperateDate),max(long$OperateDate))))%>%
    layout(xaxis = list(title="OperateDate"),
           yaxis = list(title="Cpk"))
  return(pl)
}

###畫良率折線圖，使用資料表(measure_all, spec)

yield_control_chart <- function(input_sProduct, input_sMold, input_sTitle1, input_date1){
  long <- SQL_long_yield(input_sProduct, input_sMold, input_sTitle1, input_date1)
  spec <- SQL_spec(input_sProduct, input_sTitle1)
  
  pl<-plot_ly(mode="lines+markers",type = "scatter",showlegend=FALSE,hoverinfo="text",
              text=paste("OperateDate:",long$OperateDate,"<br>","Yield:",paste(long$Yield,"%"))
  )%>%
    add_trace(x=sort(long$OperateDate),y=long$Yield,line=list(color='black',width=2),marker=list(color="black",size=7))%>%
    add_lines(x=long$OperateDate,y = 100, line = list(color="#4ECDC4")) %>%
    layout(xaxis = list(range=c(min(long$OperateDate),max(long$OperateDate))))%>%
    layout(xaxis = list(title="OperateDate"),
           yaxis = list(title="Yield"))
  return(pl)
}

###畫cp折線圖，使用資料表(measure_all, spec)

cp_control_chart <- function(input_sProduct, input_sMold, input_sTitle1, input_date1){
  long <- SQL_long_cp(input_sProduct, input_sMold, input_sTitle1, input_date1)
  spec <- SQL_spec(input_sProduct, input_sTitle1)
  
  pl<-plot_ly(mode="lines+markers",type = "scatter",showlegend=FALSE,hoverinfo="text",
              text=paste("OperateDate:",long$OperateDate,"<br>","Cp:",long$Cp)
  )%>%
    add_trace(x=sort(long$OperateDate),y=long$Cp,line=list(color='black',width=2),marker=list(color="black",size=7))%>%
    layout(xaxis = list(range=c(min(long$OperateDate),max(long$OperateDate))))%>%
    layout(xaxis = list(title="OperateDate"),
           yaxis = list(title="Cp"))
  
  return(pl)
  
}




###量測原始資料表，使用資料表:type,measure_detail





DetailWarning <- function(type,measure_detail){
  measure_all_detail <- measure_detail[,c("Product","MoldVersion","OperateDate","TITLE","number","value","Note")]
  measure_all_detail <- measure_all_detail[with(measure_all_detail,order(OperateDate, decreasing=TRUE)),]
  return(measure_all_detail)
}

###異常txt排除

InfoExclude <- function(type,measure_info){
  measure_info_exclude <- measure_info[,c("Product","MoldVersion","OperateDate","ignore")]
  return(measure_info_exclude)
}




# onStop(function() {
#   print("Close DB connection...........")
#   poolClose(my_db)
# })
