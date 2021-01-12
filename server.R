
###呼叫外部R檔
source("global.R",encoding = "UTF-8")
Sys.setlocale(locale = "zh_TW.UTF8")


shinyServer(function(input, output, session) {
  
  autoInvalidate<- reactiveTimer(20000)
  
  ###篩選完Product後更新不同的模具版本
  
  output$sMold<-renderUI({
    selectInput("sMold", label="選擇模具版本:", choices = product$MoldVersion[product$Product==input$sProduct])
  })
  
  output$sGrade <- renderUI({
    selectInput("sGrade",label = "選擇規格:",unique(as.character(spec$Grade)), selected = "B")
  })
  
  
  observeEvent(input$sMold,{
    if(grepl("V", input$sMold)){
      warning <- SQL_warning("CP",input$sProduct, input$sMold)
    }else{
      warning <- SQL_warning("WP",input$sProduct, input$sMold)
    }
    type <-warning$type
    measure_info <- warning$measure_info
    
    output$info <- renderText({c(paste("產品:",input$sProduct),paste("模具版本:",input$sMold),paste("人員:",tail(operator(type,measure_info),1)))})
    
    #in the place of input$color you can put your personal title
    
  })
  observeEvent(input$sProduct,{
    titlejs=paste0("document.title ='", input$sProduct,"'")
    runjs(titlejs)
  })
  
  
  ###長期圖尺寸更新
  
  observeEvent(input$sMold,{
    if(grepl("V", input$sMold)){Type="CP"}else{Type="WP"}
    updateSelectInput(session, "sTitle1", label="選擇觀測尺寸:", choices = type$TITLE[type$Type==Type])
  })
  
  ###平躺、翻肚、總良率預警看板
  
  observeEvent(input$sMold, {
    if(grepl("V", input$sMold)){
      warning <- SQL_warning("CP",input$sProduct, input$sMold)
    }else{
      warning <- SQL_warning("WP",input$sProduct, input$sMold)
    }
    type <-warning$type
    measure_total_yield <- warning$measure_total_yield
    
    output$yield_flat_current<-renderValueBox({
      autoInvalidate()
      v <- tail(yield_all_Warning(type,measure_total_yield)$FlatYield,1)
      t <- tail(yield_all_Warning(type,measure_total_yield)[,1],1)
      valueBox(paste(v,"%"),tags$p(paste("平躺良率",t),style="font-size: 20px"),
               color = ifelse(v>=100,"green",
                              ifelse(v>0,"yellow",
                                     ifelse(v<=0,"red"))))
    })
    output$yield_flip_current<-renderValueBox({
      autoInvalidate()
      o <- na.omit(yield_all_Warning(type,measure_total_yield))
      v <- tail(o[,3],1)
      t <- tail(o[,1],1)
      if(length(which(v==v))==0){
        valueBox(paste0(""),tags$p(paste0("沒有量測值"),style="font-size: 20px"),color="black")
      }else{
        valueBox(paste(v,"%"),tags$p(paste("翻肚良率",t),style="font-size: 20px"),
                 color = ifelse(v>=100,"green",
                                ifelse(v>0,"yellow",
                                       ifelse(v<=0,"red"))))
      }
      
    })
    output$yield_total_current<-renderValueBox({
      autoInvalidate()
      o <- na.omit(yield_all_Warning(type,measure_total_yield))
      v <- tail(o[,4],1)
      t <- tail(o[,1],1)
      if(length(which(v==v))==0){
        valueBox(paste0(""),tags$p(paste0("沒有量測值"),style="font-size: 20px"),color="black")
      }else{
        valueBox(paste(v,"%"),tags$p(paste("總良率",t),style="font-size: 20px"),
                 color = ifelse(v>=100,"green",
                                ifelse(v>0,"yellow",
                                       ifelse(v<=0,"red"))))
      }
    })
  })
  
  
  ###預警Button
  
  observeEvent(input$cpk_btn,{
    showModal(modalDialog(
      shinydashboard::box(
        width = 12,height=500,
        title = "製程能力預警表", status = "primary", solidHeader = TRUE,
        collapsible = F,
        div(style = 'overflow-y: scroll', DT::dataTableOutput("cpk_warning", height = "250px")),
        actionButton("cpktable","CPK對應表",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        actionButton("cptable","CP值指標",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      ),
      footer = tagList(
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$yield_btn,{
    showModal(modalDialog(
      shinydashboard::box(
        width = 12,height=500,
        title = "尺寸良率預警表", status = "primary", solidHeader = TRUE,
        collapsible = F,
        div(style = 'overflow-y: scroll', DT::dataTableOutput("yield_warning", height = "250px"))
      ),
      footer = tagList(
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$mean_btn,{
    showModal(modalDialog(
      shinydashboard::box(
        width = 12,height=500,
        title = "目標值預警表", status = "primary", solidHeader = TRUE,
        collapsible = F,
        div(style = 'overflow-y: scroll', DT::dataTableOutput("target_warning", height = "250px"))
      ),
      footer = tagList(
        modalButton("Cancel")
      )
    ))
  })
  
  ###CPK、尺寸良率、目標值預警
  
  observeEvent(input$sMold, {
    if(grepl("V", input$sMold)){
      warning <- SQL_warning("CP", input$sProduct, input$sMold)
    }else{
      warning <- SQL_warning("WP", input$sProduct, input$sMold)
    }
    type <- warning$type
    warning_all <- warning$warning_all
    spec <- warning$spec
    output$cpk_warning<-DT::renderDataTable({DT::datatable(CpkWarning(warning_all, type, spec), rownames=FALSE, options = list( dom = 'tip',scrollX = TRUE,pageLength=5,
                                                                                                                                rowCallback = JS('
                                                                                                                                                 function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                                                                                                                                 // Bold and green cells for conditions
                                                                                                                                                 if (parseFloat(aData[2]) <1)
                                                                                                                                                 $("td:eq(2)", nRow).css("background-color", "	#FF3333");
                                                                                                                                                 if (parseFloat(aData[3]) < 1)
                                                                                                                                                 $("td:eq(3)", nRow).css("background-color", "	#FF3333");
                                                                                                                                                 
                                                                                                                                                 }')))})
    output$yield_warning<-DT::renderDataTable({DT::datatable(YieldWarning(warning_all, type, spec), rownames=FALSE, options = list(dom = 'tip',scrollX = TRUE,pageLength=5,
                                                                                                                                   rowCallback = JS('
                                                                                                                                                    function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                                                                                                                                    // Bold and green cells for conditions
                                                                                                                                                    if (parseFloat(aData[2]) <100)
                                                                                                                                                    $("td:eq(2)", nRow).css("background-color", "	#FF3333");
                                                                                                                                                    }')))})
    output$target_warning<-DT::renderDataTable({DT::datatable(TargetWarning(warning_all, spec), rownames=FALSE, options = list(dom = 'tip',scrollX = TRUE,pageLength=5,
                                                                                                                               rowCallback = JS('
                                                                                                                                                function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                                                                                                                                // Bold and green cells for conditions
                                                                                                                                                if (parseFloat(aData[3]) >parseFloat(aData[4]))
                                                                                                                                                $("td:eq(3)", nRow).css("background-color", "	#FF3333");
                                                                                                                                                if (parseFloat(aData[3]) <parseFloat(aData[5]))
                                                                                                                                                $("td:eq(3)", nRow).css("background-color", "	#FF3333");
                                                                                                                                                
                                                                                                                                                }')))})
    
    })
  
  ###製程能力對應表
  
  output$cpktable<-renderTable({cpkTable})
  output$cptable<-renderTable({cpTable})
  
  observeEvent(input$cpktable, {
    showModal(modalDialog(
      title = "CPK對應表",
      renderTable({cpkTable}),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$cptable, {
    showModal(modalDialog(
      title = "CP值指標",
      img(src = 'cp_pic.png'),
      easyClose = TRUE
    ))
  })
  
  ###畫短期圖
  
  output$short_layout_UI<-renderUI({
    if(grepl("V", input$sMold)){
      uiOutput("short_layout_V")
    }else{
      uiOutput("short_layout_E")
    }
  })
  
  ###CP針呈現
  
  output$short_layout_V<-renderUI({
    input$sMold
    input$sGrade
    #autoInvalidate()
    layout <- isolate({
      short_chart <- SQL_short(input$sProduct, input$sMold)
      short <- short_chart$short
      spec <- short_chart$spec
      
      short_chart_grade <- SQL_short_grade(input$sProduct, input$sMold, input$sGrade) 
      short_grade <- short_chart_grade$short
      spec_grade <- short_chart_grade$spec
      
      
      withProgress(message = 'Control chart in progress',detail = "This may take a while", value = 0, {
        fluidPage(
          fluidRow(
            column(4, short_control_chart("A(offset)", short, spec, plot_num=19)),
            column(4, short_control_chart("C(Bending length)", short, spec, plot_num=19)),
            column(4, short_control_chart("D(Tail length)", short, spec,plot_num=19))
          ),
          fluidRow(
            if(input$sProduct=="N6(A,U9)_2.5mil_LF_Point_Head_Uncoating_C(HT-B)_T2 (REV.3)"|input$sProduct=="N6(A,U9)_2mil_Point_Head_Uncoating_C(HT-B)_T4"){
              column(4, short_control_chart("B(Tip length)", short_grade, spec_grade,plot_num=19))}
            else{
              column(4, short_control_chart("B(Tip length)", short, spec,plot_num=19))
            },
            column(4, short_control_chart("C+D(Bending+tail)", short, spec,plot_num=19)),
            column(4, short_control_chart("G(angle)", short, spec,plot_num=19))
          ),
          fluidRow(
            column(4, short_control_chart("straight X", short, spec,plot_num=19)),
            column(4, short_control_chart("膝高度寬", short, spec,plot_num=19)),
            column(4, short_control_chart("K(Tip deg)", short, spec,plot_num=19))
          ),
          fluidRow(
            column(4, short_control_chart("N(needle-dia center tolerance) 針徑偏心", short, spec,plot_num=19)),
            column(4, short_control_chart("M(conical circular runout  tolerance )針尖端偏擺", short, spec,plot_num=19)),
            column(4, short_control_chart("O(TIP直度)", short, spec,plot_num=19))
          ),
          fluidRow(
            column(4, short_control_chart("H1 X", short, spec,plot_num=19)),
            column(4, short_control_chart("H2 X", short, spec,plot_num=19)),
            column(4, short_control_chart("H3 X", short, spec,plot_num=19))
          ),
          fluidRow(
            column(4, short_control_chart("I(扁尾寬)", short, spec,plot_num=19)),
            column(4, short_control_chart("L(扁尾高)", short, spec,plot_num=19)),
            column(4, short_control_chart("Tip width", short, spec,plot_num=19))
          ),
          fluidRow(
            column(4, short_control_chart("R(R值)", short, spec,plot_num=19))
          )
        )
      })
    })
    
    layout
    
  })
  
  ###WP針呈現
  
  output$short_layout_E<-renderUI({
    input$sMold 
    layout <- isolate({
      short_chart <- SQL_short(input$sProduct, input$sMold)
      short <- short_chart$short
      spec <- short_chart$spec
      #autoInvalidate()
      withProgress(message = 'Control chart in progress',detail = "This may take a while", value = 0, {
        fluidPage(
          fluidRow(
            column(4, short_control_chart("E(Total length)", short, spec,plot_num=10)),
            column(4, short_control_chart("B(Tip length)", short, spec,plot_num=10)),
            column(4, short_control_chart("膝高度寬", short, spec,plot_num=10))
          ),
          fluidRow(
            column(4, short_control_chart("Tip width", short, spec,plot_num=10)),
            column(4, short_control_chart("I(扁尾寬)", short, spec,plot_num=10)),
            column(4, short_control_chart("L(扁尾高)", short, spec,plot_num=10))
          ),
          fluidRow(
            column(4, short_control_chart("N(needle-dia center tolerance) 針徑偏心", short, spec,plot_num=10)),
            column(4, short_control_chart("M(conical circular runout  tolerance )針尖端偏擺", short, spec,plot_num=10)),
            column(4, short_control_chart("K(Tip deg)", short, spec,plot_num=10))
          ),
          fluidRow(
            column(4, short_control_chart("O(TIP直度)", short, spec,plot_num=10))
          )
        )
      })
    })
    layout
  })
  
  
  
  ###畫長期圖
  
  output$long_layout_UI<-renderUI({
    if(input$sProduct=="N6(A,U9)_2.5mil_LF_Point_Head_Uncoating_C(HT-B)_T2 (REV.3)"|input$sProduct=="N6(A,U9)_2mil_Point_Head_Uncoating_C(HT-B)_T4"){
      plotlyOutput("long_grade_layout")
    }else{
      plotlyOutput("long_layout")
    }
  })
  
  output$long_grade_layout<-renderPlotly({
    if(input$sTitle1=="B(Tip length)"){
      input$sGrade
      input$sTitle1
      input$date_long1
      layout <- isolate(
        long_control_chart_grade(input$sProduct, input$sMold, input$sGrade,input$sTitle1, input$date_long1)
      )
      rangeslider(layout)  
    }else{
      input$sTitle1
      input$date_long1
      layout <- isolate(
        long_control_chart(input$sProduct, input$sMold, input$sTitle1, input$date_long1)
      )
      rangeslider(layout) 
    }
    
    
  })
  
  output$long_layout<-renderPlotly({
    input$sTitle1
    input$date_long1
    layout <- isolate(
      long_control_chart(input$sProduct, input$sMold, input$sTitle1, input$date_long1)
    )
    rangeslider(layout)
    
  })
  
  
  ###畫兩倍標準差長期圖
  
  output$twoStd_layout_UI<-renderUI({
    if(input$sProduct=="N6(A,U9)_2.5mil_LF_Point_Head_Uncoating_C(HT-B)_T2 (REV.3)"|input$sProduct=="N6(A,U9)_2mil_Point_Head_Uncoating_C(HT-B)_T4"){
      plotlyOutput("twoStd_layout_grade")
    }else{
      plotlyOutput("twoStd_layout")
    }
  })
  
  
  
  output$twoStd_layout_grade<-renderPlotly({
    if(input$sTitle1=="B(Tip length)"){
      input$sGrade
      input$sTitle1
      input$date_long1
      layout <- isolate(
        twoStd_control_chart_grade(input$sProduct, input$sMold, input$sGrade, input$sTitle1, input$date_long1)
      )
      layout
    }else{
      input$sTitle1
      input$date_long1
      layout <- isolate(
        twoStd_control_chart(input$sProduct, input$sMold, input$sTitle1, input$date_long1)
      )
      layout
    }
    
  })
  
  output$twoStd_layout<-renderPlotly({
    input$sTitle1
    input$date_long1
    layout <- isolate(
      twoStd_control_chart(input$sProduct, input$sMold, input$sTitle1, input$date_long1)
    )
    layout
  })
  
  
  
  ###畫cpk折線圖
  
  output$cpk_layout_UI<-renderUI({
    plotlyOutput("cpk_layout")
  })
  
  output$cpk_layout<-renderPlotly({
    input$sTitle1
    input$date_long1
    layout <- isolate(
      cpk_control_chart(input$sProduct, input$sMold, input$sTitle1, input$date_long1)
    )
    rangeslider(layout)
  })
  
  
  ###畫cp折線圖
  
  output$cp_layout_UI<-renderUI({
    plotlyOutput("cp_layout")
  })
  
  output$cp_layout<-renderPlotly({
    input$sTitle1
    input$date_long1
    layout <- isolate(
      cp_control_chart(input$sProduct, input$sMold, input$sTitle1, input$date_long1)
    )   
    rangeslider(layout)
    
  })
  
  ###畫良率折線圖
  
  output$yield_UI<-renderUI({
    plotlyOutput("yield_layout")
  })
  
  output$yield_layout<-renderPlotly({
    input$sTitle1
    input$date_long1
    layout <- isolate(
      yield_control_chart(input$sProduct, input$sMold, input$sTitle1, input$date_long1)
    )
    rangeslider(layout)
  })
  
  ###畫總良率折線圖
  
  output$yieldall_layout_UI <- renderUI({
    fluidPage(
      plotlyOutput("chart_yield")
    )
  })
  
  
  ###總良率趨勢圖
  
  observeEvent(input$sMold, {
    if(grepl("V", input$sMold)){
      warning <- SQL_warning("CP",input$sProduct, input$sMold)
    }else{
      warning <- SQL_warning("WP",input$sProduct, input$sMold)
    }
    type <-warning$type
    measure_total_yield <- warning$measure_total_yield
    
    output$chart_yield <- renderPlotly({
      pl<-plot_ly(yield_all_Warning(type,measure_total_yield),x=~OperateDate,y=~FlatYield,type="scatter",mode="lines+markers",name = "平躺良率")%>%
        add_trace(y=~FlipYield,name="翻肚良率",mode = "lines+markers")%>%
        add_trace(y=~TotalYield,name="總良率",mode = "lines+markers")%>%
        layout(xaxis = list(title="OperateDate"),
               yaxis = list(title="Yield"))
      return(pl)
    })
  })
  
  ###改SPEC資料表
  
  ###login
  USER <- reactiveValues(Logged = FALSE , session = session$user) 
  source("www/Login.R",  local = TRUE)
  
  output$table <- renderUI({    
    if (USER$Logged == TRUE) {      
      rHandsontableOutput('spec')
    }
  })
  output$subset_1 <- renderUI({
    if (USER$Logged == TRUE) {      
      selectInput('subset_product',"Product:",c("All",unique(as.character(spec$Product))))
    }
  })
  output$subset_2 <- renderUI({
    if (USER$Logged == TRUE) {      
      selectInput('subset_TITLE',"TITLE:",c("All",unique(as.character(spec$TITLE))))
    }
  })
  
  # output$btn2 <- renderUI({
  #   if (USER$Logged == TRUE){
  #     actionButton("refresh","Refresh",icon("refresh"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  #   }
  # })
  
  
  ###refresh更新後重新撈資料庫
  # refresh_spec <- eventReactive(input$refresh,ignoreNULL = FALSE,{
  # 
  #   df<-loaddb(tablename = 'spec',
  #              username = usernametest,
  #              password = passwordtest,
  #              host = hosttest,
  #              port = porttest,
  #              dbname= dbnametest)
  #   return(df)
  # })
  
  subset_spec<-reactive({
    if(input$subset_product != "All"){
      spec <- spec[spec$Product == input$subset_product,]
    }
    if(input$subset_TITLE != "All"){
      spec <- spec[spec$TITLE == input$subset_TITLE,]
    }
    spec
  })
  
  ###spec table  
  output$spec<- renderRHandsontable(
    rhandsontable(data.table(subset_spec()), search = TRUE,digits = 5,width = 1300,height = 600) %>% hot_col("UCL", format = "0.00000") %>% hot_col("LCL", format = "0.00000") %>% hot_col("USL", format = "0.00000") %>% hot_col("LSL", format = "0.00000")
    %>%hot_table(highlightCol = TRUE, highlightRow = TRUE)
    %>%hot_col(col="Product",type = "dropdown",source = spec$Product)
    %>%hot_col(col="TITLE",type = "dropdown",source = spec$TITLE)
  )
  
  ###spec 存檔
  output$btn1 <-renderUI({
    if (USER$Logged == TRUE){
      actionButton("saveBtn","Save",icon("glyphicon glyphicon-download-alt", lib = "glyphicon"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
  observeEvent(input$saveBtn,{
    write.csv(hot_to_r(input$spec),row.names = FALSE,file=file.path("/srv/shiny-server/spc/modify_data/",paste(Sys.time(),"_spec.csv",sep="")))
    showNotification("Data has already been uploaded to the database.",
                     action = a(href = "javascript:location.reload();", "Please Reload Page")
    )
  })
  
  
  ###上傳csv檔
  output$input_table <- renderUI({
    if (USER$Logged == TRUE){
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    }
  })
  
  output$output_table <-renderUI({
    if (USER$Logged == TRUE){
      tableOutput("spec_csv")
    }
  })
  
  output$spec_csv  <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
  })
  
  ###量測原始資料
  observeEvent(input$send, {
    withProgress(message = 'Datatable in progress',detail = "This may take a while", value = 0, {
      if(grepl("V", input$sMold)){
        warning_detail <- SQL_warning("CP", input$sProduct, input$sMold)
      }else{
        warning_detail <- SQL_warning("WP", input$sProduct, input$sMold)
      }
      measure_detail <- warning_detail$measure_detail
      type <- warning_detail$type
      row_table <- DetailWarning(type,measure_detail)
      
      output$measure_table<-renderRHandsontable(
        rhandsontable(data.table(row_table),search = TRUE,digits = 5,width = 1300,height = 600))
    })
    ###量測原始資料存檔
    observeEvent(input$save,{
      write.csv(hot_to_r(input$measure_table),row.names = FALSE,file=file.path("/srv/shiny-server/spc/modify_data/",paste(Sys.time(),"_note.csv",sep="")))
      showNotification("Data has already been uploaded to the database.",
                       action = a(href = "javascript:location.reload();", "Please Reload Page"))
    })
    
  })
  
  
  ###排除異常txt檔
  observeEvent(input$search,{
    showModal(modalDialog(
      title = "排除異常資料",
      size = "l",
      rHandsontableOutput('remove_table'),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("delete", "Delete")
        #actionButton("refresh_txt","Refresh")
      )
    )
    )
  })
  
  observeEvent(input$sMold, {
    if(grepl("V", input$sMold)){
      data_exclude <- SQL_warning("CP", input$sProduct, input$sMold)
    }else{
      data_exclude <- SQL_warning("WP", input$sProduct, input$sMold)
    }
    measure_info <- data_exclude$measure_info
    type <- data_exclude$type
    remove <- InfoExclude(type,measure_info)
    ##delete資料表後，refresh重新撈資料庫
    # remove <- eventReactive(input$refresh_txt,ignoreNULL = FALSE,{
    #   df<-loaddb(tablename = 'measure_info',
    #              username = usernametest,
    #              password = passwordtest,
    #              host = hosttest,
    #              port = porttest,
    #              dbname= dbnametest)
    #   return(df)
    # })
    
    
    output$remove_table<-renderRHandsontable(
      rhandsontable(data.table(remove),search = TRUE,digits = 5,width = 1300,height = 600)
    )
  })
  
  ###排除異常txt存檔  
  observeEvent(input$delete,{
    write.csv(hot_to_r(input$remove_table),row.names = FALSE,file=file.path("/srv/shiny-server/spc/modify_data/",paste(Sys.time(),"_remove.csv",sep="")))
    showNotification("Data has already been uploaded to the database.",
                     action = a(href = "javascript:location.reload(true);", "Please Reload Page")
    )
  })
  
  ###cpk,良率,目標值歷史預警表  
  observeEvent(input$sMold, {
    if(grepl("V", input$sMold)){
      warning <- SQL_warning("CP", input$sProduct, input$sMold)
    }else{
      warning <- SQL_warning("WP", input$sProduct, input$sMold)
    }
    type <- warning$type
    warning_all <- warning$warning_all
    spec <- warning$spec
    datasetInput <- eventReactive(input$update, {
      switch(input$dataset,
             "製程能力預警" =CpkHisWarning(warning_all, type, spec),
             "良率預警" = YieldHisWarning(warning_all, type, spec),
             "目標值預警" = TargetHisWarning(warning_all, spec))
    }, ignoreNULL = FALSE)
    output$warning_his <- DT::renderDataTable({
      DT::datatable(datasetInput(),options = list(pageLength=10), rownames=FALSE)
    })
  })
  output$update_log <- DT::renderDataTable({
    update_log
  })
  
  
  })




