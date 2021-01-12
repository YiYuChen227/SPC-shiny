Sys.setlocale(locale = "zh_TW.UTF8")
library(shinydashboard)
library(shiny)
library(shinyWidgets)
tags$script('
            $(document).on("shiny:connected",function(e){
            var Width = window.screen.width;
            Shiny.onInputChange("MonitorWidth",Width);})
            ')

dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "SPC精密成型與大數據分析", titleWidth = 450
    ,tags$li(a("Designed by DALab Solutions x Associates Co., Ltd.",href="https://www.dalabx.com.tw/",
               style ="padding-top:10px;padding-right:2%; padding-bottom:0px;",target="_blank"),
             class = "dropdown"#,img(src = 'dalab.png',title = "Dalab決策分析研究室", height = "20px")
    )
  ),
  dashboardSidebar(
    collapsed =T,
    sidebarMenu(
      h6("Version : v.2.0.3"),
      h6("Update Date : 20180322"),
      h6("Developer : 陳奕妤"),
      h6("Maintainer : 陳奕妤、許智翔"),
      menuItem("SPC", tabName = "dashboard1", icon = icon("area-chart")),
      menuItem("更新項目",tabName = "update",icon = icon("book"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$title('N6(A,N14)_3mil_Flat_Head_Uncoating_C(HT-B)_T5'),
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             height: 100px;
             width: 800px;
             position:fixed;
             top: calc(50% - 50px);
             left: calc(50% - 400px);
             font-size: 250%;
             text-align: center;
             }"))),
    tags$head(tags$style(HTML(
      '.myClass { 
      font-size: 20px;
      line-height: 50px;
      text-align: left;
      font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
      padding: 0 15px;
      overflow: hidden;
      color: white;
      }'))),
    tags$script(HTML('$(document).ready(function() {
                     $("header").find("nav").append(\'<span class="myClass"></span>\');
    })')),
    tags$head(tags$style(HTML(".small-box {height: 280px}"))),
    tabItems(
      tabItem(
        tabName = "dashboard1",
        ###短期圖篩選條件
        boxPlus(
          width = 4,
          title = "資料選取", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          closable = TRUE, 
          tagList(
            div(id = "sProduct",
                selectInput("sProduct", label="選擇產品:", choices = unique(product$Product)))),
          uiOutput("sMold"),
          uiOutput("sGrade"),
          br(),
          ###長期圖篩選條件  
          column(8,
                 selectInput("sTitle1", label="選擇觀測尺寸:", choices ="")
                 ,dateRangeInput("date_long1",label = "選擇日期：",start = Sys.Date()-7,end = Sys.Date()))
        ),
        verbatimTextOutput('info'),
        tags$head(tags$style("#info{
                             font-size: 20px;
                             font-style: :Microsoft JhengHei;}"
        )),
        
        
        ###平躺、翻肚、總良率預警看板
        tags$head(tags$style(HTML(".small-box {height: 150px}"))),
        
        
        hr(),
        ###下排資訊
        fluidRow(
          shinydashboard::tabBox(id="tabset",
                                 width = 10
                                 ,tabPanel("短期圖", uiOutput("short_layout_UI"),value=1)
                                 ,tabPanel("長期圖",uiOutput("long_layout_UI"),value=2)
                                 ,tabPanel("兩倍標準差長期圖",uiOutput("twoStd_layout_UI"),value=3)
                                 ,tabPanel("製程能力折線圖",uiOutput("cpk_layout_UI"), 
                                           uiOutput("cp_layout_UI"),value=4)
                                 ,tabPanel("尺寸良率折線圖",uiOutput("yield_UI"),value=5)
                                 ,tabPanel("總良率折線圖",uiOutput("yieldall_layout_UI"),value=6)
                                 ,tabPanel("維護spec資料表",
                                           fluidPage(
                                             fluidRow(
                                               div(class = "login",
                                                   uiOutput("uiLogin"),
                                                   textOutput("pass"),
                                                   tags$head(tags$style("#pass{color: red;")))
                                             ),
                                             fluidRow(
                                               column(12,
                                                      div(uiOutput("userPanel")),
                                                      hr(),
                                                      div(fluidRow(
                                                        column(4,
                                                               uiOutput('subset_1')),
                                                        column(4,
                                                               uiOutput('subset_2')),
                                                        column(2,
                                                               uiOutput("btn1"),
                                                               uiOutput('btn2'))
                                                      ),
                                                      uiOutput('table'),
                                                      br(),
                                                      hr(),
                                                      uiOutput("input_table"),
                                                      uiOutput("output_table")
                                                      )))))
                                 ,tabPanel('預警歷史',
                                           fluidPage(
                                             sidebarPanel(
                                               selectInput("dataset", "Choose a dataset:",
                                                           choices = c("製程能力預警", "良率預警", "目標值預警")),
                                               actionButton("update", "Update")
                                             ),
                                             mainPanel(DT::dataTableOutput("warning_his")),value=7))
                                 ,tabPanel('量測原始資料',
                                           fluidRow(
                                             column(3,actionButton("send","Search",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                             column(3,actionButton('search',"排除異常資料",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                             column(3,actionButton('save',"儲存修改資料",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                           ),
                                           rHandsontableOutput('measure_table')
                                 )
                                 
          ),
          absolutePanel(
            right = 0,
            fixed = TRUE,
            div(
              style="padding: 4px; border-bottom: 1px solid #CCC; background: #FFFFEE;",
              div(valueBoxOutput("yield_total_current",width = 12)),
              div(valueBoxOutput("yield_flat_current",width = 12)),
              div(valueBoxOutput("yield_flip_current",width = 12)),
              actionButton('cpk_btn',"製程能力預警",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              actionButton('yield_btn',"良率預警",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              actionButton('mean_btn',"目標值預警",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            )
          )
        )
        ),
      tabItem(
        tabName = "update",
        fluidPage(
          DT::dataTableOutput("update_log")
        )
      )
        )
        )
    )