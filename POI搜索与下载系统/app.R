# 完整正确的 app.R - 星图地球版本
library(shiny)
library(DT)
library(leaflet)
library(dplyr)   # 让 bind_rows 可用
source("poi_function.R")

ui <- fluidPage(
  titlePanel("POI搜索与下载工具"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("搜索参数"),
      # +++ 【新增代码开始】+++ 
      # 添加API密钥输入框（passwordInput类型可以隐藏输入内容）
      passwordInput("api_key", "高德地图API密钥:", 
                    value = "", 
                    placeholder = "请输入您的高德地图API密钥"),
      # +++ 【新增代码结束】+++
      textInput("keywords", "关键词:", value = "酒店", 
                placeholder = "例如: 酒店、学校、加油站"),
      textInput("city", "城市:", value = "北京市", 
                placeholder = "例如: 北京市、上海市"),
      actionButton("search_btn", "开始搜索", class = "btn-primary"),
      br(), br(),
      downloadButton("download_btn", "下载CSV文件", class = "btn-success")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("数据表格",
                 verbatimTextOutput("status_text"),
                 DTOutput("poi_table")
        ),
        tabPanel("地图显示",
                 leafletOutput("map", height = "600px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  poi_data <- reactiveVal(NULL)
  
  output$status_text <- renderText({"请输入参数并点击搜索"})
  
  observeEvent(input$search_btn, {
    # +++ 【修改1：输入验证，增加API密钥检查】+++
    if (input$api_key == "" || input$keywords == "" || input$city == "") {
      output$status_text <- renderText({"错误：请填写API密钥、关键词和城市！"})
      return()
    }
    
    output$status_text <- renderText({"正在搜索，请稍候..."})
    
    result <- tryCatch({
      # +++ 【修改2：使用用户输入的密钥，删除硬编码的密钥】+++
      # 按细分类别循环搜索，每类最多 200 条
      types <- c("商务酒店", "度假酒店", "快捷酒店", "星级酒店", "民宿", "招待所")
      data  <- bind_rows(lapply(types, function(kw)
        get_poi_data(kw, input$city, input$api_key)))
      data
    }, error = function(e) {
      return(paste("错误:", e$message))
    })
    
    if (is.character(result)) {
      output$status_text <- renderText({result})
      poi_data(NULL)
    } else {
      output$status_text <- renderText({paste("完成！共", nrow(result), "条数据")})
      poi_data(result)
    }
  })
  
  output$poi_table <- renderDT({
    req(poi_data())
    datatable(poi_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$map <- renderLeaflet({
    req(poi_data())
    data <- poi_data()
    data$经度 <- as.numeric(data$经度)
    data$纬度 <- as.numeric(data$纬度)
    
    # 使用星图地球地图源
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%   # 高速稳定底图
      addMarkers(~经度, ~纬度, popup = ~paste("<b>", 名称, "</b><br>", 地址))
  })
  
  output$download_btn <- downloadHandler(
    filename = function() {
      paste0(input$keywords, "_", input$city, "_POI数据.csv")
    },
    content = function(file) {
      req(poi_data())
      write.csv(poi_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui = ui, server = server)