get_poi_data <- function(keywords, city, api_key, max_pages = 50) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  
  all_pois <- list()
  page <- 1
  cat("开始获取数据...\n")
  
  repeat {
    # 1. 构造请求URL（offset=50 单页50条）
    url <- sprintf(
      "https://restapi.amap.com/v3/place/text?key=%s&keywords=%s&city=%s&offset=50&page=%d&extensions=all",
      api_key, keywords, city, page
    )
    
    # 2. 发送GET请求
    res <- tryCatch(GET(url), error = function(e) NULL)
    if (is.null(res)) stop("网络请求失败，请检查网络连接。")
    
    # 3. 解析JSON
    dat <- fromJSON(content(res, "text", encoding = "UTF-8"))
    if (dat$status != "1") stop(paste("API错误:", dat$info))
    
    # 4. 获取当前页POI
    pois <- dat$pois
    if (length(pois) == 0) break          # 没数据就退出
    
    # 5. 安全提取字段
    df <- data.frame(
      名称 = sapply(pois$name,        function(x) ifelse(is.null(x), NA, x)),
      类型 = sapply(pois$type,        function(x) ifelse(is.null(x), NA, x)),
      地址 = sapply(pois$address,     function(x) ifelse(is.null(x), NA, x)),
      电话 = sapply(pois$tel,         function(x) ifelse(is.null(x), NA, x)),
      经度 = as.numeric(sapply(strsplit(pois$location, ","), function(x) x[1])),
      纬度 = as.numeric(sapply(strsplit(pois$location, ","), function(x) x[2])),
      stringsAsFactors = FALSE
    )
    
    all_pois[[page]] <- df
    cat("第", page, "页数据获取成功（", nrow(df), "条）。\n")
    
    # 6. 最后一页不足50条说明抓完
    if (nrow(df) < 50) break
    page <- page + 1
    if (page > max_pages) break        # 安全上限
  }
  
  final_df <- bind_rows(all_pois)
  cat("全部获取完成！共", nrow(final_df), "条记录。\n")
  return(final_df)
}