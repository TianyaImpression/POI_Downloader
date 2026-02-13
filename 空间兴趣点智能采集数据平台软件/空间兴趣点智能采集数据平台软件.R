library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(data.table)
library(jsonlite)
library(httr)
library(geosphere)
library(leaflet)
library(leaflet.extras)
library(DT)
library(plotly)
library(bsicons)
library(uuid)
library(leaflet.providers)
library(shinyWidgets)
library(ggplot2)
library(forcats)
library(purrr)
library(leafem)

## ---------------- 工具函数 ----------------
## GCJ02→WGS84坐标转换（增强版）
gcj02_to_wgs84 <- function(lon, lat) {
  # 输入验证
  if (any(is.na(lon)) || any(is.na(lat))) {
    warning("输入坐标包含NA值")
    return(list(lon = lon, lat = lat))
  }
  
  a <- 6378245.0
  ee <- 0.00669342162296594323
  pi_val <- pi
  
  # 过滤无效坐标
  valid_idx <- !is.na(lon) & !is.na(lat) & 
    lon >= 73 & lon <= 135 & 
    lat >= 18 & lat <= 54
  
  if (sum(valid_idx) == 0) {
    return(list(lon = lon, lat = lat))
  }
  
  x <- lon[valid_idx] - 105
  y <- lat[valid_idx] - 35
  
  dlat <- -100 + 2*x + 3*y + 0.2*y^2 + 0.1*x*y + 0.2*sqrt(abs(x)) +
    (20*sin(pi_val*y*6) + 20*sin(pi_val*y*2))*2/3 +
    (20*sin(pi_val*y) + 40*sin(pi_val*y/3))*2/3 +
    (160*sin(pi_val*y*12) + 320*sin(pi_val*y*30))*2/3
  
  dlng <- 300 + x + 2*y + 0.1*x^2 + 0.1*x*y + 0.2*sqrt(abs(x)) +
    (20*sin(pi_val*x*6) + 20*sin(pi_val*x*2))*2/3 +
    (20*sin(pi_val*x) + 40*sin(pi_val*x/3))*2/3 +
    (150*sin(pi_val*x*12) + 300*sin(pi_val*x*30))*2/3
  
  radlat <- lat[valid_idx]/180*pi_val
  magic <- sin(radlat)
  magic <- 1 - ee*magic^2
  sqrtmagic <- sqrt(magic)
  
  dlat <- (dlat*180)/((a*(1-ee))/(magic*sqrtmagic)*pi_val)
  dlng <- (dlng*180)/(a/sqrtmagic*cos(radlat)*pi_val)
  
  lon[valid_idx] <- lon[valid_idx] - dlng
  lat[valid_idx] <- lat[valid_idx] - dlat
  
  list(lon = lon, lat = lat)
}

## WGS84→GCJ02
wgs84_to_gcj02 <- function(lon, lat) {
  a <- 6378245.0; ee <- 0.00669342162296594323; pi <- pi
  x <- lon - 105; y = lat - 35
  dlat <- -100 + 2*x + 3*y + 0.2*y^2 + 0.1*x*y + 0.2*sqrt(abs(x)) +
    (20*sin(pi*y*6) + 20*sin(pi*y*2))*2/3 +
    (20*sin(pi*y) + 40*sin(pi*y/3))*2/3 +
    (160*sin(pi*y*12) + 320*sin(pi*y*30))*2/3
  dlng <- 300 + x + 2*y + 0.1*x^2 + 0.1*x*y + 0.2*sqrt(abs(x)) +
    (20*sin(pi*x*6) + 20*sin(pi*x*2))*2/3 +
    (20*sin(pi*x) + 40*sin(pi*x/3))*2/3 +
    (150*sin(pi*x*12) + 300*sin(pi*x*30))*2/3
  radlat <- lat/180*pi; magic <- sin(radlat); magic <- 1 - ee*magic^2
  sqrtmagic <- sqrt(magic)
  dlat <- (dlat*180)/((a*(1-ee))/(magic*sqrtmagic)*pi)
  dlng <- (dlng*180)/(a/sqrtmagic*cos(radlat)*pi)
  list(lon = lon + dlng, lat = lat + dlat)
}

## 坐标转换函数
convert_coords <- function(df, from_crs, to_crs) {
  if (from_crs == to_crs) return(df)
  
  if (from_crs == "gcj02" && to_crs == "wgs84") {
    coords <- st_coordinates(df)
    converted <- gcj02_to_wgs84(coords[,1], coords[,2])
    st_geometry(df) <- st_as_sf(data.frame(lon = converted$lon, lat = converted$lat), 
                                coords = c("lon", "lat"), crs = 4326)$geometry
  } else if (from_crs == "wgs84" && to_crs == "gcj02") {
    coords <- st_coordinates(df)
    converted <- wgs84_to_gcj02(coords[,1], coords[,2])
    st_geometry(df) <- st_as_sf(data.frame(lon = converted$lon, lat = converted$lat), 
                                coords = c("lon", "lat"), crs = 4326)$geometry
  }
  return(df)
}

## 获取省份的城市列表
get_province_cities <- function(province_name) {
  # 这里可以根据需要实现真实的城市数据获取
  # 目前使用预设的主要城市
  city_mapping <- list(
    "北京" = c("北京"),
    "天津" = c("天津"),
    "河北" = c("石家庄", "唐山", "秦皇岛", "邯郸", "邢台", "保定", "张家口", "承德", "沧州", "廊坊", "衡水"),
    "山西" = c("太原", "大同", "阳泉", "长治", "晋城", "朔州", "晋中", "运城", "忻州", "临汾", "吕梁"),
    "内蒙古" = c("呼和浩特", "包头", "乌海", "赤峰", "通辽", "鄂尔多斯", "呼伦贝尔", "巴彦淖尔", "乌兰察布"),
    "辽宁" = c("沈阳", "大连", "鞍山", "抚顺", "本溪", "丹东", "锦州", "营口", "阜新", "辽阳", "盘锦", "铁岭", "朝阳", "葫芦岛"),
    "吉林" = c("长春", "吉林", "四平", "辽源", "通化", "白山", "松原", "白城", "延边"),
    "黑龙江" = c("哈尔滨", "齐齐哈尔", "鸡西", "鹤岗", "双鸭山", "大庆", "伊春", "佳木斯", "七台河", "牡丹江", "黑河", "绥化"),
    "上海" = c("上海"),
    "江苏" = c("南京", "无锡", "徐州", "常州", "苏州", "南通", "连云港", "淮安", "盐城", "扬州", "镇江", "泰州", "宿迁"),
    "浙江" = c("杭州", "宁波", "温州", "嘉兴", "湖州", "绍兴", "金华", "衢州", "舟山", "台州", "丽水"),
    "安徽" = c("合肥", "芜湖", "蚌埠", "淮南", "马鞍山", "淮北", "铜陵", "安庆", "黄山", "滁州", "阜阳", "宿州", "六安", "亳州", "池州", "宣城"),
    "福建" = c("福州", "厦门", "莆田", "三明", "泉州", "漳州", "南平", "龙岩", "宁德"),
    "江西" = c("南昌", "景德镇", "萍乡", "九江", "新余", "鹰潭", "赣州", "吉安", "宜春", "抚州", "上饶"),
    "山东" = c("济南", "青岛", "淄博", "枣庄", "东营", "烟台", "潍坊", "济宁", "泰安", "威海", "日照", "临沂", "德州", "聊城", "滨州", "菏泽"),
    "河南" = c("郑州", "开封", "洛阳", "平顶山", "安阳", "鹤壁", "新乡", "焦作", "濮阳", "许昌", "漯河", "三门峡", "南阳", "商丘", "信阳", "周口", "驻马店"),
    "湖北" = c("武汉", "黄石", "十堰", "宜昌", "襄阳", "鄂州", "荆门", "孝感", "荆州", "黄冈", "咸宁", "随州", "恩施"),
    "湖南" = c("长沙", "株洲", "湘潭", "衡阳", "邵阳", "岳阳", "常德", "张家界", "益阳", "郴州", "永州", "怀化", "娄底", "湘西"),
    "广东" = c("广州", "韶关", "深圳", "珠海", "汕头", "佛山", "江门", "湛江", "茂名", "肇庆", "惠州", "梅州", "汕尾", "河源", "阳江", "清远", "东莞", "中山", "潮州", "揭阳", "云浮"),
    "广西" = c("南宁", "柳州", "桂林", "梧州", "北海", "防城港", "钦州", "贵港", "玉林", "百色", "贺州", "河池", "来宾", "崇左"),
    "海南" = c("海口", "三亚", "三沙", "儋州"),
    "重庆" = c("重庆"),
    "四川" = c("成都", "自贡", "攀枝花", "泸州", "德阳", "绵阳", "广元", "遂宁", "内江", "乐山", "南充", "眉山", "宜宾", "广安", "达州", "雅安", "巴中", "资阳", "阿坝", "甘孜", "凉山"),
    "贵州" = c("贵阳", "六盘水", "遵义", "安顺", "毕节", "铜仁", "黔西南", "黔东南", "黔南"),
    "云南" = c("昆明", "曲靖", "玉溪", "保山", "昭通", "丽江", "普洱", "临沧", "楚雄", "红河", "文山", "西双版纳", "大理", "德宏", "怒江", "迪庆"),
    "西藏" = c("拉萨", "日喀则", "昌都", "林芝", "山南", "那曲", "阿里"),
    "陕西" = c("西安", "铜川", "宝鸡", "咸阳", "渭南", "延安", "汉中", "榆林", "安康", "商洛"),
    "甘肃" = c("兰州", "嘉峪关", "金昌", "白银", "天水", "武威", "张掖", "平凉", "酒泉", "庆阳", "定西", "陇南", "临夏", "甘南"),
    "青海" = c("西宁", "海东", "海北", "黄南", "海南", "果洛", "玉树", "海西"),
    "宁夏" = c("银川", "石嘴山", "吴忠", "固原", "中卫"),
    "新疆" = c("乌鲁木齐", "克拉玛依", "吐鲁番", "哈密", "昌吉", "博尔塔拉", "巴音郭楞", "阿克苏", "克孜勒苏", "喀什", "和田", "伊犁", "塔城", "阿勒泰"),
    "台湾" = c("台北", "高雄", "基隆", "台中", "台南", "新竹", "嘉义"),
    "香港" = c("香港"),
    "澳门" = c("澳门")
  )
  
  if (province_name %in% names(city_mapping)) {
    return(city_mapping[[province_name]])
  } else {
    return(province_name)  # 如果不在列表中，直接返回省份名
  }
}

## 安全的sf对象创建函数 - 增强版，支持从location字段提取坐标
safe_st_as_sf_from_location <- function(df, location_col = "location", crs = 4326) {
  tryCatch({
    if (!location_col %in% names(df)) {
      stop(paste("数据中缺少location列"))
    }
    
    # 检查location列是否有有效数据
    valid_locations <- !is.na(df[[location_col]]) & df[[location_col]] != ""
    
    if (sum(valid_locations) == 0) {
      warning("没有有效的location数据")
      # 添加空的几何列
      df$geometry <- NA
      return(df)
    }
    
    # 提取坐标
    coords_list <- strsplit(as.character(df[[location_col]][valid_locations]), ",")
    valid_coords <- sapply(coords_list, function(x) {
      if (length(x) == 2) {
        num_x <- suppressWarnings(as.numeric(x[1]))
        num_y <- suppressWarnings(as.numeric(x[2]))
        !is.na(num_x) && !is.na(num_y)
      } else FALSE
    })
    
    if (sum(valid_coords) == 0) {
      warning("location数据格式不正确")
      df$geometry <- NA
      return(df)
    }
    
    # 创建坐标矩阵
    coords_matrix <- do.call(rbind, coords_list[valid_coords])
    coords <- matrix(as.numeric(coords_matrix), ncol = 2)
    
    # 创建有效的数据子集
    valid_df <- df[valid_locations, ][valid_coords, , drop = FALSE]
    
    # 添加坐标列
    valid_df$经度 <- coords[,1]
    valid_df$纬度 <- coords[,2]
    
    # 创建sf对象
    result_sf <- st_as_sf(valid_df, coords = c("经度", "纬度"), crs = crs)
    
    # 处理无效的行
    invalid_df <- df[!valid_locations | (valid_locations & !valid_coords), , drop = FALSE]
    if (nrow(invalid_df) > 0) {
      invalid_df$geometry <- NA
      invalid_df$经度 <- NA_real_
      invalid_df$纬度 <- NA_real_
      result <- rbind(result_sf, st_as_sf(invalid_df, coords = c("经度", "纬度"), crs = crs))
    } else {
      result <- result_sf
    }
    
    return(result)
  }, error = function(e) {
    message("从location创建sf对象失败: ", e$message)
    # 返回原始数据框，但添加几何列
    df$geometry <- NA
    df$经度 <- NA_real_
    df$纬度 <- NA_real_
    return(df)
  })
}

## 超级增强版POI搜索函数 - 大幅提高数据获取量
get_poi_super_enhanced <- function(key, city, keywords, coord = "wgs84", max_results = 10000, search_mode = "city") {
  base <- "https://restapi.amap.com/v3/place/text"
  all_results <- list()
  total_collected <- 0
  page_delays <- c(0.1, 0.2, 0.3, 0.2, 0.1)  # 循环使用不同的延迟时间
  
  if (is.null(key) || key == "") {
    showNotification("高德密钥不能为空", type = "error")
    return(NULL)
  }
  
  if (is.null(keywords) || keywords == "") {
    showNotification("搜索关键词不能为空", type = "error")
    return(NULL)
  }
  
  # 扩展关键词列表，包括同义词和相关词
  expanded_keywords <- expand_search_keywords(keywords)
  message("扩展关键词列表: ", paste(expanded_keywords, collapse = ", "))
  
  # 根据搜索模式确定搜索区域
  search_areas <- c()
  if (search_mode == "province") {
    # 全省搜索模式
    search_areas <- get_province_cities(city)
    message("全省搜索模式，搜索区域: ", paste(search_areas, collapse = ", "))
  } else {
    # 单城市搜索模式
    search_areas <- c(city)
  }
  
  # 对每个搜索区域和关键词进行搜索
  for (search_area in search_areas) {
    for (keyword in expanded_keywords) {
      if (total_collected >= max_results) break
      
      page <- 1
      has_more <- TRUE
      consecutive_empty <- 0  # 连续空页面的计数器
      area_collected <- 0
      max_pages_per_keyword <- 50  # 每个关键词最多搜索50页
      
      while (has_more && total_collected < max_results && page <= max_pages_per_keyword && consecutive_empty < 3) {
        param <- list(
          key = key, 
          keywords = keyword, 
          city = search_area,
          citylimit = "true", 
          offset = 50,  # 每页50条
          page = page,
          output = "json", 
          extensions = "all"
        )
        
        tryCatch({
          r <- httr::GET(base, query = param, timeout(15))
          
          if (r$status_code != 200) {
            consecutive_empty <- consecutive_empty + 1
            page <- page + 1
            Sys.sleep(0.5)
            next
          }
          
          content_text <- content(r, "text", encoding = "UTF-8")
          if (nchar(content_text) == 0) {
            consecutive_empty <- consecutive_empty + 1
            page <- page + 1
            Sys.sleep(0.5)
            next
          }
          
          dat <- jsonlite::fromJSON(content_text)
          
          if (dat$status != "1") {
            consecutive_empty <- consecutive_empty + 1
            page <- page + 1
            Sys.sleep(0.5)
            next
          }
          
          if (length(dat$pois) == 0 || is.null(dat$pois) || nrow(dat$pois) == 0) {
            consecutive_empty <- consecutive_empty + 1
            if (consecutive_empty >= 3) {
              has_more <- FALSE
            }
            page <- page + 1
            Sys.sleep(0.3)
            next
          }
          
          # 重置连续空页面计数器
          consecutive_empty <- 0
          
          pois_df <- dat$pois
          current_count <- nrow(pois_df)
          
          # 处理列表列
          pois_df <- process_list_columns(pois_df)
          
          # 检查location字段的有效性
          valid_locations <- !is.na(pois_df$location) & pois_df$location != ""
          if (sum(valid_locations) == 0) {
            page <- page + 1
            next
          }
          
          # 直接使用location字段创建sf对象
          if (coord == "wgs84") {
            # 先提取GCJ02坐标并转换为WGS84
            coords_list <- strsplit(as.character(pois_df$location[valid_locations]), ",")
            valid_coords <- sapply(coords_list, function(x) {
              if (length(x) == 2) {
                num_x <- suppressWarnings(as.numeric(x[1]))
                num_y <- suppressWarnings(as.numeric(x[2]))
                !is.na(num_x) && !is.na(num_y)
              } else FALSE
            })
            
            if (sum(valid_coords) > 0) {
              coords_matrix <- do.call(rbind, coords_list[valid_coords])
              coords <- matrix(as.numeric(coords_matrix), ncol = 2)
              
              # 转换坐标
              tmp <- gcj02_to_wgs84(coords[, 1], coords[, 2])
              coords <- cbind(tmp$lon, tmp$lat)
              
              # 更新坐标
              updated_locations <- character(length(valid_coords))
              valid_idx_in_subset <- which(valid_coords)
              for (i in seq_along(valid_coords)) {
                if (valid_coords[i]) {
                  updated_locations[i] <- paste(round(coords[valid_idx_in_subset[i], 1], 6), 
                                                round(coords[valid_idx_in_subset[i], 2], 6), sep = ",")
                }
              }
              
              pois_df$location[valid_locations][valid_coords] <- updated_locations
            }
          }
          
          # 使用新的安全函数从location创建sf对象
          result_sf <- safe_st_as_sf_from_location(pois_df, "location", crs = 4326)
          
          # 添加额外信息
          result_sf$POI_ID <- uuid::UUIDgenerate(n = nrow(result_sf))
          result_sf$搜索关键词 <- keyword
          result_sf$搜索区域 <- search_area
          
          # 标准化列名和内容
          result_sf <- result_sf %>%
            mutate(
              名称 = ifelse(!is.na(name) & name != "", name, "未知"),
              地址 = ifelse(!is.na(address) & address != "", address, "未知"),
              行政区域 = ifelse(!is.na(adname) & adname != "", adname, 
                            ifelse(!is.na(cityname) & cityname != "", cityname, "未知")),
              城市 = ifelse(!is.na(cityname) & cityname != "", cityname, "未知"),
              邮政编码 = ifelse(!is.na(postcode) & postcode != "", postcode, "未知"),
              类别 = ifelse(!is.na(type) & type != "", type, "未知"),
              类型 = ifelse(!is.na(typecode) & typecode != "", typecode, "未知"),
              标签 = ifelse(!is.na(tag) & tag != "", tag, "未知")
            )
          
          all_results <- c(all_results, list(result_sf))
          total_collected <- total_collected + nrow(result_sf)
          area_collected <- area_collected + nrow(result_sf)
          
          message(sprintf("搜索进度: 区域=%s, 关键词=%s, 页码=%d, 本页=%d, 累计=%d/%d", 
                          search_area, keyword, page, nrow(result_sf), total_collected, max_results))
          
          # 检查是否还有更多数据
          if (current_count < 50) {
            has_more <- FALSE
          } else {
            page <- page + 1
          }
          
          # 使用循环延迟避免请求过快
          delay_index <- ((page - 1) %% length(page_delays)) + 1
          Sys.sleep(page_delays[delay_index])
          
        }, error = function(e) {
          message("搜索出错: ", e$message)
          consecutive_empty <- consecutive_empty + 1
          page <- page + 1
          Sys.sleep(0.5)
        })
      }
      
      if (total_collected >= max_results) break
      # 关键词间延迟
      Sys.sleep(1)
    }
    
    if (total_collected >= max_results) break
  }
  
  if (length(all_results) == 0) {
    return(list(data = NULL, count = 0))
  }
  
  # 合并所有结果并去重
  combined_df <- bind_rows(all_results)
  
  # 基于名称和坐标去重
  if (nrow(combined_df) > 0 && all(c("名称", "经度", "纬度") %in% names(combined_df))) {
    combined_df <- combined_df %>%
      distinct(名称, 经度, 纬度, .keep_all = TRUE)
  }
  
  final_count <- nrow(combined_df)
  message(sprintf("搜索完成: 总计获取 %d 个POI点", final_count))
  
  return(list(data = combined_df, count = final_count))
}

## 扩展搜索关键词函数
expand_search_keywords <- function(original_keywords) {
  # 分割原始关键词
  keyword_list <- strsplit(original_keywords, "[,，;；]")[[1]]
  keyword_list <- trimws(keyword_list)
  keyword_list <- keyword_list[keyword_list != ""]
  
  # 定义扩展词库
  expansion_dict <- list(
    "餐厅" = c("餐厅", "饭店", "餐馆", "饭馆", "美食", "餐饮", "小吃", "快餐", "正餐", "家常菜", "川菜", "粤菜", "湘菜", "鲁菜", "火锅", "烧烤", "日料", "韩料", "西餐", "自助餐"),
    "酒店" = c("酒店", "宾馆", "旅馆", "客栈", "民宿", "招待所", "住宿", "旅店"),
    "超市" = c("超市", "商场", "购物中心", "百货", "商店", "便利店", "大卖场", "仓储式超市"),
    "银行" = c("银行", "ATM", "自动取款机", "金融机构", "工商银行", "建设银行", "农业银行", "中国银行", "招商银行", "交通银行"),
    "医院" = c("医院", "诊所", "卫生院", "医疗", "卫生服务中心", "三甲医院", "社区医院", "专科医院"),
    "学校" = c("学校", "大学", "中学", "小学", "幼儿园", "学院", "教育", "培训", "辅导班"),
    "加油站" = c("加油站", "加气站", "充电站", "中石油", "中石化", "中海油", "壳牌", "BP"),
    "停车场" = c("停车场", "停车楼", "停车位", "车库", "停车"),
    "咖啡" = c("咖啡", "星巴克", "瑞幸", "costa", "咖啡厅", "咖啡店", "cafe"),
    "麦当劳" = c("麦当劳", "MCDONALD'S", "金拱门"),
    "肯德基" = c("肯德基", "KFC")
  )
  
  # 扩展关键词
  expanded <- c()
  for (kw in keyword_list) {
    if (kw %in% names(expansion_dict)) {
      expanded <- c(expanded, expansion_dict[[kw]])
      message(sprintf("扩展关键词 '%s' -> %d 个相关词", kw, length(expansion_dict[[kw]])))
    } else {
      expanded <- c(expanded, kw)
    }
  }
  
  # 去重并保持顺序
  unique_expanded <- c()
  for (kw in expanded) {
    if (!(kw %in% unique_expanded)) {
      unique_expanded <- c(unique_expanded, kw)
    }
  }
  
  return(unique_expanded)
}

## 处理列表列的函数
process_list_columns <- function(pois_df) {
  n <- nrow(pois_df)
  for (col_name in names(pois_df)) {
    if (is.list(pois_df[[col_name]])) {
      new_col <- character(n)
      for (i in seq_len(n)) {
        if (i <= length(pois_df[[col_name]])) {
          item <- pois_df[[col_name]][[i]]
        } else {
          item <- NULL
        }
        
        if (is.null(item) || length(item) == 0) {
          new_col[i] <- NA_character_
        } else if (is.list(item)) {
          new_col[i] <- tryCatch({
            if (length(item) > 0) jsonlite::toJSON(item, auto_unbox = TRUE) else NA_character_
          }, error = function(e) NA_character_)
        } else if (length(item) > 1) {
          new_col[i] <- paste(item, collapse = ";")
        } else {
          new_col[i] <- as.character(item)
        }
      }
      pois_df[[col_name]] <- new_col
    }
  }
  return(pois_df)
}

## 修复坐标验证函数 - 确保能处理从location提取的坐标
validate_poi_coordinates <- function(poi_data, reference_points = NULL) {
  # 确保输入数据有效
  if (is.null(poi_data) || (is.data.frame(poi_data) && nrow(poi_data) == 0)) {
    return(poi_data)
  }
  
  # 转换为普通数据框
  if (inherits(poi_data, "sf")) {
    df <- st_drop_geometry(poi_data)
  } else if (is.data.frame(poi_data)) {
    df <- as.data.frame(poi_data)
  } else {
    message("无效的输入数据类型")
    return(poi_data)
  }
  
  # 确保数据框有必要的列 - 增强检查，优先使用已有的经纬度列
  if (!all(c("经度", "纬度") %in% names(df))) {
    # 如果没有经纬度列，检查是否有location字段
    if ("location" %in% names(df)) {
      message("检测到location字段，从中提取坐标作为经度和纬度")
      valid_locations <- !is.na(df$location) & df$location != ""
      if (sum(valid_locations) > 0) {
        coords_list <- strsplit(as.character(df$location[valid_locations]), ",")
        valid_coords <- sapply(coords_list, function(x) {
          if (length(x) == 2) {
            num_x <- suppressWarnings(as.numeric(x[1]))
            num_y <- suppressWarnings(as.numeric(x[2]))
            !is.na(num_x) && !is.na(num_y)
          } else FALSE
        })
        
        if (sum(valid_coords) > 0) {
          coords_matrix <- do.call(rbind, coords_list[valid_coords])
          coords <- matrix(as.numeric(coords_matrix), ncol = 2)
          
          df$经度[valid_locations][valid_coords] <- coords[,1]
          df$纬度[valid_locations][valid_coords] <- coords[,2]
          message("成功从location字段提取", sum(valid_coords), "个坐标点")
        }
      }
    }
    
    # 如果还是没有经纬度列，尝试从其他可能的列名中获取
    if (!all(c("经度", "纬度") %in% names(df))) {
      possible_lon_cols <- c("longitude", "lon", "LNG", "x", "coords.x1")
      possible_lat_cols <- c("latitude", "lat", "LAT", "y", "coords.x2")
      
      lon_col <- intersect(possible_lon_cols, names(df))[1]
      lat_col <- intersect(possible_lat_cols, names(df))[1]
      
      if (!is.na(lon_col) && !is.na(lat_col)) {
        names(df)[names(df) == lon_col] <- "经度"
        names(df)[names(df) == lat_col] <- "纬度"
        message("已将列 ", lon_col, " 重命名为 经度，", lat_col, " 重命名为 纬度")
      } else {
        # 如果还是找不到坐标列，检查是否是sf对象的坐标
        if (inherits(poi_data, "sf")) {
          coords <- st_coordinates(poi_data)
          if (ncol(coords) >= 2) {
            df$经度 <- coords[, 1]
            df$纬度 <- coords[, 2]
            message("已从sf对象提取坐标作为经度和纬度")
          } else {
            message("警告：数据中缺少必要的经纬度列且无法提取坐标")
            # 添加空的验证结果列
            df$验证结果 <- "无法验证"
            df$最近参考点 <- "无"
            df$偏差距离 <- NA_real_
            return(df)
          }
        } else {
          message("警告：数据中缺少必要的经纬度列，尝试使用空间坐标")
          # 添加空的验证结果列
          df$验证结果 <- "无法验证"
          df$最近参考点 <- "无"
          df$偏差距离 <- NA_real_
          return(df)
        }
      }
    }
  }
  
  # 确保数据框有验证相关列
  if (!"验证结果" %in% names(df)) {
    df$验证结果 <- "未验证"
  }
  if (!"最近参考点" %in% names(df)) {
    df$最近参考点 <- ""
  }
  if (!"偏差距离" %in% names(df)) {
    df$偏差距离 <- NA_real_
  }
  
  # 只处理有效的坐标点
  valid_coords <- !is.na(df$经度) & !is.na(df$纬度) &
    is.finite(df$经度) & is.finite(df$纬度)
  
  if (sum(valid_coords) == 0) {
    message("警告：没有有效的坐标数据可供验证")
    df$验证结果 <- "无法验证"
    return(df)
  }
  
  # 默认参考点
  if (is.null(reference_points)) {
    reference_points <- data.frame(
      名称 = c("天安门", "故宫博物院", "上海东方明珠", "广州塔", "成都春熙路"),
      经度 = c(116.3974, 116.3970, 121.4997, 113.3233, 104.0805),
      纬度 = c(39.9093, 39.9175, 31.2397, 23.1067, 30.6598),
      城市 = c("北京", "北京", "上海", "广州", "成都"),
      stringsAsFactors = FALSE
    )
  }
  
  # 确保参考点有正确的列名
  if (!all(c("经度", "纬度", "名称") %in% names(reference_points))) {
    message("警告：参考点数据格式不正确")
    df$验证结果 <- "参考点格式错误"
    return(df)
  }
  
  # 对有效坐标进行验证
  for (i in which(valid_coords)) {
    poi <- df[i, ]
    min_distance <- Inf
    closest_ref <- ""
    
    # 比较所有参考点
    for (j in 1:nrow(reference_points)) {
      ref <- reference_points[j, ]
      if (!all(c("经度", "纬度") %in% names(ref))) next
      
      distance <- distHaversine(
        c(poi$经度, poi$纬度),
        c(ref$经度, ref$纬度)
      )
      
      if (distance < min_distance) {
        min_distance <- distance
        closest_ref <- ref$名称
      }
    }
    
    df$偏差距离[i] <- min_distance
    df$最近参考点[i] <- closest_ref
    
    if (min_distance < 1000) {
      df$验证结果[i] <- "高精度"
    } else if (min_distance < 5000) {
      df$验证结果[i] <- "中等精度"
    } else if (min_distance < 20000) {
      df$验证结果[i] <- "低精度"
    } else {
      df$验证结果[i] <- "需人工验证"
    }
  }
  
  # 对于无效坐标，标记为无法验证
  df$验证结果[!valid_coords] <- "无法验证"
  df$最近参考点[!valid_coords] <- "无有效坐标"
  df$偏差距离[!valid_coords] <- NA_real_
  
  return(df)
}

## 可视化分析函数
create_visualizations <- function(poi_data) {
  if (is.null(poi_data) || (is.data.frame(poi_data) && nrow(poi_data) == 0)) {
    return(NULL)
  }
  
  df <- st_drop_geometry(poi_data)
  
  # 检查必要的列是否存在
  if (!"类别" %in% names(df)) {
    message("警告：数据中缺少类别列，无法生成类别分布图")
    category_plot <- NULL
  } else {
    # 1. 类别分布图
    category_plot <- tryCatch({
      category_counts <- df %>%
        count(类别, sort = TRUE) %>%
        head(15)
      
      if (nrow(category_counts) == 0) {
        NULL
      } else {
        p1 <- ggplot(category_counts, aes(x = reorder(类别, n), y = n)) +
          geom_col(fill = "steelblue", alpha = 0.8) +
          coord_flip() +
          labs(title = "POI类别分布", x = "类别", y = "数量") +
          theme_minimal() +
          theme(axis.text.y = element_text(size = 8))
        
        ggplotly(p1, height = 300)
      }
    }, error = function(e) {
      message("类别图生成失败:", e$message)
      NULL
    })
  }
  
  if (!"行政区域" %in% names(df)) {
    message("警告：数据中缺少行政区域列，无法生成区域分布图")
    district_plot <- NULL
  } else {
    # 2. 区域分布图
    district_plot <- tryCatch({
      district_counts <- df %>%
        count(行政区域, sort = TRUE) %>%
        head(10)
      
      if (nrow(district_counts) == 0) {
        NULL
      } else {
        p2 <- ggplot(district_counts, aes(x = reorder(行政区域, n), y = n)) +
          geom_col(fill = "coral", alpha = 0.8) +
          labs(title = "行政区域分布", x = "行政区域", y = "数量") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(p2, height = 300)
      }
    }, error = function(e) {
      message("区域图生成失败:", e$message)
      NULL
    })
  }
  
  return(list(
    category_plot = category_plot,
    district_plot = district_plot
  ))
}

## 数据导出函数
export_poi_data <- function(poi_data, format = "csv", file_path) {
  if (is.null(poi_data) || (is.data.frame(poi_data) && nrow(poi_data) == 0)) {
    return(FALSE)
  }
  
  tryCatch({
    if (format == "csv") {
      # 导出为CSV
      df <- st_drop_geometry(poi_data)
      write.csv(df, file_path, row.names = FALSE, fileEncoding = "UTF-8")
      
    } else if (format == "gpkg") {
      # 导出为GeoPackage
      if (inherits(poi_data, "sf")) {
        st_write(poi_data, file_path, layer = "poi_data", delete_layer = TRUE)
      } else {
        showNotification("数据不是空间格式，无法导出为GeoPackage", type = "warning")
        return(FALSE)
      }
      
    } else if (format == "shp") {
      # 导出为Shapefile
      if (inherits(poi_data, "sf")) {
        st_write(poi_data, file_path, layer = "poi_data", delete_layer = TRUE, delete_dsn = TRUE)
      } else {
        showNotification("数据不是空间格式，无法导出为Shapefile", type = "warning")
        return(FALSE)
      }
      
    } else if (format == "geojson") {
      # 导出为GeoJSON
      if (inherits(poi_data, "sf")) {
        st_write(poi_data, file_path, layer = "poi_data", delete_layer = TRUE)
      } else {
        showNotification("数据不是空间格式，无法导出为GeoJSON", type = "warning")
        return(FALSE)
      }
      
    } else if (format == "kml") {
      # 导出为KML
      if (inherits(poi_data, "sf")) {
        st_write(poi_data, file_path, layer = "poi_data", delete_layer = TRUE)
      } else {
        showNotification("数据不是空间格式，无法导出为KML", type = "warning")
        return(FALSE)
      }
      
    } else if (format == "rds") {
      # 导出为RDS
      saveRDS(poi_data, file_path)
    } else {
      showNotification("不支持的导出格式", type = "error")
      return(FALSE)
    }
    
    return(TRUE)
  }, error = function(e) {
    message("导出失败: ", e$message)
    return(FALSE)
  })
}

## 省份城市数据
province_city_data <- data.frame(
  省份 = c("北京", "天津", "河北", "山西", "内蒙古", "辽宁", "吉林", "黑龙江",
         "上海", "江苏", "浙江", "安徽", "福建", "江西", "山东",
         "河南", "湖北", "湖南", "广东", "广西", "海南", "重庆",
         "四川", "贵州", "云南", "西藏", "陕西", "甘肃", "青海",
         "宁夏", "新疆", "台湾", "香港", "澳门"),
  省会 = c("北京", "天津", "石家庄", "太原", "呼和浩特", "沈阳", "长春", "哈尔滨",
         "上海", "南京", "杭州", "合肥", "福州", "南昌", "济南",
         "郑州", "武汉", "长沙", "广州", "南宁", "海口", "重庆",
         "成都", "贵阳", "昆明", "拉萨", "西安", "兰州", "西宁",
         "银川", "乌鲁木齐", "台北", "香港", "澳门"),
  stringsAsFactors = FALSE
)

## ---------------- UI界面 ----------------
ui <- navbarPage(
  title = "高德地图POI搜索工具",
  id = "tabs",
  theme = bslib::bs_theme(version = 5, primary = "#0d6efd"),
  
  tabPanel("POI搜索",
           layout_sidebar(
             sidebar = sidebar(
               width = 350,
               tags$h4("搜索参数", style = "margin-top: 0;"),
               
               textInput("key", "高德Web密钥*", 
                         value = "", 
                         placeholder = "请输入您的高德API密钥"),
               actionButton("validate_key", "验证密钥", 
                            class = "btn-info w-100",
                            icon = icon("key")),
               verbatimTextOutput("key_status"),
               
               tags$hr(),
               
               fluidRow(
                 column(6, selectInput("province", "省份", 
                                       choices = c("选择省份", province_city_data$省份),
                                       selected = "北京")),
                 column(6, textInput("city", "城市*", 
                                     value = "北京", 
                                     placeholder = "如：北京"))
               ),
               
               # 新增：搜索模式选择
               radioButtons("search_mode", "搜索模式",
                            choices = c("单城市搜索" = "city", "全省搜索" = "province"),
                            selected = "city",
                            inline = TRUE),
               
               textInput("kw", "关键词*", 
                         value = "餐厅", 
                         placeholder = "如：餐厅、酒店等"),
               
               fluidRow(
                 column(6, numericInput("max_results", "最大数量", 
                                        value = 5000, min = 100, max = 10000, step = 100)),
                 column(6, selectInput("coord", "坐标系", 
                                       choices = c("WGS84" = "wgs84", "GCJ02" = "gcj02"), 
                                       selected = "wgs84"))
               ),
               
               awesomeCheckbox("auto_validate", "自动坐标验证", TRUE),
               
               actionButton("search_poi", "开始搜索", 
                            class = "btn-primary w-100", 
                            icon = icon("search")),
               
               tags$hr(),
               tags$h4("搜索结果"),
               verbatimTextOutput("search_result"),
               verbatimTextOutput("search_stats"),
               
               # 数据导出选项
               conditionalPanel(
                 condition = "output.poi_data_available",
                 tags$hr(),
                 tags$h4("数据导出"),
                 fluidRow(
                   column(6, selectInput("export_format", "格式", 
                                         choices = c("CSV" = "csv", "GeoPackage" = "gpkg", 
                                                     "Shapefile" = "shp", "GeoJSON" = "geojson",
                                                     "KML" = "kml", "RDS" = "rds"),
                                         selected = "csv")),
                   column(6, downloadButton("export_data", "导出数据", 
                                            class = "btn-success w-100"))
                 )
               )
             ),
             
             mainPanel(
               card(
                 card_header("POI分布地图"),
                 leafletOutput("map_poi", height = 600)
               )
             )
           )
  ),
  
  tabPanel("坐标验证",
           card(
             card_header("坐标精度验证工具"),
             fluidRow(
               column(4,
                      tags$h4("参考点管理"),
                      textAreaInput("custom_refs", "自定义参考点（名称,经度,纬度,城市）",
                                    value = "天安门,116.3974,39.9093,北京\n故宫博物院,116.3970,39.9175,北京",
                                    placeholder = "每行一个点，格式：名称,经度,纬度,城市",
                                    rows = 6),
                      actionButton("add_refs", "添加参考点", class = "btn-info"),
                      actionButton("clear_refs", "清空参考点", class = "btn-warning")
               ),
               column(4,
                      tags$h4("验证设置"),
                      numericInput("precision_threshold", "高精度阈值(米)", 
                                   value = 1000, min = 10, max = 10000),
                      radioButtons("search_validation_city", "验证城市搜索模式",
                                   choices = c("全国搜索" = "nationwide",
                                               "特定城市" = "specific"),
                                   selected = "nationwide"),
                      conditionalPanel(
                        condition = "input.search_validation_city == 'specific'",
                        textInput("specific_validation_city", "指定验证城市", 
                                  value = "北京",
                                  placeholder = "如：北京")
                      ),
                      actionButton("validate_poi", "执行验证", class = "btn-success"),
                      tags$hr(),
                      verbatimTextOutput("validation_summary")
               ),
               column(4,
                      tags$h4("验证结果"),
                      plotlyOutput("precision_chart", height = 200),
                      verbatimTextOutput("validation_details")
               )
             ),
             card(
               card_header("验证地图"),
               leafletOutput("validation_map", height = 500)
             )
           )
  ),
  
  tabPanel("数据浏览",
           navset_tab(
             nav_panel("POI数据", 
                       DTOutput("data_table")),
             nav_panel("统计信息",
                       verbatimTextOutput("data_stats"))
           )
  ),
  
  tabPanel("可视化分析",
           card(
             card_header("POI数据可视化分析"),
             fluidRow(
               column(6, plotlyOutput("category_plot", height = 400)),
               column(6, plotlyOutput("district_plot", height = 400))
             ),
             fluidRow(
               column(12,
                      tags$h4("数据分析报告"),
                      verbatimTextOutput("analysis_report")
               )
             )
           )
  ),
  
  tabPanel("使用说明",
           card(
             card_header("使用指南"),
             tags$div(
               style = "padding: 20px;",
               tags$h4("超级增强功能说明"),
               tags$p("• 大幅提升数据获取量：单次搜索可获取数千至数万个POI点"),
               tags$p("• 智能关键词扩展：自动扩展同义词和相关词，提高搜索覆盖率"),
               tags$p("• 多页深度搜索：每个关键词搜索多达50页，确保数据完整性"),
               tags$p("• 智能分页控制：动态调整请求频率，避免API限制"),
               tags$p("• 连续空页检测：自动停止无结果的搜索，提高效率"),
               tags$p("• 示例：搜索'餐厅'可扩展到20+相关词，获取更全面的数据"),
               tags$h4("搜索建议"),
               tags$p("• 关键词建议使用通用词汇如'餐厅'、'酒店'等"),
               tags$p("• 系统会自动扩展相关词汇，提高数据获取量"),
               tags$p("• 建议将最大数量设置为3000-5000以获取充足数据")
             )
           )
  )
)

## ---------------- Server逻辑 ----------------
server <- function(input, output, session) {
  
  # 响应式值
    values <- reactiveValues(
    poi_data = NULL,
    search_result = "等待搜索...",
    search_stats = "",
    key_valid = FALSE,
    key_status = "未验证",
    reference_points = data.frame(
      名称 = c("天安门", "故宫博物院", "上海东方明珠", "广州塔", "成都春熙路"),
      经度 = c(116.3974, 116.3970, 121.4997, 113.3233, 104.0805),
      纬度 = c(39.9093, 39.9175, 31.2397, 23.1067, 30.6598),
      城市 = c("北京", "北京", "上海", "广州", "成都"),
      类型 = "地标",
      stringsAsFactors = FALSE
    ),
    validated_data = NULL,
    visualizations = NULL,
    validation_city = "北京"  # 默认验证城市
  )
  
  # 检查是否有POI数据
  output$poi_data_available <- reactive({
    !is.null(values$poi_data) && 
      (is.data.frame(values$poi_data) && nrow(values$poi_data) > 0 ||
         inherits(values$poi_data, "sf") && nrow(values$poi_data) > 0)
  })
  outputOptions(output, "poi_data_available", suspendWhenHidden = FALSE)
  
  # 观察省份变化更新城市
  observeEvent(input$province, {
    req(input$province)
    
    if (input$province != "选择省份") {
      province <- input$province
      capital <- province_city_data$省会[province_city_data$省份 == province]
      
      if (length(capital) > 0) {
        updateTextInput(session, "city", value = capital)
      }
    }
  })
  
  observeEvent(input$search_validation_city, {
    if (input$search_validation_city == "specific") {
      updateTextInput(session, "specific_validation_city", value = values$validation_city)
    } else {
      values$validation_city <- "全国"  # 仅用于标识
    }
  })
  
  observeEvent(input$specific_validation_city, {
    req(input$specific_validation_city)
    values$validation_city <- input$specific_validation_city
  }, ignoreInit = TRUE)
  
  # API密钥验证
  observeEvent(input$validate_key, {
    req(input$key)
    
    if (nchar(trimws(input$key)) < 6) {
      values$key_status <- "密钥格式错误"
      values$key_valid <- FALSE
      showNotification("密钥格式不正确（至少需要6个字符）", type = "error")
      return()
    }
    
    values$key_status <- "验证中..."
    
    # 使用更可靠的验证端点
    test_url <- "https://restapi.amap.com/v3/place/text"
    test_params <- list(
      key = trimws(input$key), 
      keywords = "天安门",
      city = "北京",
      citylimit = "true",
      output = "json"
    )
    
    tryCatch({
      response <- GET(test_url, query = test_params, timeout(15))
      
      if (response$status_code != 200) {
        stop(paste("HTTP请求失败，状态码:", response$status_code))
      }
      
      content_text <- content(response, "text", encoding = "UTF-8")
      if (nchar(content_text) == 0) {
        stop("返回内容为空")
      }
      
      data <- fromJSON(content_text)
      
      if (!"status" %in% names(data)) {
        stop("API返回格式异常，缺少status字段")
      }
      
      if (data$status == "1") {
        values$key_status <- "✓ 密钥有效"
        values$key_valid <- TRUE
        showNotification("API密钥验证成功！", type = "message")
        message("密钥验证成功，返回信息:", data$info)
      } else {
        error_info <- if ("info" %in% names(data)) data$info else "未知错误"
        values$key_status <- paste("✗ 密钥无效:", error_info)
        values$key_valid <- FALSE
        showNotification(paste("密钥验证失败:", error_info), type = "error")
      }
    }, error = function(e) {
      error_msg <- e$message
      values$key_status <- paste("✗ 网络连接失败:", substr(error_msg, 1, 50))
      values$key_valid <- FALSE
      
      message("密钥验证失败详情:", error_msg)
      showNotification(
        paste("网络连接失败:", error_msg),
        type = "error",
        duration = 8
      )
    })
  })
  
  output$key_status <- renderText({
    values$key_status
  })
  
  # POI搜索 - 使用超级增强版搜索函数
  observeEvent(input$search_poi, {
    req(input$key, input$city, input$kw)
    
    if (!values$key_valid) {
      showNotification("请先验证API密钥有效性", type = "warning")
      return()
    }
    
    if (input$city == "" || input$kw == "") {
      showNotification("请填写城市和关键词", type = "warning")
      return()
    }
    
    # 根据搜索模式调整提示信息
    search_mode_text <- if (input$search_mode == "province") "全省" else "城市"
    showNotification(paste("正在", search_mode_text, "搜索POI数据，这可能需要几分钟..."), type = "message", id = "loading", duration = NULL)
    
    # 使用超级增强版搜索函数，传入搜索模式
    result <- get_poi_super_enhanced(
      key = input$key,
      city = input$city,
      keywords = input$kw,
      coord = input$coord,
      max_results = input$max_results,
      search_mode = input$search_mode
    )
    
    removeNotification("loading")
    
    if (is.null(result$data) || (is.data.frame(result$data) && nrow(result$data) == 0)) {
      showNotification("未找到POI数据或搜索失败", type = "error")
      values$search_result <- "搜索失败: 未找到POI数据"
      values$poi_data <- NULL
      values$validated_data <- NULL
      values$visualizations <- NULL
    } else {
      values$poi_data <- result$data
      search_mode_desc <- if (input$search_mode == "province") "全省搜索" else "城市搜索"
      values$search_result <- paste(search_mode_desc, "成功! 找到", result$count, "个POI")
      
      # 生成统计信息
      df <- st_drop_geometry(result$data)
      cities_found <- if ("城市" %in% names(df)) length(unique(df$城市)) else "未知"
      districts_found <- if ("行政区域" %in% names(df)) length(unique(df$行政区域)) else "未知"
      keywords_used <- if ("搜索关键词" %in% names(df)) paste(unique(df$搜索关键词), collapse = ", ") else "未知"
      
      stats_text <- paste(
        "搜索模式: ", search_mode_desc, "\n",
        "总记录数: ", nrow(df), "\n",
        "涉及城市: ", cities_found, "个\n",
        "涉及区域: ", districts_found, "个\n",
        "使用关键词: ", keywords_used, "\n",
        "数据获取时间: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      
      values$search_stats <- stats_text
      
      # 自动坐标验证
      if (input$auto_validate) {
        validated_df <- validate_poi_coordinates(st_drop_geometry(result$data))
        if (inherits(result$data, "sf")) {
          # 如果原始数据是sf对象，保持sf格式
          if (all(c("经度", "纬度") %in% names(validated_df))) {
            values$validated_data <- safe_st_as_sf_from_location(validated_df, "location", crs = 4326)
          } else {
            values$validated_data <- result$data
          }
        } else {
          values$validated_data <- validated_df
        }
      } else {
        values$validated_data <- result$data
      }
      
      # 生成可视化
      values$visualizations <- create_visualizations(result$data)
      
      showNotification(values$search_result, type = "message", duration = 10)
    }
  })
  
  output$search_result <- renderText({
    values$search_result
  })
  
  output$search_stats <- renderText({
    values$search_stats
  })
  
  # 修复地图渲染函数 - 直接从数据中提取坐标，不依赖sf对象
  output$map_poi <- renderLeaflet({
    tryCatch({
      # 创建基础地图 - 使用高德地图瓦片
      base_map <- leaflet(options = leafletOptions(
        crs = leafletCRS("L.CRS.EPSG3857"),
        minZoom = 3,
        maxZoom = 18
      )) %>%
        addTiles(
          urlTemplate = "https://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}",
          attribution = '© 高德地图',
          options = tileOptions(
            subdomains = c("1", "2", "3", "4"),
            detectRetina = TRUE
          )
        ) %>%
        setView(lng = 116.40, lat = 39.90, zoom = 10) %>%
        addScaleBar(position = "bottomleft")
      
      if (!is.null(values$poi_data) && 
          (is.data.frame(values$poi_data) && nrow(values$poi_data) > 0 ||
           inherits(values$poi_data, "sf") && nrow(values$poi_data) > 0)) {
        
        # 转换为普通数据框
        df <- st_drop_geometry(values$poi_data)
        
        # 确保有经纬度列
        if (!all(c("经度", "纬度") %in% names(df))) {
          # 尝试从location字段提取
          if ("location" %in% names(df)) {
            valid_locations <- !is.na(df$location) & df$location != ""
            if (sum(valid_locations) > 0) {
              coords_list <- strsplit(as.character(df$location[valid_locations]), ",")
              valid_coords <- sapply(coords_list, function(x) {
                if (length(x) == 2) {
                  num_x <- suppressWarnings(as.numeric(x[1]))
                  num_y <- suppressWarnings(as.numeric(x[2]))
                  !is.na(num_x) && !is.na(num_y)
                } else FALSE
              })
              
              if (sum(valid_coords) > 0) {
                coords_matrix <- do.call(rbind, coords_list[valid_coords])
                coords <- matrix(as.numeric(coords_matrix), ncol = 2)
                
                df$经度[valid_locations][valid_coords] <- coords[,1]
                df$纬度[valid_locations][valid_coords] <- coords[,2]
                message("地图渲染：从location字段提取了", sum(valid_coords), "个坐标")
              }
            }
          }
        }
        
        # 过滤有效坐标
        valid_df <- df %>%
          filter(!is.na(经度) & !is.na(纬度) & 
                   is.finite(经度) & is.finite(纬度))
        
        if (nrow(valid_df) == 0) {
          return(base_map %>% addControl("无有效坐标数据", position = "topright"))
        }
        
        # 创建弹出窗口内容
        popups <- paste0(
          "<div style='max-width:250px;'>",
          "<strong>", valid_df$名称, "</strong><br>",
          "<strong>地址:</strong> ", substr(valid_df$地址, 1, 50), "<br>",
          "<strong>城市:</strong> ", valid_df$城市, "<br>",
          "<strong>坐标:</strong> ", round(valid_df$经度, 6), ", ", round(valid_df$纬度, 6),
          "</div>"
        )
        
        # 添加标记到地图
        base_map <- base_map %>%
          addMarkers(
            lng = ~经度,
            lat = ~纬度,
            data = valid_df,
            popup = popups,
            label = ~名称,
            clusterOptions = markerClusterOptions(
              chunkedLoading = TRUE,  # 分块加载大量标记
              maxClusterRadius = 50
            )
          )
        
        # 计算边界并缩放
        lng1 <- min(valid_df$经度, na.rm = TRUE)
        lat1 <- min(valid_df$纬度, na.rm = TRUE)
        lng2 <- max(valid_df$经度, na.rm = TRUE)
        lat2 <- max(valid_df$纬度, na.rm = TRUE)
        
        if (all(is.finite(c(lng1, lat1, lng2, lat2))) && 
            lng1 != lng2 && lat1 != lat2) {
          base_map <- base_map %>% fitBounds(lng1, lat1, lng2, lat2)
        }
        
        base_map <- base_map %>% 
          addControl(paste("成功显示", nrow(valid_df), "个POI点"), position = "topright")
        
      } else {
        base_map <- base_map %>% 
          addControl("请先搜索POI数据", position = "topright")
      }
      
      return(base_map)
    }, error = function(e) {
      message("主地图渲染失败: ", e$message)
      return(leaflet() %>% 
               addTiles(
                 urlTemplate = "https://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}",
                 attribution = '© 高德地图',
                 options = tileOptions(subdomains = c("1", "2", "3", "4"))
               ) %>% 
               setView(116.40, 39.90, 10) %>%
               addControl("地图初始化完成", position = "topright"))
    })
  })
  
  # 修复的验证地图渲染函数 - 直接从数据中提取坐标
  output$validation_map <- renderLeaflet({
    tryCatch({
      # 基础地图配置
      base_map <- leaflet() %>%
        addTiles(
          urlTemplate = "https://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}",
          attribution = '© 高德地图',
          options = tileOptions(subdomains = c("1", "2", "3", "4"))
        ) %>%
        setView(lng = 116.40, lat = 39.90, zoom = 10) %>%
        addScaleBar(position = "bottomleft")
      
      # 检查数据
      data_to_show <- NULL
      if (!is.null(values$validated_data) && 
          (is.data.frame(values$validated_data) && nrow(values$validated_data) > 0 ||
           inherits(values$validated_data, "sf") && nrow(values$validated_data) > 0)) {
        message("验证地图：使用验证数据")
        data_to_show <- values$validated_data
      } else if (!is.null(values$poi_data) && 
                 (is.data.frame(values$poi_data) && nrow(values$poi_data) > 0 ||
                  inherits(values$poi_data, "sf") && nrow(values$poi_data) > 0)) {
        message("验证地图：使用原始POI数据")
        data_to_show <- values$poi_data
      }
      
      if (!is.null(data_to_show)) {
        # 转换为普通数据框
        df <- st_drop_geometry(data_to_show)
        
        # 确保有经纬度列
        if (!all(c("经度", "纬度") %in% names(df))) {
          # 尝试从location字段提取
          if ("location" %in% names(df)) {
            valid_locations <- !is.na(df$location) & df$location != ""
            if (sum(valid_locations) > 0) {
              coords_list <- strsplit(as.character(df$location[valid_locations]), ",")
              valid_coords <- sapply(coords_list, function(x) {
                if (length(x) == 2) {
                  num_x <- suppressWarnings(as.numeric(x[1]))
                  num_y <- suppressWarnings(as.numeric(x[2]))
                  !is.na(num_x) && !is.na(num_y)
                } else FALSE
              })
              
              if (sum(valid_coords) > 0) {
                coords_matrix <- do.call(rbind, coords_list[valid_coords])
                coords <- matrix(as.numeric(coords_matrix), ncol = 2)
                
                df$经度[valid_locations][valid_coords] <- coords[,1]
                df$纬度[valid_locations][valid_coords] <- coords[,2]
                message("验证地图：从location字段提取了", sum(valid_coords), "个坐标")
              }
            }
          }
        }
        
        # 过滤有效坐标
        valid_df <- df %>%
          filter(!is.na(经度) & !is.na(纬度) & 
                   is.finite(经度) & is.finite(纬度))
        
        if (nrow(valid_df) == 0) {
          message("没有有效坐标")
          return(base_map %>% addControl("无有效坐标数据", position = "topright"))
        }
        
        message("验证地图：成功提取", nrow(valid_df), "个有效坐标")
        
        # 设置颜色方案
        accuracy_colors <- c(
          "高精度" = "#28a745",
          "中等精度" = "#ffc107", 
          "低精度" = "#fd7e14",
          "需人工验证" = "#dc3545",
          "未验证" = "#007bff",
          "无法验证" = "#6c757d"
        )
        
        # 确保验证结果字段存在
        if (!"验证结果" %in% names(valid_df)) {
          valid_df$验证结果 <- "无法验证"
        }
        
        # 创建颜色向量
        colors <- sapply(valid_df$验证结果, function(x) {
          if (!is.null(x) && x %in% names(accuracy_colors)) {
            accuracy_colors[x]
          } else {
            "#6c757d"
          }
        })
        
        # 处理偏差距离的警告
        if ("偏差距离" %in% names(valid_df)) {
          valid_df$偏差距离 <- ifelse(is.infinite(valid_df$偏差距离) | is.nan(valid_df$偏差距离), 
                                  NA_real_, valid_df$偏差距离)
        }
        
        # 创建弹出窗口内容
        popups <- paste0(
          "<div style='max-width: 300px;'>",
          "<strong>", valid_df$名称, "</strong><br>",
          "<strong>验证结果:</strong> ", valid_df$验证结果, "<br>",
          if ("偏差距离" %in% names(valid_df)) {
            paste0("<strong>偏差距离:</strong> ", 
                   ifelse(is.na(valid_df$偏差距离), "未知", 
                          paste0(round(valid_df$偏差距离, 1), "米")), "<br>")
          } else { "" },
          "<strong>地址:</strong> ", substr(valid_df$地址, 1, 60), "<br>",
          "<strong>城市:</strong> ", valid_df$城市, "<br>",
          "<strong>坐标:</strong> ", round(valid_df$经度, 6), ", ", round(valid_df$纬度, 6),
          "</div>"
        )
        
        # 添加标记到地图
        base_map <- base_map %>%
          addCircleMarkers(
            lng = ~经度, 
            lat = ~纬度,
            data = valid_df,
            popup = popups,
            label = ~名称,
            radius = 6,
            stroke = FALSE,
            fillOpacity = 0.8,
            color = colors,
            group = "poi_points"
          ) %>%
          addLegend(
            position = "bottomright",
            colors = accuracy_colors,
            labels = names(accuracy_colors),
            title = "坐标精度"
          )
        
        # 安全计算边界并缩放
        lng1 <- min(valid_df$经度, na.rm = TRUE)
        lat1 <- min(valid_df$纬度, na.rm = TRUE)
        lng2 <- max(valid_df$经度, na.rm = TRUE)
        lat2 <- max(valid_df$纬度, na.rm = TRUE)
        
        if (all(is.finite(c(lng1, lat1, lng2, lat2))) && 
            lng1 != lng2 && lat1 != lat2) {
          base_map <- base_map %>% fitBounds(lng1, lat1, lng2, lat2)
        }
        
        # 添加成功提示
        base_map <- base_map %>% 
          addControl(paste("成功显示", nrow(valid_df), "个POI点"), position = "topright")
        
      } else {
        base_map <- base_map %>% 
          addControl("请先搜索POI数据并执行验证", position = "topright")
      }
      
      return(base_map)
    }, error = function(e) {
      message("验证地图渲染失败:", e$message)
      return(leaflet() %>% 
               addTiles() %>% 
               setView(116.40, 39.90, 10) %>%
               addControl("地图初始化完成", position = "topright"))
    })
  })
  
  # 坐标验证功能
  observeEvent(input$validate_poi, {
    req(values$poi_data)
    
    showNotification("正在执行坐标验证...", type = "message", id = "validating")
    
    # 解析自定义参考点
    if (nchar(trimws(input$custom_refs)) > 0) {
      custom_refs <- tryCatch({
        lines <- strsplit(trimws(input$custom_refs), "\n")[[1]]
        refs_list <- lapply(lines, function(line) {
          if (nchar(trimws(line)) > 0) {
            parts <- strsplit(trimws(line), ",")[[1]]
            if (length(parts) >= 4) {
              data.frame(
                名称 = trimws(parts[1]),
                经度 = as.numeric(trimws(parts[2])),
                纬度 = as.numeric(trimws(parts[3])),
                城市 = trimws(parts[4]),
                类型 = "自定义",
                stringsAsFactors = FALSE
              )
            }
          }
        })
        refs_list <- refs_list[!sapply(refs_list, is.null)]
        if (length(refs_list) > 0) do.call(rbind, refs_list) else NULL
      }, error = function(e) {
        showNotification("参考点格式错误", type = "error")
        NULL
      })
      
      if (!is.null(custom_refs) && nrow(custom_refs) > 0) {
        values$reference_points <- unique(rbind(values$reference_points, custom_refs))
        showNotification(paste("成功添加", nrow(custom_refs), "个参考点"), type = "message")
      }
    }
    
    # 执行验证
    validated_df <- validate_poi_coordinates(
      st_drop_geometry(values$poi_data),
      values$reference_points
    )
    
    # 保持sf格式（如果原始数据是sf对象）
    if (inherits(values$poi_data, "sf")) {
      if (all(c("经度", "纬度") %in% names(validated_df))) {
        values$validated_data <- safe_st_as_sf_from_location(validated_df, "location", crs = 4326)
      } else {
        values$validated_data <- values$poi_data
      }
    } else {
      values$validated_data <- validated_df
    }
    
    removeNotification("validating")
    showNotification("坐标验证完成!", type = "message")
  })
  
  # 清空参考点
  observeEvent(input$clear_refs, {
    values$reference_points <- data.frame(
      名称 = c("天安门", "故宫博物院", "上海东方明珠", "广州塔", "成都春熙路"),
      经度 = c(116.3974, 116.3970, 121.4997, 113.3233, 104.0805),
      纬度 = c(39.9093, 39.9175, 31.2397, 23.1067, 30.6598),
      城市 = c("北京", "北京", "上海", "广州", "成都"),
      类型 = "地标",
      stringsAsFactors = FALSE
    )
    updateTextAreaInput(session, "custom_refs", value = "")
    showNotification("参考点已重置", type = "message")
  })
  
  # 验证摘要
  output$validation_summary <- renderText({
    req(values$validated_data)
    
    df <- st_drop_geometry(values$validated_data)
    if (!"验证结果" %in% names(df)) {
      return("验证结果不可用")
    }
    
    summary <- df %>%
      count(验证结果) %>%
      mutate(比例 = round(n/sum(n)*100, 1))
    
    paste(
      "验证完成!\n",
      "高精度: ", ifelse(any(summary$验证结果 == "高精度"), summary$n[summary$验证结果 == "高精度"], 0), 
      " (", ifelse(any(summary$验证结果 == "高精度"), summary$比例[summary$验证结果 == "高精度"], 0), "%)\n",
      "中等精度: ", ifelse(any(summary$验证结果 == "中等精度"), summary$n[summary$验证结果 == "中等精度"], 0), 
      " (", ifelse(any(summary$验证结果 == "中等精度"), summary$比例[summary$验证结果 == "中等精度"], 0), "%)\n",
      "低精度: ", ifelse(any(summary$验证结果 == "低精度"), summary$n[summary$验证结果 == "低精度"], 0), 
      " (", ifelse(any(summary$验证结果 == "低精度"), summary$比例[summary$验证结果 == "低精度"], 0), "%)\n",
      "需人工验证: ", ifelse(any(summary$验证结果 == "需人工验证"), summary$n[summary$验证结果 == "需人工验证"], 0), 
      " (", ifelse(any(summary$验证结果 == "需人工验证"), summary$比例[summary$验证结果 == "需人工验证"], 0), "%)"
    )
  })
  
  # 验证结果图表
  output$precision_chart <- renderPlotly({
    req(values$validated_data)
    
    df <- st_drop_geometry(values$validated_data)
    if (!"验证结果" %in% names(df)) {
      return(plot_ly() %>% layout(title = "无验证数据"))
    }
    
    summary <- df %>% count(验证结果)
    
    plot_ly(summary, labels = ~验证结果, values = ~n, type = "pie",
            marker = list(colors = c("#28a745", "#ffc107", "#fd7e14", "#dc3545", "#007bff"))) %>%
      layout(title = "精度分布", showlegend = TRUE)
  })
  
  # 验证详情
  output$validation_details <- renderText({
    req(values$validated_data)
    
    df <- st_drop_geometry(values$validated_data)
    
    if (!"偏差距离" %in% names(df)) {
      return("验证详情不可用")
    }
    
    # 处理无限值和NA值
    valid_distances <- df$偏差距离[!is.infinite(df$偏差距离) & !is.na(df$偏差距离)]
    
    if (length(valid_distances) == 0) {
      return(paste(
        "验证详情:\n",
        "所有偏差距离都无法计算\n",
        "验证时间: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ))
    }
    
    paste(
      "验证详情:\n",
      "平均偏差距离: ", round(mean(valid_distances, na.rm = TRUE), 1), "米\n",
      "最小偏差距离: ", round(min(valid_distances, na.rm = TRUE), 1), "米\n",
      "最大偏差距离: ", round(max(valid_distances, na.rm = TRUE), 1), "米\n",
      "有效样本数: ", length(valid_distances), "/", nrow(df), "\n",
      "验证时间: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })
  
  # 数据表格
  output$data_table <- renderDT({
    req(values$poi_data)
    
    df <- st_drop_geometry(values$poi_data)
    display_cols <- c("名称", "地址", "城市", "行政区域", "类别", "坐标精度", "数据完整性", "电话", "搜索关键词")
    available_cols <- intersect(display_cols, names(df))
    
    if (length(available_cols) == 0) {
      return(datatable(data.frame(消息 = "无可用数据列"), 
                       options = list(pageLength = 10)))
    }
    
    datatable(df[, available_cols], 
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                autoWidth = TRUE
              ),
              rownames = FALSE)
  })
  
  # 数据统计
  output$data_stats <- renderText({
    req(values$poi_data)
    
    df <- st_drop_geometry(values$poi_data)
    
    paste(
      "数据统计信息:\n",
      "总记录数: ", nrow(df), "\n",
      "城市: ", paste(unique(df$城市), collapse = ", "), "\n",
      "关键词: ", paste(unique(df$搜索关键词), collapse = ", "), "\n",
      "类别数量: ", length(unique(df$类别)), "\n",
      "行政区域: ", paste(unique(df$行政区域), collapse = ", "), "\n",
      "更新时间: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })
  
  # 可视化图表
  output$category_plot <- renderPlotly({
    if (!is.null(values$visualizations) && !is.null(values$visualizations$category_plot)) {
      values$visualizations$category_plot
    } else {
      plot_ly(type = "scatter", mode = "markers") %>%
        layout(
          title = "暂无数据",
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          annotations = list(
            text = "请先搜索POI数据",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
        )
    }
  })
  
  output$district_plot <- renderPlotly({
    if (!is.null(values$visualizations) && !is.null(values$visualizations$district_plot)) {
      values$visualizations$district_plot
    } else {
      plot_ly(type = "scatter", mode = "markers") %>%
        layout(
          title = "暂无数据",
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          annotations = list(
            text = "请先搜索POI数据",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
        )
    }
  })
  
  # 分析报告
  output$analysis_report <- renderText({
    if (!is.null(values$poi_data)) {
      df <- st_drop_geometry(values$poi_data)
      
      paste(
        "数据分析报告:\n",
        "数据概况: 共收集", nrow(df), "个POI点，涵盖", length(unique(df$类别)), "个类别\n",
        "空间分布: 分布在", length(unique(df$行政区域)), "个行政区域\n",
        "涉及城市: ", length(unique(df$城市)), "个\n",
        "主要类别: ", paste(names(sort(table(df$类别), decreasing = TRUE)[1:3]), collapse = ", "), "\n",
        "分析时间: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    } else {
      "暂无数据，请先搜索POI数据"
    }
  })
  
  # 数据导出功能
  output$export_data <- downloadHandler(
    filename = function() {
      format <- input$export_format
      city <- if (!is.null(values$poi_data)) {
        unique(st_drop_geometry(values$poi_data)$城市)[1]
      } else {
        "POI"
      }
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      
      switch(format,
             "csv" = paste0(city, "_POI数据_", timestamp, ".csv"),
             "gpkg" = paste0(city, "_POI数据_", timestamp, ".gpkg"),
             "shp" = paste0(city, "_POI数据_", timestamp, ".zip"),
             "geojson" = paste0(city, "_POI数据_", timestamp, ".geojson"),
             "kml" = paste0(city, "_POI数据_", timestamp, ".kml"),
             "rds" = paste0(city, "_POI数据_", timestamp, ".rds"),
             paste0(city, "_POI数据_", timestamp, ".csv")
      )
    },
    content = function(file) {
      req(values$poi_data)
      
      showNotification("正在导出数据...", type = "message", id = "exporting")
      
      # 根据格式选择导出数据
      if (input$export_format == "shp") {
        # Shapefile需要特殊处理
        temp_dir <- tempdir()
        temp_file <- file.path(temp_dir, "poi_data.shp")
        success <- export_poi_data(values$poi_data, "shp", temp_file)
        
        if (success) {
          # 创建ZIP文件
          shp_files <- list.files(temp_dir, pattern = "poi_data\\..*", full.names = TRUE)
          zip(file, shp_files, flags = "-j")
        } else {
          showNotification("导出失败", type = "error")
        }
      } else {
        success <- export_poi_data(values$poi_data, input$export_format, file)
      }
      
      removeNotification("exporting")
      
      if (success) {
        showNotification(paste("数据导出成功:", basename(file)), type = "message")
      } else {
        showNotification("数据导出失败", type = "error")
      }
    }
  )
}

shinyApp(ui, server)
