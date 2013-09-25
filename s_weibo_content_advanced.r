library(RCurl)
library(XML)
library(RJSONIO)
library(digest)

.cntoUTF8 <- function(strcn) {
  OUT <- paste(strcn, "Rweibo", sep = "")
  OUT <- gsub("Rweibo$", "", OUT)
  return(OUT)
}

.strextract <- function(string, pattern, invert = FALSE,
                        ignore.case = FALSE, perl = FALSE, useBytes = FALSE) 
{
  expr <- gregexpr(pattern = pattern, text = string, ignore.case = ignore.case, 
                   perl = perl, fixed = FALSE, useBytes = useBytes)
  OUT <- regmatches(x = string, m = expr, invert = invert)
  return(OUT)
}

.fromJSON <- function(json, api = c("rjson", "RJSONIO"), ...) {
  api <- match.arg(api)
  iscontent <- inherits(json, "AsIs") || (!file.exists(json) && length(grep("^[[:space:]]*[[{]", json)))
  
  if (api == "rjson") {
    if (iscontent) {
      OUT <- rjson:::fromJSON(json_str = json)
    } else {
      OUT <- rjson:::fromJSON(file = json)
    }
  }
  
  if (api == "RJSONIO") {
    OUT <- RJSONIO:::fromJSON(json_str = content, ...)
  }
  
  return(OUT)
}

.strtrim <- function(string, side = c("both", "left", "right")) {
  side <- match.arg(side)
  pattern <- switch(side, left = "^\\s+", right = "\\s+$", both = "^\\s+|\\s+$")
  OUT <- gsub(pattern, "", string)
  return(OUT)
}

## 拼凑URL
.assembleURL <- function(search.word,
				 xsort = "",
				 scope = FALSE,
				 atten = FALSE,
				 vip = FALSE,
				 haspic = FALSE,
				 hasvideo = FALSE,
				 hasmusic = FALSE,
				 haslink = FALSE, 
				 userscope = "",
				 begin.timescope = "",
				 end.timescope = "",
				 province.region = "",
				 city.region = "") {
  host <- "http://s.weibo.com/weibo/"
  search.word <- curlEscape(.cntoUTF8(search.word))
  search.word <- curlEscape(.cntoUTF8(search.word))

  ## 排序
  if (!is.na(pmatch(xsort, ""))) xsort <- ""
  XSORT <- c("", "time", "hot")
  xsort <- pmatch(xsort, XSORT)
  if (is.na(xsort)) stop("参数xsort只能是\"default\"、\"time\"或\"hot\"")
  if (xsort == 1) SORT <- "" else SORT <- paste("&xsort=", XSORT[xsort], sep = "")

  ## 类型
  SCOPE <- ifelse(scope, "&scope=ori", "")
  ATTEN <- ifelse(atten, "&atten=1", "")
  VIP <- ifelse(vip, "&vip=1", "")
  PIC <- ifelse(haspic, "&haspic=1", "")
  VIDEO <- ifelse(hasvideo, "&hasvideo=1", "")
  MUSIC <- ifelse(hasmusic, "&hasmusic=1", "")
  LINK <- ifelse(haslink, "&haslink=1", "")
  ## 昵称
  if (nchar(userscope) == 0) USERSCOPE <- "" else {
  userscope <- curlEscape(.cntoUTF8(userscope))
  userscope <- curlEscape(.cntoUTF8(userscope))
  USERSCOPE <- paste("&userscope=custom%253A", userscope, sep = "")
  }
  ## 时间
  if (nchar(begin.timescope) == 0 & nchar(end.timescope) == 0) TIMESCOPE <- "" else 
  TIMESCOPE <- paste("&timescope=custom:", begin.timescope, ":", end.timescope, sep = "")
  ## 地区
  REGION <- ""
  ## 参数汇总
  strurl <- paste(host, search.word, SORT, SCOPE, ATTEN, VIP, PIC, VIDEO, MUSIC, LINK, USERSCOPE, REGION, TIMESCOPE, "&page=", sep = "")    

  return(strurl)
}



sweiboContent <- function(strurl, page = 1, curl = NULL, ...) {
  require(RCurl)
  require(XML)
  require(RJSONIO)
  require(digest)
  strurl <- paste(strurl, page, sep="")
  resXML <- getURL(strurl, curl = curl, .encoding = 'UTF-8')
  resHTMLs <- .strextract(resXML, "<script>.+?</script>")[[1]]
  resHTML <- resHTMLs[grep("\"pid\":\"pl_weibo_feedlist\"", resHTMLs)][1]
  if (is.na(resHTML)) {
    warning("Can not crawl any page now. May be forbidden by Sina temporarily.", call. = FALSE)
    return(NULL)
  }
  
  weibojson <- gsub("\\)</script>$", "", gsub("^.*STK.pageletM.view\\(", "", resHTML))
  weibolist <- .fromJSON(weibojson)
  
  weibopage <- htmlParse(weibolist[["html"]], asText=TRUE, encoding = "UTF-8")
  
  weiboitem.attr <- getNodeSet(weibopage, "//dl[@class='feed_list']")
  weiboitem.con <- getNodeSet(weibopage, "//dd[@class='content']")
  weiboitem.nores <- getNodeSet(weibopage, "//div[@class='pl_noresult']")
  
  if (length(weiboitem.nores) == 0) {
    res.mid <- sapply(weiboitem.attr, function(X) xmlGetAttr(X, "mid"))
    res.con <- sapply(weiboitem.con, FUN = function(X) xmlValue(getNodeSet(X, "p[@node-type='feed_list_content']")[[1]]))  
    res.name <- sapply(weiboitem.con, FUN = function(X) xmlGetAttr(getNodeSet(X, "p[@node-type='feed_list_content']/a")[[1]], "nick-name"))
    res.date <- sapply(weiboitem.con, FUN = function(X) xmlGetAttr(getNodeSet(X, "p/a[@node-type='feed_list_item_date']")[[1]], "title"))
    res.stat <- lapply(weiboitem.con, FUN = function(X) sapply(getNodeSet(X, "p/span/a"), xmlValue))
    res.source <- sapply(weiboitem.con, FUN = function(X) xmlValue(getNodeSet(X, "p[@class='info W_linkb W_textb']/a[last()]")[[1]]))
    res.forward <- sapply(weiboitem.con, FUN = function(X) {
      tmp.node <- getNodeSet(X, "dl/dt[@node-type='feed_list_forwardContent']")
      if (length(tmp.node) == 0) {
        NA
      } else {
        xmlValue(tmp.node[[1]])
      }
    }
    )
    Encoding(res.name) <- "UTF-8"          
    res.con <- .strtrim(res.con)
    res.forward <- .strtrim(res.forward)
    res.date <- strptime(res.date, format = "%Y-%m-%d %H:%M")
    res.stat.f <- as.numeric(gsub("[^0-9]", "", sapply(res.stat, FUN = function(X) X[grep("\u8F6C\u53D1", X)])))
    res.stat.r <- as.numeric(gsub("[^0-9]", "", sapply(res.stat, FUN = function(X) X[grep("\u8BC4\u8BBA", X)])))
    res.stat.f[is.na(res.stat.f)] <- 0
    res.stat.r[is.na(res.stat.r)] <- 0
    
    OUT <- data.frame(MID = res.mid, Author = res.name, Weibo = res.con, Forward = res.forward, Time_Weibo = res.date,
                      Time_Search = Sys.time(), Count_Forward = res.stat.f, Count_Reply = res.stat.r, Source = res.source, 
				stringsAsFactors = FALSE)
    OUT$Weibo <- sapply(seq_along(OUT$Weibo), FUN = function(X) 
      gsub(paste("^ *", OUT$Author[X], "\uFF1A *", sep = ""), "", OUT$Weibo[X]))
  } else {
    OUT <- NULL
  }
  return(OUT)
}



searchWeiboContent <- function(roauth, 
					 sword, 
					 page = 1, 
					 combinewith = NULL, 
					 since = NULL, 
					 sinceID = NULL, 
					 sleepmean = 22, 
					 sleepsd = 1, 
					 xsort = "",
					 scope = FALSE,
					 atten = FALSE,
					 vip = FALSE,
					 haspic = FALSE,
					 hasvideo = FALSE,
					 hasmusic = FALSE,
					 haslink = FALSE, 
					 userscope = "",
					 begin.timescope = "",
					 end.timescope = "",
					 province.region = "",
					 city.region = "")
{
  ### 参数
  #	roauth	Oauth授权
  #	sword		搜索关键词
  #	page		抓取页数
  #	combinewith	
  #	since		
  #	sinceID	
  #	sleepmean	
  #	sleepsd	
  #	xsort		高级搜索排序：""——综合；"time"——时间；"hot"——热门，默认""
  #	scope		类型：是否原创，默认FALSE
  #	atten		类型：是否我关注的，默认FALSE
  #	vip		类型：是否认证用户，默认FALSE
  #	haspic	类型：是否带有图片，默认FALSE
  #	hasvideo	类型：是否带有视频，默认FALSE
  #	hasmusic	类型：是否带有音乐，默认FALSE
  #	haslink	类型：是否带有短链，默认FALSE
  #	userscope	昵称：默认无昵称
  #	begin.timescope	时间：起始时间，默认无
  #	end.timescope	时间：结束时间，默认无
  #	province.region	地区：省份，默认无
  #	city.region		地区：城市，默认无
  require(RCurl)
  require(XML)
  require(RJSONIO)
  require(digest)
  if (length(page) == 1) page = 1:page
  page <- page[page > 0 & page <= 50]
  page <- sort(page)
  # if (length(page) > 25) page <- page[1:25]
  Search <- TRUE
  ipage <- 1
  if (!is.null(combinewith)) {
    if (all(c("MID", "Author", "Weibo", "Forward", "Time_Weibo", "Time_Search", "Count_Forward", "Count_Reply", "Source") %in% names(combinewith))) {
      OUT <- combinewith[, c("MID", "Author", "Weibo", "Forward", "Time_Weibo", "Time_Search", "Count_Forward", "Count_Reply", "Source")]
      maxid <- max(as.numeric(OUT$MID))
    } else {
      OUT <- data.frame(stringsAsFactors = FALSE)
      maxid <- 0
      warning("Ignored 'combinewith' because of wrong format!")
    }
  } else {
    OUT <- data.frame(stringsAsFactors = FALSE)
    maxid <- 0
  }
  
  if (!is.null(sinceID)) {
    maxid <- max(maxid, as.numeric(sinceID))
  }
  
  if (is.null(since)) {
    maxdate <- -Inf
  } else {
    if (inherits(since, "character")) {
      since <- strptime(since, format = "%Y-%m-%d")
      if (is.na(since)) {
        warning("Ignore 'since' because of the wrong format!")
        maxdate <- -Inf
      }
    }
    if (inherits(since, "POSIXlt")) maxdate <- since
  }
  
  while (Search && ipage <= length(page)) {
    Sys.sleep(abs(rnorm(1, sleepmean, sleepsd)))
    strurl <- .assembleURL(sword, xsort, scope, atten, vip, haspic, hasvideo, hasmusic, haslink, 
				userscope, begin.timescope, end.timescope, province.region, city.region)
    cat(strurl ,page[ipage],"\n")
    tmp.search <- try(sweiboContent(strurl, page[ipage], roauth), silent = TRUE)
    ipage <- ipage + 1
    if (is.null(tmp.search)) {
      cat(paste(ipage - 2, " pages was stored!\n", sep = ""))
      Search <- FALSE
    } else if (inherits(tmp.search, "try-error")){
      warning(paste("Error in page ", ipage - 1, sep = ""))
    } else {
      if (min(as.numeric(tmp.search$MID)) <= maxid || min(tmp.search$Time_Weibo) < maxdate) {
        Search <- FALSE
        tmp.search <- tmp.search[as.numeric(tmp.search$MID) > maxid & tmp.search$Time_Weibo >= maxdate, ]
      }
      OUT <- rbind(tmp.search, OUT)
    }
  }
  OUT <- OUT[order(as.numeric(OUT$MID), decreasing = TRUE), ]
  return(OUT)
}

