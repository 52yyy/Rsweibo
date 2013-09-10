library(RCurl)
library(XML)
library(RJSONIO)
library(digest)

## 微博模拟登录函数(来源：@波波头一头)
f_weibo_login <- function(name='Username',
                          pwd='Password',
                          cookie_file=NULL){
  if(is.null(cookie_file)){
    require(RJSONIO)
    require(RCurl)
    require(digest)
    
    # ID预处理
    name1 <- URLencode(name, reserved=T)
    name2 <- base64(name1)[1]
    
    d <- debugGatherer()
    cH <- getCurlHandle(followlocation=T, verbose=T,
                        debugfunction=d$update,
                        ssl.verifyhost=F, ssl.verifypeer=F,
                        cookiejar='./cookies', cookiefile='./cookies')
    
    # 预登录
    preurl <- paste('http://login.sina.com.cn/sso/prelogin.php?entry=miniblog&callback=sinaSSOController.preloginCallBack&su=',
                    name2, '&client=ssologin.js(v1.3.18)', sep='')
    prelogin <- getURL(preurl, curl=cH)
    preinfo <- fromJSON(gsub('^.*\\((.*)\\).*$','\\1',prelogin))
    servertime <- preinfo$servertime
    pcid <- preinfo$pcid
    nonce <- preinfo$nonce
    # 加密的过程
    pwd1 <- digest(pwd, algo='sha1', seria=F)
    pwd2 <- digest(pwd1, algo='sha1', seria=F)
    pwd3 <- digest(paste(pwd2, servertime, nonce, sep=''), algo='sha1', seria=F)
    pinfo=c(
      'service'='miniblog',
      'client'='ssologin.js(v1.3.18)',
      'entry'='weibo',
      'encoding'='UTF-8',
      'gateway'='1',
      'savestate'='7',
      'from'='',
      'useticket'='1',
      'su'=name2,
      'servertime'=servertime,
      'nonce'=nonce,
      'pwencode'='wsse',
      'sp'=pwd3,
      'vsnf'='1',
      'vsnval'='',
      'pcid'=pcid,
      'url'='http://weibo.com/ajaxlogin.php?framelogin=1&callback=parent.sinaSSOController.feedBackUrlCallBack',
      'returntype'='META',
      'ssosimplelogin'='1',
      'setdomain'='1')
    # 登录
    bkp_ctype <- Sys.getlocale('LC_CTYPE')
    if(bkp_ctype == 'zh_CN.UTF-8'){Sys.setlocale('LC_CTYPE', 'C')}
    x <- try(ttt <- postForm('http://login.sina.com.cn/sso/login.php?client=ssologin.js(v1.3.18)',
                             .params=pinfo, curl=cH, style='post'), silent=T)
    if(class(x) == 'try-error'){cat('no!!!!!!');return(NULL)}
    newurl <- gsub('^.*location.replace\\([\'\"](.+)[\'\"]\\);.*$', '\\1', ttt[1])
    x <- try(x <- getURL(newurl, curl=cH, .encoding='UTF-8'), silent=T)
    Sys.setlocale('LC_CTYPE', bkp_ctype)
    if(class(x) == 'try-error'){cat('no!!!!!!');return(NULL)}
    getCurlInfo(cH)[['cookielist']]
  } else{
    require(RCurl)
    d <- debugGatherer()
    cH <- getCurlHandle(followlocation=T, verbose=T,
                        debugfunction=d$update,
                        ssl.verifyhost=F, ssl.verifypeer=F,
                        cookiejar='./cookies', cookiefile=cookie_file)
  }
  return(cH)
}


## 微博搜索函数(来源：@lijian)
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

## 搜索主函数
sweiboContent <- function(sword, page = 1, curl = NULL, begin.time = NULL, end.time = NULL, ...) {
  requestURL <- "http://s.weibo.com/weibo/"
  sword <- curlEscape(.cntoUTF8(sword))
  begin.time = paste(":", begin.time, sep = "")
  end.time = paste(":", end.time, sep = "")
  strurl <- paste(requestURL, sword, "&xsort=time&timescope=custom", begin.time, end.time, "&page=", page, sep = "") # time sorting 
  
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

## 搜索函数
## 参数
## roauth		模拟登陆对象
## sword		搜索关键词
## page			搜索页数，默认为最大页数50
## combinewith		见@lijian Rweibo包
## since		见@lijian Rweibo包
## sinceID		见@lijian Rweibo包
## sleepmean		翻页等待时间mean，默认为20，若小可能出现验证码，造成无法查询结果
## sleepsd		翻页等待时间sd，默认为1
## begin.time		高级搜索功能，设置起始时间，格式"yyyy-mm-dd-hh"
## end.time		高级搜索功能，设置结束时间，格式"yyyy-mm-dd-hh"
searchWeiboContent <- function(roauth, sword, page = 50, combinewith = NULL, 
                                     since = NULL, sinceID = NULL, sleepmean = 20, sleepsd = 1, 
                                     begin.time = NULL, end.time = NULL, ...) 
{
  if (length(page) == 1) page = 1:page
  page <- page[page > 0 & page <= 50]
  page <- sort(page)
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
    tmp.search <- try(sweiboContent(sword, page[ipage], roauth, begin.time, end.time), silent = TRUE)
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

