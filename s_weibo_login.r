library(RCurl)
library(XML)
library(RJSONIO)
library(digest)

# ΢����¼�ĺ���
weiboLogin <- function(name='',
                       pwd='',
                       cookie_file=NULL){
  if(is.null(cookie_file)){
    require(RJSONIO)
    require(RCurl)
    require(digest)
    
    # IDԤ����
    name1 <- URLencode(name, reserved=T)
    name2 <- base64(name1)[1]
    
    d <- debugGatherer()
    cH <- getCurlHandle(followlocation=T, verbose=T,
                        debugfunction=d$update,
                        ssl.verifyhost=F, ssl.verifypeer=F,
                        cookiejar='./cookies', cookiefile='./cookies')
    
    # Ԥ��¼
    preurl <- paste('http://login.sina.com.cn/sso/prelogin.php?entry=miniblog&callback=sinaSSOController.preloginCallBack&su=',
                    name2, '&client=ssologin.js(v1.3.18)', sep='')
    prelogin <- getURL(preurl, curl=cH)
    preinfo <- fromJSON(gsub('^.*\\((.*)\\).*$','\\1',prelogin))
    servertime <- preinfo$servertime
    pcid <- preinfo$pcid
    nonce <- preinfo$nonce
    # ���ܵĹ���
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
    # ��¼
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
