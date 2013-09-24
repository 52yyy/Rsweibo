Rsweibo
=======

Weibo Search in R

Rsweibo包主要处理新浪微博站内搜索和高级搜索的数据抓取问题。

新浪微博搜索主页：http://s.weibo.com/

代码整合了@lijian 老师的Rweibo包和@波波头一头 老师的weiBor包相关函数，并对新浪微博高级搜索功能进行了扩展。



Rsweibo主要文件：

(a) sweibo_timescope.r

提供新浪微博高级搜索中按时间搜索功能，包括f_weibo_login()和searchWeiboContent()两个主要函数。

Example:

channel <- f_weibo_login(name="username",pwd="password")  # 输入自己的用户名和密码

res <- searchWeiboContent(channel, sword = "沈浩老师", page=7,
	begin.time = "2013-01-01-0", end.time = "2013-01-31-23")  # 搜索"沈浩老师"关键词在2013年1月份的微博结果

nrow(res)  # 返回搜索结果条数

(b) s_weibo_content_advanced.r

提供新浪微博高级搜索地区以外全部功能，主要函数为searchWeiboContent()。

  ### 参数
  
  #	roauth		Oauth授权

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

  #	haspic		类型：是否带有图片，默认FALSE

  #	hasvideo	类型：是否带有视频，默认FALSE

  #	hasmusic	类型：是否带有音乐，默认FALSE

  #	haslink		类型：是否带有短链，默认FALSE

  #	userscope	昵称：默认无昵称

  #	begin.timescope	时间：起始时间，默认无

  #	end.timescope	时间：结束时间，默认无

  #	province.region	地区：省份，默认无

  #	city.region	地区：城市，默认无
  
Example：

channel <- f_weibo_login(name="username",pwd="password")  # 输入自己的用户名和密码

res <- searchWeiboContent(channel, sword = "沈浩老师", page=7, xsort = "time"
	haslink = TRUE, scope = TRUE, sleepmean = 20)  # 按时间顺序搜索"沈浩老师"关键词含有短链的原创微博结果，返回前7页内容

nrow(res)  # 返回搜索结果条数