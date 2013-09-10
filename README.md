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

待续。。。