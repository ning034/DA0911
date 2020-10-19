{
  ###1.基本设置####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(papeR)
  library(cowplot)
  #dt2016<-fread("data2016_price.csv",header = T,sep=",")#10186302
  
  #关键变量是什么？
  #(1)日期、销量、销售额、折扣额、价格，但是价格不适宜加总
  #(2)分组信息：店铺、品类、品牌，但品牌数量太多
  #(3)时间粒度也许是可以创新的点
}


{#销量、销售额、折扣额####
  
  #去除销量异常值
  dt2016
  summary(dt2016$qty)
  quantile(dt2016$qty,0.999)
  dt2016 <- subset(dt2016,qty <= 30)
  gc()
  dt2016
  
  #去除销售额异常值
  quantile(dt2016$amt,0.99)
  dt2016 <- subset(dt2016,amt <= 7960)
  gc()
  dt2016
  
  #销量-月-季度-生成
  qty <- dt2016[,c(.(q=sum(qty)),.(amt=sum(amt)),.(discount=sum(posdiscountamt)+sum(vipdiscountamt)+sum(discountoutamt)+sum(discountinamt)+sum(merchantdiscountamt))),by="saledate"]
  qty$saledate <- as.Date(qty$saledate)
  qty$week2 <- weekdays(qty$saledate)
  qty$week <- week(qty$saledate)
  qty$month <- month(qty$saledate)
  qty$quarter <- quarter(qty$saledate)
  qty
  
  #2016年周销量变化：5-6周是销量高峰，因为当时是春节前后的阶段；第53周销量低是因为，53周只有3天；
  #2016年周销售额变化：37-52周的周销售额远高于20-36周；因此，9-12月或第四季度销售额也较高
  #2016年周/月/季度折扣额变化：与销售额变化的模式比较类似
  qty_w <- qty[,c(.(q=sum(q)),.(amt=sum(amt)),.(discount=sum(discount))),by="week"]
  qty_w$q <- qty_w$q / 10000 #万
  qty_w$amt <- qty_w$amt / 10000000 #千万
  qty_w$discount <- qty_w$discount / 10000000 #千万
  qty_w
  p1 <- ggplot(qty_w,aes(as.factor(week),q,group=1))+geom_line()+geom_point()+labs(x="周数",y="销量(万)")+theme_bw()
  p2 <- ggplot(qty_w,aes(as.factor(week),amt,group=1))+geom_line()+geom_point()+labs(x="周数",y="销售额(千万元)")+theme_bw()
  p3 <- ggplot(qty_w,aes(as.factor(week),discount,group=1))+geom_line()+geom_point()+labs(x="周数",y="折扣额(千万元)")+theme_bw()
  plot_grid(p1,p2,p3,ncol = 1)
  
  #2016年月度销量变化：3-6月份、9-12月份是销量低谷
  qty_m <- qty[,c(.(q=sum(q)),.(amt=sum(amt)),.(discount=sum(discount))),by="month"]
  qty_m$q <- qty_m$q / 10000
  qty_m$amt <- qty_m$amt / 100000000 #亿元
  qty_m$discount <- qty_m$discount / 100000000 #亿元
  qty_m
  p1 <- ggplot(qty_m,aes(as.factor(month),q,group=1))+geom_line()+geom_point()+labs(x="月份",y="销量(万)")+theme_bw()
  p2 <- ggplot(qty_m,aes(as.factor(month),amt,group=1))+geom_line()+geom_point()+labs(x="月份",y="销售额(亿元)")+theme_bw()
  p3 <- ggplot(qty_m,aes(as.factor(month),discount,group=1))+geom_line()+geom_point()+labs(x="月份",y="折扣额(亿元)")+theme_bw()
  plot_grid(p1,p2,p3,ncol = 1)
  
  #2016年季度销量变化:二四季度是销量低估
  qty_q <- qty[,c(.(q=sum(q)),.(amt=sum(amt)),.(discount=sum(discount))),by="quarter"]
  qty_q$q <- qty_q$q / 10000
  qty_q$amt <- qty_q$amt / 100000000 #亿元
  qty_q$discount <- qty_q$discount / 100000000 #亿元
  qty_q
  p1 <- ggplot(qty_q,aes(quarter,q))+geom_line()+ geom_point()+labs(x="季度",y="销量(万)")+theme_bw()
  p2 <- ggplot(qty_q,aes(quarter,amt))+geom_line()+ geom_point()+labs(x="季度",y="销售额(亿元)")+theme_bw()
  p3 <- ggplot(qty_q,aes(quarter,discount))+geom_line()+ geom_point()+labs(x="季度",y="折扣额(亿元)")+theme_bw()
  plot_grid(p1,p2,p3,ncol = 1)
}