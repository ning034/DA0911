{
  ###1.基本设置####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
}

{#读入数据
  dt_cat <- fread("data_category.csv",header = T,sep=",")#7588515
  length(unique(dt_cat$brandname))#2104
  gc()
  dt_cat
  vb <-
    fread(
      "variable_code.csv",
      header = T,
      sep = ",",
      encoding = "UTF-8"
    )
  vb
}

{#品类描述-no_brands  no_stores  avg_retailprice   
  dt_cat
  numb <- function(data){length(unique(data))}
  no_brands <- dt_cat[,c(.(num_brand=numb(brandname))),by="retailtypename"]
  no_brands#品类中的品牌数量
  no_brands <- no_brands[order(retailtypename),]#按照名称排序，方便后续对照查看
  no_brands
  #no_brands <- dt_cat[,numb(brandname),by="itemtypename"]
  #no_brands#小分类中的品牌数量
  no_stores <- dt_cat[,c(.(num_store=numb(storecode))),by="retailtypename"]
  no_stores <- no_stores[order(retailtypename),]
  no_stores#那些门店销售这个品类
  #文章中是周平均价格，如果复现，需要怎样汇总到这个层面呢？
  #直接计算品牌的周平均价格，再利用品牌份额加总(但是按照价格弹性理解份额应该是销量比例，那如果用销售额份额有什么影响呢？问：用销量份额还是销售额份额好？)。
  dt_cat$week <- week(dt_cat$saledate)
  gc()
  quantile(dt_cat$price,0.90)#1596 
  cond <- which(dt_cat$price > 1596)
  quantile(dt_cat$price,0.95)#396 #再次删除一些，防止异常值影响
  cond <- which(dt_cat$price > 396)
  dt_cat <- dt_cat[-cond,]#由于异常值，因此这里删除部分价格过高的数据
  gc()
  dt_cat
  avg_price <- dt_cat[ ,c(.(avg_price=mean(amt/qty)),.(qty=sum(qty))),by=c("retailtypename","brandname","week")]
  avg_price <- avg_price %>% group_by(retailtypename,week) %>% mutate(sum_qty = sum(qty))
  avg_price
  avg_price <- arrange(avg_price, retailtypename)
  avg_price$share <- avg_price$qty / avg_price$sum_qty
  avg_price
  sum(avg_price$share)#1272
  avg_price$weight_price <- avg_price$avg_price * avg_price$share
  avg_price <- as.data.table(avg_price)
  avg_price
  avg_retailprice <- avg_price[ ,c(.(week_price= sum(weight_price))),by=c("retailtypename","week")]
  avg_retailprice
  avg_retailprice <- avg_retailprice[,c(.(retail_avgprice=mean(week_price))),by="retailtypename"]
  avg_retailprice <- avg_retailprice[order(retailtypename),]
  avg_retailprice
  
  df <- left_join(no_brands,no_stores,by="retailtypename")
  df <- left_join(df,avg_retailprice,by="retailtypename")
  df
  write.csv(df,file = "D:/D/data/xuhuibusiness/category_avgprice.csv", row.names = F)
  #dt_cat[order(saledate,storecode,poscode,saletime,possalecode),]按时间区分用户
}

