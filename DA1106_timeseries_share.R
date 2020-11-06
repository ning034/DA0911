{#基本设置
  ###1.基本设置####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(papeR)
  library(cowplot)
  dt2016<-fread("dt2016.csv",header = T,sep=",")#10175541
  gc()
  
}

{#品类销售额排名
  cat_amt <- dt2016[,c(.(amt=sum(amt)),.(brand_num=length(unique(brandname))),.(sku_num=length(unique(barcode)))),by="retailtypename"]
  cat_amt <- cat_amt[order(brand_num,decreasing = T),]
  cat_amt
}


{#品牌层面的市场份额-与sku的关系-取前三个品类
  
  df <- subset(dt2016,retailtypename %in% cat_amt$retailtypename[1:3])
  df$saledate <- as.Date(df$saledate)
  library(lubridate)
  week(df$saledate[1])
  df$week <- week(df$saledate)
  df$price2 <- df$amt / df$qty
  
  
  gc()
  brand_data <- df[,.(brand_amt=sum(amt),sku_num=uniqueN(barcode),mprice=mean(price2),pos_num=uniqueN(posdiscountamt),pos_sum=mean(posdiscountamt/price),out_num=uniqueN(discountoutamt),out_sum=mean(discountoutamt/price)),by=c("brandname","retailtypename","week")]
  brand_data[,retailamt:=sum(brand_amt),by=c("retailtypename","week")]
  brand_data$share <- 100 * brand_data$brand_amt / brand_data$retailamt
  brand_data
  
  sku <- df[,.(amt=sum(amt),mprice=mean(price2)),by=c("brandname","barcode","week")]
  sku[,sumamt:=sum(amt),by=c("brandname","week")]
  sku$weight <- sku$amt / sku$sumamt
  sku$wprice <- sku$mprice * sku$weight
  sku
  wprice <- sku[,.(wp=sum(wprice)),by=c("brandname","week")]  
  wprice
  
  brand_data <- left_join(brand_data,wprice,by=c("brandname","week"))
  brand_data <- as.data.table(brand_data)
  brand_data
  
  #零食小吃类
  t1 <- subset(brand_data,brandname=="益达")#德芙  益达
  p1 <- ggplot(t1,aes(week,share))+geom_line()+geom_point()
  p2 <- ggplot(t1,aes(week,wp))+geom_line()+geom_point()
  t1 <- subset(brand_data,brandname=="德芙")#德芙  益达
  p3 <- ggplot(t1,aes(week,share))+geom_line()+geom_point()
  p4 <- ggplot(t1,aes(week,wp))+geom_line()+geom_point()
  library(cowplot)
  plot_grid(p1,p3,p2,p4,ncol=2)
  
  #服装类
  t1 <- subset(brand_data,brandname=="adidas"&retailtypename=="女装")#adidas 
  p1 <- ggplot(t1,aes(week,share))+geom_line()+geom_point()
  p2 <- ggplot(t1,aes(week,wp))+geom_line()+geom_point()+labs(y="wprice")
  t1 <- subset(brand_data,brandname=="ochirly"&retailtypename=="女装")#ochirly
  p3 <- ggplot(t1,aes(week,share))+geom_line()+geom_point()
  p4 <- ggplot(t1,aes(week,wp))+geom_line()+geom_point()+labs(y="wprice")
  plot_grid(p1,p3,p2,p4,ncol=2)
  
  
 
}
