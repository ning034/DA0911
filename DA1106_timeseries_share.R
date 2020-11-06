{#��������
  ###1.��������####
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

{#Ʒ�����۶�����
  cat_amt <- dt2016[,c(.(amt=sum(amt)),.(brand_num=length(unique(brandname))),.(sku_num=length(unique(barcode)))),by="retailtypename"]
  cat_amt <- cat_amt[order(brand_num,decreasing = T),]
  cat_amt
}


{#Ʒ�Ʋ�����г��ݶ�-��sku�Ĺ�ϵ-ȡǰ����Ʒ��
  
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
  
  #��ʳС����
  t1 <- subset(brand_data,brandname=="���")#��ܽ  ���
  p1 <- ggplot(t1,aes(week,share))+geom_line()+geom_point()
  p2 <- ggplot(t1,aes(week,wp))+geom_line()+geom_point()
  t1 <- subset(brand_data,brandname=="��ܽ")#��ܽ  ���
  p3 <- ggplot(t1,aes(week,share))+geom_line()+geom_point()
  p4 <- ggplot(t1,aes(week,wp))+geom_line()+geom_point()
  library(cowplot)
  plot_grid(p1,p3,p2,p4,ncol=2)
  
  #��װ��
  t1 <- subset(brand_data,brandname=="adidas"&retailtypename=="Ůװ")#adidas 
  p1 <- ggplot(t1,aes(week,share))+geom_line()+geom_point()
  p2 <- ggplot(t1,aes(week,wp))+geom_line()+geom_point()+labs(y="wprice")
  t1 <- subset(brand_data,brandname=="ochirly"&retailtypename=="Ůװ")#ochirly
  p3 <- ggplot(t1,aes(week,share))+geom_line()+geom_point()
  p4 <- ggplot(t1,aes(week,wp))+geom_line()+geom_point()+labs(y="wprice")
  plot_grid(p1,p3,p2,p4,ncol=2)
  
  
 
}