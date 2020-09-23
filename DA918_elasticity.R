{
  ###1.��������####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
}

{#��������
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

{#Ʒ������-no_brands  no_stores  avg_retailprice   
  dt_cat
  numb <- function(data){length(unique(data))}
  no_brands <- dt_cat[,c(.(num_brand=numb(brandname))),by="retailtypename"]
  no_brands#Ʒ���е�Ʒ������
  no_brands <- no_brands[order(retailtypename),]#�����������򣬷���������ղ鿴
  no_brands
  #no_brands <- dt_cat[,numb(brandname),by="itemtypename"]
  #no_brands#С�����е�Ʒ������
  no_stores <- dt_cat[,c(.(num_store=numb(storecode))),by="retailtypename"]
  no_stores <- no_stores[order(retailtypename),]
  no_stores#��Щ�ŵ��������Ʒ��
  #����������ƽ���۸�������֣���Ҫ�������ܵ���������أ�
  #ֱ�Ӽ���Ʒ�Ƶ���ƽ���۸�������Ʒ�Ʒݶ����(���ǰ��ռ۸�������ݶ�Ӧ������������������������۶�ݶ���ʲôӰ���أ��ʣ��������ݶ�����۶�ݶ�ã�)��
  dt_cat$week <- week(dt_cat$saledate)
  gc()
  quantile(dt_cat$price,0.90)#1596 
  cond <- which(dt_cat$price > 1596)
  quantile(dt_cat$price,0.95)#396 #�ٴ�ɾ��һЩ����ֹ�쳣ֵӰ��
  cond <- which(dt_cat$price > 396)
  dt_cat <- dt_cat[-cond,]#�����쳣ֵ���������ɾ�����ּ۸���ߵ�����
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
  #dt_cat[order(saledate,storecode,poscode,saletime,possalecode),]��ʱ�������û�
}
