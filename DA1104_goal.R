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

{#Ʒ�Ʋ�����г��ݶ�-��sku�Ĺ�ϵ
  
  
  gc()
  brand_data <- dt2016[,.(brand_amt=sum(amt),sku_num=uniqueN(barcode),mprice=mean(price),pos_num=uniqueN(posdiscountamt),pos_sum=mean(posdiscountamt/price),out_num=uniqueN(discountoutamt),out_sum=mean(discountoutamt/price)),by=c("brandname","retailtypename")]
  brand_data[,retailamt:=sum(brand_amt),by="retailtypename"]
  brand_data$share <- 100 * brand_data$brand_amt / brand_data$retailamt
  brand_data
  
  sku <- dt2016[,.(amt=sum(amt),mprice=mean(price)),by=c("brandname","barcode")]
  sku[,sumamt:=sum(amt),by="brandname"]
  sku$weight <- sku$amt / sku$sumamt
  sku$wprice <- sku$mprice * sku$weight
  sku
  wprice <- sku[,.(wp=sum(wprice)),by="brandname"]  
  wprice
  
  brand_data <- left_join(brand_data,wprice,by="brandname")
  brand_data <- as.data.table(brand_data)
  brand_data
  
  #test
  test <- brand_data[retailtypename==cat_amt$retailtypename[3]]
  model0 <- lm(share ~ mprice + sku_num + pos_num + pos_sum + out_num + out_sum, data=test)
  summary(model0)
  
  #����ͳ��
  test <- brand_data[retailtypename==cat_amt$retailtypename[3]]
  test
  summary(test$share)
  test$index_share <- "high"
  w <- which(test$share < median(test$share))
  test[w,]$index_share <- "low"
  test
  
  hist(test$share,xlab="Ʒ���г��ݶ�",ylab="Ƶ��",main=NULL)
  
  par(mfrow=c(3,2))
  hist(test$wp,xlab="��Ȩƽ���ۼ�",ylab="sku����",main=NULL)
  hist(test$sku_num,xlab="sku����",ylab="sku����",main=NULL)
  hist(test$pos_num,xlab="pos���۴���",ylab="sku����",main=NULL)
  hist(test$pos_sum,xlab="pos��������",ylab="sku����",main=NULL)
  hist(test$out_num,xlab="���ô���",ylab="sku����",main=NULL)
  hist(test$out_sum,xlab="��������",ylab="sku����",main=NULL)
  dev.off()
  
  par(mfrow=c(3,2))
  boxplot(wp~ index_share,data=test,xlab="Ʒ���г��ݶ�",ylab="��Ȩƽ���ۼ�")
  boxplot(sku_num~ index_share,data=test,xlab="Ʒ���г��ݶ�",ylab="sku����")
  boxplot(pos_num~ index_share,data=test,xlab="Ʒ���г��ݶ�",ylab="pos�ۿ۴���")
  boxplot(pos_sum~ index_share,data=test,xlab="Ʒ���г��ݶ�",ylab="pos�ۿ�����")
  boxplot(out_num~ index_share,data=test,xlab="Ʒ���г��ݶ�",ylab="���ô���")
  boxplot(out_sum~ index_share,data=test,xlab="Ʒ���г��ݶ�",ylab="��������")
  dev.off()
  
  #
  
}
