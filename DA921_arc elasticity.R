{
  ###1.��������####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
}
{
  dt_cat <- fread("data_category.csv",header = T,sep=",")#7588515
  length(unique(dt_cat$brandname))#2104
  gc()
  
}

{
  dt_cat$week <- week(dt_cat$saledate)
  gc()
  quantile(dt_cat$price,0.855)#396 #ɾ��һЩ����ֹ�쳣ֵӰ��
  cond <- which(dt_cat$price > 396)
  dt_cat <- dt_cat[-cond,]#�����쳣ֵ���������ɾ�����ּ۸���ߵ�����
  gc()
  dt_cat
  {#brand price week
    p_elstic <- dt_cat[,c(.(qty=sum(qty)) ,.(price_ac=mean(amt/qty)) ,.(price_ori=mean(price))) ,by=c("brandname","week")]
    p_elstic
    #��Լ۸�仯####
    {#ѡ��һ��Ʒ��:����,��������ͼ1(��Լ۸��뻡�۸���)####
      brand <- dt_cat[, sum(amt),by="brandname"]
      brand <- brand[order(V1),]
      brand
      test <- subset(p_elstic,brandname=="����")
      test <- test[order(week),]
      test
      test_dif <- diff(test$price_ac)
      test_dif <- test_dif / test$price_ac[-53]#�۸�����Ա仯1-52 week
      test_dif
      pt <- test$price_ac[2:53]
      pt_1 <- test$price_ac[1:52]
      test <- subset(brand_share,brandname=="����")#���������brand share
      test <- test[order(week),]
      test
      st <- test$share[2:53]
      st_1 <- test$share[1:52]
      arc_elasticity <- ((st-st_1)/(st+st_1))/ ((pt-pt_1) / (pt+pt_1))
      arc_elasticity
      plot(test_dif,arc_elasticity)
      #ȥ���쳣ֵ���ÿ�
      which(arc_elasticity >1000)#12
      which(arc_elasticity < (-1000))#33
      arc_elasticity <- arc_elasticity[-c(12,33)]
      test_dif <- test_dif[-c(12,33)]
      plot(test_dif,arc_elasticity)
      #�������Լ۸�仯����5%�����򣬻����Ծͻ�����ƽ��
    }
    
  }
  {#brand share Ʒ��ռƷ�������ı���#####
    brand_share<-dt_cat[ ,c(.(qty=sum(qty))),by=c("retailtypename","brandname","week")]
    brand_share
    brand_share<-brand_share %>% group_by(retailtypename,week) %>% mutate(sum=sum(qty))
    brand_share
    brand_share<-arrange(brand_share,retailtypename,week)
    brand_share
    brand_share<-as.data.table(brand_share)
    brand_share$share<-brand_share$qty/brand_share$sum
    brand_share
  }
   df <- left_join(brand_share,p_elstic,by=c("brandname","week"))#�۸������г��ݶ��һ
   df
   write.csv(df,file = "D:/D/data/xuhuibusiness/arc_elasticity.csv", row.names = F)
   
   {#���¶���brand characteristic ######
     arc_elasticity <- fread("arc_elasticity.csv",header = T)
     arc_elasticity
     tips <- function(data){length(unique(data))}
     brand_num <- arc_elasticity[,c(.(num=tips(brandname))),by="retailtypename"]
     brand_num
     summary(brand_num$num)#̫���ˣ�������Ҫѡ��ϸ�ķ��������ѡ������Ʒ�ƻ��߿��ǾͶ���ô��Ʒ�ƽ����о�
     arc_elasticity
     arc_elasticity$discount_mag <- (arc_elasticity$price_ori - arc_elasticity$price_ac) / arc_elasticity$price_ori#�ۿ۷���
     arc_elasticity$discount_freq <- 0
     w <- which(arc_elasticity$discount_mag > 0.05)
     arc_elasticity[w,]$discount_freq <- 1 #��ֵ1�����ۿ۷��ȳ���5%������ͳ��������ΪƷ�Ƶ�����
     #������Լ۸�Ʒ�Ƶ�ƽ���Ǵ����۸� / Ʒ��ļ�Ȩƽ���۸�
     #����۸�䶯ϵ����Ʒ�Ƽ۸��׼�� / Ʒ��ƽ���۸�   ���������Ҫ������( Bolton 1989)��ȷ�����ӷ�ĸ�еļ۸�����ʵ�۸���ԭ��
     
     
   }
}

{#��ֵ�̻�,����
  dt_cat
  set.seed(10000)
  n <- sample(1:nrow(dt_cat),10000)
  dt_cat_sample <-  dt_cat[n,]
  write.csv(dt_cat_sample,file = "D:/D/data/xuhuibusiness/dt_cat_sample.csv", row.names = F)
}

{#���¶������ݣ���ֵ�̻���=(�̻����ŵ���)/�ܵ��ŵ���     ��ˮƽ
  dt_cat_sample <- fread("dt_cat_sample.csv",header = T)
  dt_cat_sample
  length(unique(dt_cat_sample$storecode))#8��
  tips <- function(data){length(unique(data))}
  distribution_num <- dt_cat_sample[,c(.(store_num=tips(storecode))),by=c("barcode")]
  distribution_num
  distribution_num$distri_num <- distribution_num$store_num / length(unique(dt_cat_sample$storecode))
  summary(distribution_num$distri_num)
  distribution_num
  {#��Ȩ�̻���:�̻����ŵ��������۽��)/�����ŵ����۽��
    product_amt <- dt_cat_sample[,c(.(amt_store=sum(amt))),by=c("barcode","storecode")]
    product_amt <- product_amt %>% group_by(barcode) %>%
      mutate(amt_sum=sum(amt_store))
    product_amt <- arrange(product_amt,barcode)
    product_amt
    distribution_weight <- product_amt %>% select(.,barcode,amt_sum)
    distribution_weight <- unique(distribution_weight)
    distribution_weight$distri_weight <- distribution_weight$amt_sum / sum(dt_cat_sample$amt)
    distribution_weight$distri_weight <- distribution_weight$distri_weight * 100
    summary(distribution_weight$distri_weight)
  }
  #�����̻��ǲ�����ء�������
  df <- left_join(distribution_weight,distribution_num,by="barcode")
  df
  cor(df$distri_num,df$distri_weight)#0.1871002
}