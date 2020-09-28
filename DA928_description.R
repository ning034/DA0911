{
  ###1.��������####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
  df <- fread ("data_category.csv", header = T, sep=",")
  gc()
  #dfs <- df
  #df <- subset(dfs, price<quantile(dfs$price,0.8))##��֤����Ľ��
}

{#һ�����ģ��####
  #���㼾�ȷݶ�
  df$quarter <- quarter(df$saledate)
  gc()
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","quarter")]
  sku_data <- sku_data %>% group_by(quarter) %>% mutate(jidu_amt=sum(sku_amt))
  sku_data$share <- 100 * sku_data$sku_amt / sku_data$jidu_amt
  head(sku_data)
  
  #�����Ȩ�̻�
  distribution <- df[,c(.(sku_amt=sum(amt)),.(barcode=unique(barcode))),by=c("storecode","quarter")]
  distribution <- distribution %>% group_by(barcode,quarter) %>%
    mutate(store_amt=sum(sku_amt))
  head(distribution)
  distribution$sku_distribution <- 100 * distribution$store_amt / sum(df$amt)
  distribution <- distribution %>% select(.,c(quarter,barcode,sku_distribution))
  distribution <- unique(distribution)
  distribution
  
  #�ع�����
  lm_data <- left_join(sku_data,distribution,by=c("quarter","barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  lm1 <- lm(share~sku_distribution+sku_distribution2,data=lm_data)
  summary(lm1)
}

{#ȷ��Ʒ�����ģ��####
  unique(df$retailtypename)
  #���㼾�ȷݶ�
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","quarter","retailtypename")]
  sku_data <- sku_data %>% group_by(quarter,retailtypename) %>% mutate(jidu_amt=sum(sku_amt))
  sku_data$share <- 100 * sku_data$sku_amt / sku_data$jidu_amt
  head(sku_data)
  
  #�����Ȩ�̻���
  distribution <- df[,c(.(sku_amt=sum(amt)),.(barcode=unique(barcode))),by=c("storecode","quarter")]
  distribution <- distribution %>% group_by(barcode,quarter) %>%
    mutate(store_amt=sum(sku_amt))
  head(distribution)
  distribution$sku_distribution <- 100 * distribution$store_amt / sum(df$amt)
  distribution <- distribution %>% select(.,c(quarter,barcode,sku_distribution))
  distribution <- unique(distribution)
  distribution
  
  #�ع�����
  lm_data <- left_join(sku_data,distribution,by=c("quarter","barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  #lm1 <- lm(share~sku_distribution+sku_distribution2+retailtypename,data=lm_data)#���濴��һ��
  #summary(lm1)
  
  retailtypename <- unique(df$retailtypename)
  retailtypename <- retailtypename[order(retailtypename)]
  retailtypename 
  
  result1 <- c()
  result <- c()
  for(i in 1:24){
    df1 <- subset(lm_data,retailtypename==retailtypename[i])
    lm1 <- lm(share~sku_distribution+sku_distribution2,data=df1)
    result1 <- data.frame(cat=retailtypename[i],acv=coef(lm1)[2],p_value=coef(summary(lm1))[,4][2],acv2=coef(lm1)[3],p_value2=coef(summary(lm1))[,4][3],row.names = NULL)
    result <- rbind(result,result1)
  }
  result 
  write.csv(result,file="D:/D/data/xuhuibusiness/equation3.csv")
}

{#Ʒ������ģ��####
  #ͬһƷ���мٶ�������ͬ
  
  
}

{#����Ʒ���е��쵼��Ʒ��ģ��####
  
}