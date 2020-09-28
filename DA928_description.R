{
  ###1.基本设置####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
  dfs <- fread ("data_category.csv", header = T, sep=",")
  gc()
  {#
    length(unique(df$barcode))
    sum(df$amt)
  }
  df$quarter <- quarter(df$saledate)
  df <- subset(dfs, price<quantile(dfs$price,0.80))
}

{#一般参数模型####
  #计算季度份额
  df$quarter <- quarter(df$saledate)
  gc()
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","quarter")]
  sku_data <- sku_data %>% group_by(quarter) %>% mutate(jidu_amt=sum(sku_amt))
  sku_data$share <- 100 * sku_data$sku_amt / sku_data$jidu_amt
  head(sku_data)
  
  #计算加权铺货
  distribution <- df[,c(.(sku_amt=sum(amt)),.(barcode=unique(barcode))),by=c("storecode","quarter")]
  distribution <- distribution %>% group_by(barcode,quarter) %>%
    mutate(store_amt=sum(sku_amt))
  head(distribution)
  distribution$sku_distribution <- 100 * distribution$store_amt / sum(df$amt)
  distribution <- distribution %>% select(.,c(quarter,barcode,sku_distribution))
  distribution <- unique(distribution)
  distribution
  
  #回归数据
  lm_data <- left_join(sku_data,distribution,by=c("quarter","barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  summary(lm_data$sku_distribution)
  lm1 <- lm(share~sku_distribution+sku_distribution2,data=lm_data)
  summary(lm1)
  
  {#数值铺货率
    tips <- function(data){length(unique(data))}
    store_num <- df[,c(.(num=tips(storecode))),by=c("barcode","quarter")]
    store <- df[,c(.(num=tips(storecode))),by=c("quarter")]
    store_num <- left_join(store_num,store,by="quarter")
    store_num$distribution_num <-100* store_num$num.x / store_num$num.y
    store_num
    summary(store_num$distribution_num)
    summary(store_num$sku_distribution)
    #cor
    store_num <- left_join(store_num,lm_data,by=c("barcode","quarter"))
    store_num
    cor(store_num$distribution_num,store_num$sku_distribution)
  }
  {#品类基本信息描述
    head(store_num)
    cat <- df[,c(.(amt=sum(amt)),.(brand_num=tips(brandname)),.(sku_num=tips(barcode))),by="retailtypename"]
    cat#1
    barcode_cat <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
    barcode_cat
    store_num
    store_num <- left_join(store_num,barcode_cat,by="barcode")
    store_num <- as.data.table(store_num)
    acv <- store_num[,c(.(acv_avg=mean(sku_distribution)),.(acv_med=median(sku_distribution)),.(acv_max=max(sku_distribution))),by="retailtypename"]
    acv#3
    head(sku_data)
    sku_data <- left_join(sku_data,barcode_cat,by="barcode")
    sku_data <- as.data.table(sku_data)
    ms <- sku_data[,c(.(ms_avg=mean(share)),.(ms_med=median(share)),.(ms_max=max(share))),by="retailtypename"]
    ms#2
    smr_information <- left_join(cat,ms,by="retailtypename")
    smr_information <- left_join(smr_information,acv,by="retailtypename")
    smr_information <- arrange(smr_information,retailtypename)
    
    smr_information
    max(smr_information$amt)#/10000
    smr_information$amt <- smr_information$amt / 10000
    write.csv(smr_information,file = "D:/D/data/xuhuibusiness/smr_information.csv",row.names = F)
    test <- smr_information
    test$amt <- round(test$amt,0)
    test$ms_avg <- round(test$ms_avg,3)
    test$ms_med <- round(test$ms_med,3)
    test$ms_max <- round(test$ms_max,3)
    test$acv_avg <- round(test$acv_avg,2)
    test$acv_med <- round(test$acv_med,2)
    test$acv_max <- round(test$acv_max,2)
    test
    x <- paste(test$retailtypename,test$amt,test$brand_num,test$sku_num,test$ms_avg,test$ms_med,test$ms_max,test$acv_avg,test$acv_med,test$acv_max,sep="&")
    x 
    x <- paste(x,"\\",sep="")
    write.csv(x,file = "D:/D/data/xuhuibusiness/table_smr_information.csv",row.names = F,quote = F)
    }
}

{#确定品类参数模型####
  unique(df$retailtypename)
  #计算季度份额
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","quarter","retailtypename")]
  sku_data <- sku_data %>% group_by(quarter,retailtypename) %>% mutate(jidu_amt=sum(sku_amt))
  sku_data$share <- 100 * sku_data$sku_amt / sku_data$jidu_amt
  head(sku_data)
  
  #计算加权铺货。
  distribution <- df[,c(.(sku_amt=sum(amt)),.(barcode=unique(barcode))),by=c("storecode","quarter")]
  distribution <- distribution %>% group_by(barcode,quarter) %>%
    mutate(store_amt=sum(sku_amt))
  head(distribution)
  distribution$sku_distribution <- 100 * distribution$store_amt / sum(df$amt)
  distribution <- distribution %>% select(.,c(quarter,barcode,sku_distribution))
  distribution <- unique(distribution)
  distribution
  
  #回归数据
  lm_data <- left_join(sku_data,distribution,by=c("quarter","barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  #lm1 <- lm(share~sku_distribution+sku_distribution2+retailtypename,data=lm_data)#好奇看了一下
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
  write.csv(result,file="D:/D/data/xuhuibusiness/equation3.csv",row.names = F)
}

{#品类特征模型####
  #同一品类中假定参数相同
  
  
}

{#具体品类中的领导者品牌模型####
  
}
