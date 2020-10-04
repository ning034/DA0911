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
    length(unique(df$brandname))
  }
  dfs$quarter <- quarter(dfs$saledate)
  df <- subset(dfs, price<quantile(dfs$price,0.80))
}

{#sku-level year-level picture
  typename <- unique(df$retailtypename)
  gc()
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode")]
  sku_data$share <- 100 * sku_data$sku_amt / sum(df$amt)
  head(sku_data)
  
  #计算加权铺货
  distribution <- df[,c(.(sku_amt=sum(amt)),.(barcode=unique(barcode))),by=c("storecode")]
  distribution <- distribution %>% group_by(barcode) %>%
    mutate(store_amt=sum(sku_amt))
  head(distribution)
  distribution$sku_distribution <- 100 * distribution$store_amt / sum(df$amt)
  distribution <- distribution %>% select(.,c(barcode,sku_distribution))
  distribution <- unique(distribution)
  distribution
  
  #回归数据
  lm_data <- left_join(sku_data,distribution,by=c("barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  summary(lm_data$sku_distribution)
  lm1 <- lm(share~sku_distribution+sku_distribution2,data=lm_data)
  summary(lm1)
  
  #品类信息
  cat <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
  cat
  lm_data <- left_join(lm_data,cat,by="barcode")
  lm_data <- as.data.table(lm_data)
  
  #sku-level 铺货与份额关系
  df1 <- subset(lm_data,retailtypename==typename[4])
  df1
  ggplot(df1,aes(sku_distribution,share))+geom_point()+theme_bw()
  summary(df1$sku_distribution)#第三分位数为92.104，说明大多数SKU的铺货低于这个数值
  
  #自动保存多张图片
  for (i in 1:24) {
    df1 <- subset(lm_data,retailtypename==typename[i])
    #save the plot
    png(filename = paste0("type",typename[i],i, ".jpg"),width = 2400,height = 1800,res = 300)
    print(ggplot(data = df1, aes(x = sku_distribution, y = share)) + geom_point())+theme_bw()
    dev.off()
  }
  
  

  
}