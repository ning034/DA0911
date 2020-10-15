{
  ###1.��������####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
  library(papeR)
  dfs <- fread ("data_category.csv", header = T, sep=",")
  gc()
  {#
    length(unique(df$barcode))
    sum(df$amt)
    length(unique(df$brandname))
  }
  dfs$quarter <- quarter(dfs$saledate)
  df <- subset(dfs, price<quantile(dfs$price,0.80))
  tips <- function(data){length(unique(data))}
  x <- df[,tips(barcode),by="retailtypename"]
  x
  mean(x$V1)
  quantile(dfs$price,0.80)
}

{#data2017
  #����data2017
  data2017
  summary(data2017$price)
  summary(data2017$qty)
  summary(data2017$discountinamt)
  summary(data2017$discountoutamt)
  summary(data2017$posdiscountamt)
  summary(data2017$merchantdiscountamt)
  summary(data2017$vipdiscountamt)
  
  #�޳���ֵ
  data2017 <- subset(data2017,price<quantile(data2017$price,0.8)&price>0)
  data2017 <- subset(data2017,qty<quantile(data2017$qty,0.99)&qty>0)
  data2017 <- subset(data2017,discountinamt >= 0)
  
  gc()
  
  data2017$quarter <- quarter(data2017$saledate)
  gc()
}

{#DA1009_quarter level�е�Ʒ��ģ��lm_data���ݸ�Ϊlm_data2016
  lm_data2016 <- lm_data
  retailtype <- unique(lm_data2016$retailtypename)
  
  #ɾ����ı����Ա����2017������
  rm(df)
  df <- data2017
  rm(data2017)
  df <- subset(df,retailtypename %in% retailtype)
  length(unique(df$retailtypename))
  
  {#����2017��lm����
    #���㼾�ȷݶ�
    gc()
    sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","retailtypename","quarter")]
    sku_data
    sku_data <- sku_data %>% group_by(retailtypename,quarter) %>% mutate(retailamt=sum(sku_amt))
    sku_data
    sku_data$share <- 100 * sku_data$sku_amt / sku_data$retailamt
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
    lm_data2017 <- left_join(sku_data,distribution,by=c("barcode","quarter"))
    lm_data2017$sku_distribution2 <- lm_data2017$sku_distribution ^ 2
    lm_data2017
    lm_data2017$quarter <- lm_data2017$quarter + 4
  }
  
  #����ع�����
  lm_data2016$barcode <- as.character(lm_data2016$barcode)
  lm_data2017$barcode <- as.character(lm_data2017$barcode)
  lm_data <- rbind(lm_data2016,lm_data2017)
  lm_data
  
  length(unique(lm_data$retailtypename))
  retailtypename <- unique(lm_data$retailtypename)
}

{#Ʒ��ģ�ͻع�
  #����0����������Ʒ�ำֵ1
  m <-nrow(lm_data)
  x <- matrix(0,m,48)
  x <- as.data.table(x)
  y <-c(x,lm_data)
  y <- as.data.table(y)
  head(y)
  x <- as.matrix(x)#��xת��Ϊ��������Ϊdata.table�ṹ�����ݣ��޷���DT[,i]ȡ���б���
  
  j <-seq(1,48,by=2)
  retailtypename <- rep(retailtypename,each=2)#���ں����ظ���ֵ��48������ֻȡһ�룬�������ｫƷ�������ظ�2�Ρ�
  for(i in j){
    w <- which(y$retailtypename==retailtypename[i])
    x[w , i] <- 1
    x[, i+1] <- x[, i]
    x[, i] <- x[, i] * y$sku_distribution
    x[, i+1] <- x[, i+1] * y$sku_distribution2
  }
  
  
  model2 <- lm(lm_data$share~x)
  summary(model2)
  
  #�е�����
  retailtypename
  
  #������
  j <-seq(1,48,by=2)
  result <- c()
  result1 <- c()
  zz <- prettify(summary(model2),signif.stars = getOption("show.signif.stars"))#�õ������Ե��Ǻ�
  zz <- zz[-1,]
  for(i in j){
    result1 <- data.frame(cat=retailtypename[i],acv=zz$Estimate[i],p_value=zz$`Pr(>|t|)`[i],acv2=zz$Estimate[i+1],p_value2=zz$`Pr(>|t|)`[i+1],star1=zz$`   `[i],star2=zz$`   `[i+1],row.names = NULL)
    result <- rbind(result,result1)
  }
  result
  result$acv <-round(result$acv,5)
  result$acv2 <- round(result$acv2,5)
  result
  
  output2 <- result
  output2$acv <- paste(output2$acv,output2$star1,sep="")
  output2$acv2 <- paste(output2$acv2,output2$star2,sep="")
  output2
  output2 <- output2[,-c(6,7)]
  xtable(output2)#���Ϊlatex
  write.csv(output2,file = "D:/D/data/xuhuibusiness/output2_16-17.csv",row.names = F) #���Ϊtable
  
}

#������ƽ����
  #�ȴ�һ��ԭʼ�ع����ݱ�������Ϊ�����ʹ�ñ�����lm_data
  lm_data_ori <- lm_data
  lm_data_ori$year <- 2016
  w <- which(lm_data_ori$quarter > 4)
  lm_data_ori$year[w] <- 2017
  lm_data_ori
  lm_data_ori <- as.data.table(lm_data_ori)
  
  lm_data <- lm_data_ori[,c(.(share=mean(share)),.(sku_distribution=mean(sku_distribution)),.(retailtypename=unique(retailtypename))),by=c("barcode","year")]
  lm_data$sku_distribution2 <- lm_data$sku_distribution * lm_data$sku_distribution
  lm_data
  
  retailtypename <- unique(lm_data$retailtypename)
  
  {
    {#Ʒ��ģ�ͻع�
      #����0����������Ʒ�ำֵ1
      m <-nrow(lm_data)
      x <- matrix(0,m,48)
      x <- as.data.table(x)
      y <-c(x,lm_data)
      y <- as.data.table(y)
      head(y)
      x <- as.matrix(x)#��xת��Ϊ��������Ϊdata.table�ṹ�����ݣ��޷���DT[,i]ȡ���б���
      
      j <-seq(1,48,by=2)
      retailtypename <- rep(retailtypename,each=2)#���ں����ظ���ֵ��48������ֻȡһ�룬�������ｫƷ�������ظ�2�Ρ�
      for(i in j){
        w <- which(y$retailtypename==retailtypename[i])
        x[w , i] <- 1
        x[, i+1] <- x[, i]
        x[, i] <- x[, i] * y$sku_distribution
        x[, i+1] <- x[, i+1] * y$sku_distribution2
      }
      
      
      model2 <- lm(lm_data$share~x)
      summary(model2)
      
      #�е�����
      retailtypename
      
      #������
      j <-seq(1,48,by=2)
      result <- c()
      result1 <- c()
      zz <- prettify(summary(model2),signif.stars = getOption("show.signif.stars"))#�õ������Ե��Ǻ�
      zz <- zz[-1,]
      for(i in j){
        result1 <- data.frame(cat=retailtypename[i],acv=zz$Estimate[i],p_value=zz$`Pr(>|t|)`[i],acv2=zz$Estimate[i+1],p_value2=zz$`Pr(>|t|)`[i+1],star1=zz$`   `[i],star2=zz$`   `[i+1],row.names = NULL)
        result <- rbind(result,result1)
      }
      result
      result$acv <-round(result$acv,5)
      result$acv2 <- round(result$acv2,5)
      result
      
      output2 <- result
      output2$acv <- paste(output2$acv,output2$star1,sep="")
      output2$acv2 <- paste(output2$acv2,output2$star2,sep="")
      output2
      output2 <- output2[,-c(6,7)]
      xtable(output2)#���Ϊlatex
      write.csv(output2,file = "D:/D/data/xuhuibusiness/output2_16-17.csv",row.names = F) #���Ϊtable
  }
  
  }
  