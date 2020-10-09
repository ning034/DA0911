{
  ###1.��������####
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
  tips <- function(data){length(unique(data))}
  x <- df[,tips(barcode),by="retailtypename"]
  x
  mean(x$V1)
  quantile(dfs$price,0.80)
}

{##����####
  ##����ÿ��skuȫ�����۶�
  s <- aggregate (df$amt, by = list (df$barcode), sum)
  names (s) [1:2] <- c ("barcode", "amt")
  
  ##ÿ��sku���г��ݶ�
  s$sk <- s$amt / sum (s$amt) * 100
  summary (s$sk)
  
  ##����ÿ��sku���̻�����Ȩ��
  ###ÿ���̵�����ÿ��sku�����۶�
  acv <- aggregate (df$amt, by = list (df$storecode, df$barcode), sum)
  names (acv)[1:3] <- c("storecode", "barcode", "gamt")
  
  ###ÿ���̵�������ܶ�
  store <- aggregate (df$amt, by = list (df$storecode), sum)
  names (store) [1:2] <- c("storecode", "samt")
  acv1 <- left_join (acv, store, by = "storecode")
  
  ###ÿ��sku�����̵�����۶�
  acv2 <- aggregate (acv1$samt, by = list (acv1$barcode), sum)
  ##ÿ��sku��%acv
  acv2$acv <- acv2$x / sum (store$samt) * 100
  summary (acv2$acv)
  names (acv2) [1:3] <- c("barcode", "x2", "acvk")
  acv4 <- left_join (s, acv2, by = "barcode")
  ##��ģһ�㹫������ģ��
  acv4$facvk<-acv4$acvk^2
  lm1<-lm(acv4$sk~acv4$acvk+acv4$facv)
  summary(lm1)
  result1 <- summary(lm1)
  
  {#Ʒ��ģ��####
    ##Ʒ�༶�����-�ݶ��ϵ����ȣ�
    pinlei<-unique(df$retailtypename)
    df1 <- subset (df, retailtypename == pinlei[1])
    
    ##����ÿ��skuȫ�����۶�
    s <- aggregate (df1$amt, by = list (df1$barcode), sum)
    names(s)[1:2] <- c("barcode","amt")
    
    ##ÿ��sku���г��ݶ�
    s$sk <- s$amt / sum (s$amt) * 100
    summary (s$sk)
    
    ##����ÿ��sku���̻�����Ȩ��
    ###ÿ���̵�����ÿ��sku�����۶�
    acv <- aggregate (df1$amt, by = list (df1$storecode, df1$barcode), sum)
    names(acv)[1:3] <- c ("storecode", "barcode", "gamt")
    
    ###ÿ���̵�������ܶ�
    store<-aggregate (df$amt, by = list(df$storecode), sum)
    names(store)[1:2] <- c("storecode", "samt")
    acv1<- left_join (acv, store, by = "storecode")
    
    ###ÿ��sku�����̵�����۶�
    acv2 <- aggregate (acv1$samt, by = list (acv1$barcode), sum)
    
    ##ÿ��sku��%acv
    acv2$acv <- acv2$x / sum (store$samt) * 100
    summary (acv2$acv)
    names (acv2) [1:3] <- c("barcode", "x2", "acvk")
    acv4 <- left_join (s, acv2, by = "barcode")
    ##��ģ
    acv4$facvk<-acv4$acvk^2
    lmp1<-lm(acv4$sk~acv4$acvk+acv4$facv)
    summary(lmp1)
    ##��ģ
    result1 <- c()
    for (i in 1:24){
      df1 <- subset (df, retailtypename == pinlei[i])
      
      ##����ÿ��skuȫ�����۶�
      s <- aggregate (df1$amt, by = list (df1$barcode), sum)
      names(s)[1:2] <- c("barcode","amt")
      
      ##ÿ��sku���г��ݶ�
      s$sk <- s$amt / sum (s$amt) * 100
      summary (s$sk)
      
      ##����ÿ��sku���̻�����Ȩ��
      ###ÿ���̵�����ÿ��sku�����۶�
      acv <- aggregate (df1$amt, by = list (df1$storecode, df1$barcode), sum)
      names(acv)[1:3] <- c ("storecode", "barcode", "gamt")
      
      ###ÿ���̵�������ܶ�
      store<-aggregate (df$amt, by = list(df$storecode), sum)
      names(store)[1:2] <- c("storecode", "samt")
      acv1<- left_join (acv, store, by = "storecode")
      
      ###ÿ��sku�����̵�����۶�
      acv2 <- aggregate (acv1$samt, by = list (acv1$barcode), sum)
      
      ##ÿ��sku��%acv
      acv2$acv <- acv2$x / sum (store$samt) * 100
      summary (acv2$acv)
      names (acv2) [1:3] <- c("barcode", "x2", "acvk")
      acv4 <- left_join (s, acv2, by = "barcode")
      ##������
      acv4$facvk <- acv4$acvk^2
      fit <- lm (acv4$sk ~ acv4$acvk + acv4$facvk)
      result1 <- rbind (result1, coef(summary(fit))[, c(1, 2, 4)])
    }
    
  }
  
}

{#һ�����ģ���������һ��####
  #������ȷݶ�
  gc()
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode")]
  sku_data$share <- 100 * sku_data$sku_amt / sum(df$amt)
  head(sku_data)
  
  #�����Ȩ�̻�
  distribution <- df[,c(.(sku_amt=sum(amt)),.(barcode=unique(barcode))),by=c("storecode")]
  distribution <- distribution %>% group_by(barcode) %>%
    mutate(store_amt=sum(sku_amt))
  head(distribution)
  distribution$sku_distribution <- 100 * distribution$store_amt / sum(df$amt)
  distribution <- distribution %>% select(.,c(barcode,sku_distribution))
  distribution <- unique(distribution)
  distribution
  
  #�ع�����
  lm_data <- left_join(sku_data,distribution,by=c("barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  summary(lm_data$sku_distribution)
  lm1 <- lm(share~sku_distribution+sku_distribution2,data=lm_data)
  summary(lm1) 
}

{#Ʒ��ģ�������һ��####
  #������ԭ����lm_data��sku�ķݶ�Ҫ��Ʒ����

  #������ȷݶ�
  gc()
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","retailtypename")]
  sku_data
  sku_data <- sku_data %>% group_by(retailtypename) %>% mutate(retailamt=sum(sku_amt))
  sku_data
  sku_data$share <- 100 * sku_data$sku_amt / sku_data$retailamt
  head(sku_data)
  
  #�����Ȩ�̻�
  distribution <- df[,c(.(sku_amt=sum(amt)),.(barcode=unique(barcode))),by=c("storecode")]
  distribution <- distribution %>% group_by(barcode) %>%
    mutate(store_amt=sum(sku_amt))
  head(distribution)
  distribution$sku_distribution <- 100 * distribution$store_amt / sum(df$amt)
  distribution <- distribution %>% select(.,c(barcode,sku_distribution))
  distribution <- unique(distribution)
  distribution
  
  #�ع�����
  lm_data <- left_join(sku_data,distribution,by=c("barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  
  #
  retailtypename <- unique(df$retailtypename)
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
  xtable(result,digits = 5)
  
  {#ԭ��Ʒ��ģ��####
    lm_data
    
    #���������
    retailtypename
    lm_data2 <- lm_data[,-c(1,3,4)]
    
    #һ����������ĳ���
    lm_data2$cat1 <- 0
    lm_data2
    w <-which(lm_data2$retailtypename==retailtypename[1])
    lm_data2$cat1[w] <- 1
    lm_data2$cat2 <- lm_data2$cat1
    lm_data2$cat1 <- lm_data2$sku_distribution * lm_data2$cat1
    lm_data2$cat2 <- lm_data2$sku_distribution2 * lm_data2$cat2
    lm_data2
    lm1 <- lm(lm_data2$share~lm_data2$cat1+lm_data2$cat2)
    summary(lm1)
    
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
    
    lm1 <- lm(lm_data$share~x)
    summary(lm1)
    #�е�����
    retailtypename
    xtable(summary(lm1))
    
    j <-seq(1,48,by=2)
    beta <- coef(lm1)
    beta <- beta[-1]
    pvalue <- coef(summary(lm1))[,4]
    pvalue <- pvalue[-1]
    result <- c()
    result1 <- c()
    zz <- prettify(summary(lm1),signif.stars = getOption("show.signif.stars"))#�õ������Ե��Ǻ�
    zz <- zz[-1,]
    for(i in j){
    result1 <- data.frame(cat=retailtypename[i],acv=beta[i],p_value=pvalue[i],acv2=beta[i+1],p_value2=pvalue[i+1],signif=zz$`   `[i],row.names = NULL)
    result <- rbind(result,result1)
    }
    result
    result$p_value2 <-signif(result$p_value2,5)
    xtable(result,digits = 5)
    
  }
}