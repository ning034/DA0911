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
  tips <- function(data){length(unique(data))}
  x <- df[,tips(barcode),by="retailtypename"]
  x
  mean(x$V1)
  quantile(dfs$price,0.80)
}

{##红雨####
  ##计算每个sku全年销售额
  s <- aggregate (df$amt, by = list (df$barcode), sum)
  names (s) [1:2] <- c ("barcode", "amt")
  
  ##每个sku的市场份额
  s$sk <- s$amt / sum (s$amt) * 100
  summary (s$sk)
  
  ##计算每个sku的铺货（加权）
  ###每个商店销售每个sku的销售额
  acv <- aggregate (df$amt, by = list (df$storecode, df$barcode), sum)
  names (acv)[1:3] <- c("storecode", "barcode", "gamt")
  
  ###每个商店的销售总额
  store <- aggregate (df$amt, by = list (df$storecode), sum)
  names (store) [1:2] <- c("storecode", "samt")
  acv1 <- left_join (acv, store, by = "storecode")
  
  ###每个sku所在商店的销售额
  acv2 <- aggregate (acv1$samt, by = list (acv1$barcode), sum)
  ##每个sku的%acv
  acv2$acv <- acv2$x / sum (store$samt) * 100
  summary (acv2$acv)
  names (acv2) [1:3] <- c("barcode", "x2", "acvk")
  acv4 <- left_join (s, acv2, by = "barcode")
  ##建模一般公共参数模型
  acv4$facvk<-acv4$acvk^2
  lm1<-lm(acv4$sk~acv4$acvk+acv4$facv)
  summary(lm1)
  result1 <- summary(lm1)
  
  {#品类模型####
    ##品类级别分销-份额关系（年度）
    pinlei<-unique(df$retailtypename)
    df1 <- subset (df, retailtypename == pinlei[1])
    
    ##计算每个sku全年销售额
    s <- aggregate (df1$amt, by = list (df1$barcode), sum)
    names(s)[1:2] <- c("barcode","amt")
    
    ##每个sku的市场份额
    s$sk <- s$amt / sum (s$amt) * 100
    summary (s$sk)
    
    ##计算每个sku的铺货（加权）
    ###每个商店销售每个sku的销售额
    acv <- aggregate (df1$amt, by = list (df1$storecode, df1$barcode), sum)
    names(acv)[1:3] <- c ("storecode", "barcode", "gamt")
    
    ###每个商店的销售总额
    store<-aggregate (df$amt, by = list(df$storecode), sum)
    names(store)[1:2] <- c("storecode", "samt")
    acv1<- left_join (acv, store, by = "storecode")
    
    ###每个sku所在商店的销售额
    acv2 <- aggregate (acv1$samt, by = list (acv1$barcode), sum)
    
    ##每个sku的%acv
    acv2$acv <- acv2$x / sum (store$samt) * 100
    summary (acv2$acv)
    names (acv2) [1:3] <- c("barcode", "x2", "acvk")
    acv4 <- left_join (s, acv2, by = "barcode")
    ##建模
    acv4$facvk<-acv4$acvk^2
    lmp1<-lm(acv4$sk~acv4$acvk+acv4$facv)
    summary(lmp1)
    ##建模
    result1 <- c()
    for (i in 1:24){
      df1 <- subset (df, retailtypename == pinlei[i])
      
      ##计算每个sku全年销售额
      s <- aggregate (df1$amt, by = list (df1$barcode), sum)
      names(s)[1:2] <- c("barcode","amt")
      
      ##每个sku的市场份额
      s$sk <- s$amt / sum (s$amt) * 100
      summary (s$sk)
      
      ##计算每个sku的铺货（加权）
      ###每个商店销售每个sku的销售额
      acv <- aggregate (df1$amt, by = list (df1$storecode, df1$barcode), sum)
      names(acv)[1:3] <- c ("storecode", "barcode", "gamt")
      
      ###每个商店的销售总额
      store<-aggregate (df$amt, by = list(df$storecode), sum)
      names(store)[1:2] <- c("storecode", "samt")
      acv1<- left_join (acv, store, by = "storecode")
      
      ###每个sku所在商店的销售额
      acv2 <- aggregate (acv1$samt, by = list (acv1$barcode), sum)
      
      ##每个sku的%acv
      acv2$acv <- acv2$x / sum (store$samt) * 100
      summary (acv2$acv)
      names (acv2) [1:3] <- c("barcode", "x2", "acvk")
      acv4 <- left_join (s, acv2, by = "barcode")
      ##二次项
      acv4$facvk <- acv4$acvk^2
      fit <- lm (acv4$sk ~ acv4$acvk + acv4$facvk)
      result1 <- rbind (result1, coef(summary(fit))[, c(1, 2, 4)])
    }
    
  }
  
}

{#一般参数模型与红雨结果一致####
  #计算年度份额
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
}

{#品类模型与红雨一致####
  #不能用原来的lm_data，sku的份额要按品类算

  #计算年度份额
  gc()
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","retailtypename")]
  sku_data
  sku_data <- sku_data %>% group_by(retailtypename) %>% mutate(retailamt=sum(sku_amt))
  sku_data
  sku_data$share <- 100 * sku_data$sku_amt / sku_data$retailamt
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
  
  {#原文品类模型####
    lm_data
    
    #加虚拟变量
    retailtypename
    lm_data2 <- lm_data[,-c(1,3,4)]
    
    #一次虚拟变量的尝试
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
    
    #生成0变量，根据品类赋值1
    m <-nrow(lm_data)
    x <- matrix(0,m,48)
    x <- as.data.table(x)
    y <-c(x,lm_data)
    y <- as.data.table(y)
    head(y)
    x <- as.matrix(x)#将x转化为矩阵是因为data.table结构的数据，无法用DT[,i]取得列变量
    
    j <-seq(1,48,by=2)
    retailtypename <- rep(retailtypename,each=2)#由于后续重复赋值，48个变量只取一半，所以这里将品类名称重复2次。
    for(i in j){
      w <- which(y$retailtypename==retailtypename[i])
      x[w , i] <- 1
      x[, i+1] <- x[, i]
      x[, i] <- x[, i] * y$sku_distribution
      x[, i+1] <- x[, i+1] * y$sku_distribution2
    }
    
    lm1 <- lm(lm_data$share~x)
    summary(lm1)
    #列的命名
    retailtypename
    xtable(summary(lm1))
    
    j <-seq(1,48,by=2)
    beta <- coef(lm1)
    beta <- beta[-1]
    pvalue <- coef(summary(lm1))[,4]
    pvalue <- pvalue[-1]
    result <- c()
    result1 <- c()
    zz <- prettify(summary(lm1),signif.stars = getOption("show.signif.stars"))#用到显著性的星号
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