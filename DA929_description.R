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
}

{#无模型证据-季度####
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
  barcode <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
  lm_data <- left_join(lm_data,barcode,by="barcode")
  
  retail_amt <- df[,c(.(amt=sum(amt))),by="retailtypename"]
  retail_amt <- arrange(retail_amt,desc(amt))
  retail_amt
  nm <- retail_amt$retailtypename[1:4]
  
  test <- subset(lm_data,retailtypename==nm[4])
  test
  ggplot(test,aes(sku_distribution,share))+geom_point()+theme_bw()
  
  }

{#无模型证据-年度####
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
  barcode <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
  lm_data <- left_join(lm_data,barcode,by="barcode")
  
  retail_amt <- df[,c(.(amt=sum(amt))),by="retailtypename"]
  retail_amt <- arrange(retail_amt,desc(amt))
  retail_amt
  nm <- retail_amt$retailtypename[c(1,3,4,5)]
  nm
  
  test <- subset(lm_data,retailtypename==nm[1])
  test
  ggplot(test,aes(sku_distribution,share))+geom_point()+theme_bw()
  for (i in 1:4) {
    df1 <- subset(lm_data,retailtypename==nm[i])
    #save the plot
    png(filename = paste0("type",nm[i],i, ".jpg"),width = 2400,height = 1800,res = 300)
    print(ggplot(data = df1, aes(x = sku_distribution, y = share)) + geom_point()+theme_bw())
    dev.off()
  }
  
}


{#一般参数模型####
  #计算季度份额
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
    sku_data
    ms <- sku_data[,c(.(ms_avg=mean(share)),.(ms_med=median(share)),.(ms_max=max(share))),by="retailtypename"]
    ms#2
    smr_information <- left_join(cat,ms,by="retailtypename")
    smr_information <- left_join(smr_information,acv,by="retailtypename")
    smr_information <- arrange(smr_information,retailtypename)
    
    smr_information
    max(smr_information$amt)#/10000
    smr_information$amt <- smr_information$amt / 10000
    #计算平均ms的倍数
    mean(smr_information$ms_max / smr_information$ms_avg)
    mean(smr_information$ms_avg / smr_information$ms_med)
    
    #计算平均acv的倍数
    mean(smr_information$acv_max / smr_information$acv_avg)
    mean(smr_information$acv_avg / smr_information$acv_med)
    
    #看优秀的sku与一般sku
    lm_data
    lm_data %>% arrange(.,desc(share))#max 4.2 ms 18.3 distribution 
    summary(lm_data$share)
    w <- which(lm_data$share < 0.009)
    sum(lm_data[w,]$share)
    
    
    
    qwrite.csv(smr_information,file = "D:/D/data/xuhuibusiness/smr_information.csv",row.names = F)
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
  
  {#整体做
    lm1 <- lm(lm_data$share~lm_data$retailtypename * (lm_data$sku_distribution+lm_data$sku_distribution2))
    summary(lm1)
  }
}

{#品类特征模型-原始####
  #认为每个品类中的特征，现在考虑应该是按照特征区分为几种品类
  unique(df$retailtypename)
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
  
  #计算品类特征-size
  cat <- df[,c(.(amt=sum(amt))),by="retailtypename"]
  w <- which(cat$amt <= 20000000 & cat$amt > 4000000)
  cat$size <- "large" 
  cat[w,]$size <- "medium"
  w <- which(cat$amt <= 4000000)
  cat[w,]$size <- "small"
  cat
  
  #计算品类特征-HII
  hii <- df[,c(.(amt=sum(amt)),.(retailtypename=unique(retailtypename)),.(mprice=mean(price))),by="barcode"]
  hii
  retail_amt <- df[,c(.(amt=sum(amt))),by="retailtypename"]
  retail_amt
  hii <- left_join(hii,retail_amt,by="retailtypename")
  head(hii)
  hii$share <-  hii$amt.x / hii$amt.y
  hii$share2 <- hii$share ^ 2
  head(hii)
  {##计算品类特征- value density
    #去品类份额最大的sku的价格
    hii <- arrange(hii,desc(share))#先按照share排序
    head(hii)
    hii <- as.data.table(hii)
    vd <- hii[,.SD[1],by="retailtypename"]#取每个品类的最大值
    vd <- vd[,c(1,4)]
    vd
  }
  
  hii<- hii[,c(.(HII=sum(share2))),by="retailtypename"]
  hii
  hii <- left_join(hii,vd,by="retailtypename")#结合vd
  hii <- left_join(hii,cat,by="retailtypename")#结合cat
  hii <- hii[,-4]
  hii
  
  #回归数据
  lm_data <- left_join(sku_data,distribution,by=c("quarter","barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  #lm1 <- lm(share~sku_distribution+sku_distribution2+retailtypename,data=lm_data)#好奇看了一下
  #summary(lm1)
  
  #
  test <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
  test
  
  #结合特征数据
  lm_data <- left_join(lm_data,test,by="barcode")
  lm_data <- left_join(lm_data,hii,by="retailtypename")
  lm_data
  #write.csv(lm_data,file = "D:/D/data/xuhuibusiness/model_cat.csv",row.names = F)
  
  retailtypename <- unique(df$retailtypename)
  retailtypename <- retailtypename[order(retailtypename)]
  retailtypename 
  
  {#测试回归####
    lm_data <- fread("model_cat.csv",header = T)
    lm_data <- lm_data[,-c(3:4)]
    lm_data
    lm_data$size <- as.factor(lm_data$size)
    #retailtypename <- unique(lm_data$retailtypename)
    df1 <- subset(lm_data,retailtypename==retailtypename[3])
    df1 
    df1$c11 <- df1$HII * df1$sku_distribution
    df1$c12 <- df1$HII * df1$sku_distribution2
    df1$c21 <- df1$mprice * df1$sku_distribution
    df1$c22 <- df1$mprice * df1$sku_distribution2 
    #lm1 <- lm(df1$share~df1$HII * (df1$sku_distribution+df1$sku_distribution2) + df1$mprice * (df1$sku_distribution+df1$sku_distribution2) + df1$size * (df1$sku_distribution+df1$sku_distribution2))
    lm1 <- lm(df1$share~df1$c11 +df1$c12 )
    summary(lm1)
    lm1 <- lm(df1$share~df1$c21+ df1$c22)
    summary(lm1)
    lm1 <- lm(df1$share~(df1$sku_distribution+df1$sku_distribution2))
    summary(lm1)
    }
  
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
  
{#品类特征模型-修正####
  #认为每个品类中的特征，现在考虑应该是按照特征区分为几种品类
  unique(df$retailtypename)
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
  
  #计算品类特征-size；retailtypename 改为 
  #food0 开包即食类(retailtypename[c(1,2,13,15,16,17,18)])、
  #food1 加工即食类(retailtypename[c(3,9,10,13,23,24)])、
  #household 家庭/清洁类(retailtypename[c(8,19,20)])、
  #wear 服装(retailtypename[c(11,12,22)])、
  #person 个人护理(retailtypename[c(5)])、
  #mark 化妆(retailtypename[c(7)])、
  #other 其他(retailtypename[c(4,6,14,21)])
  {
    type <- as.data.frame(retailtypename)
    type$typename <- "a"
    w <- c(1,2,13,15,16,17,18)
    type[w,]$typename <- "food0"
    w <- c(3,9,10,13,23,24)
    type[w,]$typename <- "food1"
    w <- c(8,19,20)
    type[w,]$typename <- "household"
    w <- c(11,12,22)
    type[w,]$typename <- "wear"
    w <- c(7)
    type[w,]$typename <- "mark"
    w <- c(4,6,14,21)
    type[w,]$typename <- "other"
    type
  }
  
  {#
    w <- which(df$retailtypename %in% retailtypename[c(1,2,13,15,16,17,18)])
    df[w,]$retailtypename <- "food0"
    gc()
    w <- which(df$retailtypename %in% retailtypename[c(3,9,10,13,23,24)])
    df[w,]$retailtypename <- "food1"
    gc()
    w <- which(df$retailtypename %in% retailtypename[c(8,19,20)])
    df[w,]$retailtypename <- "household"
    gc()
    w <- which(df$retailtypename %in% retailtypename[c(11,12,22)])
    df[w,]$retailtypename <- "wear"
    gc()
    w <- which(df$retailtypename %in% retailtypename[5])
    df[w,]$retailtypename <- "person"
    gc()
    w <- which(df$retailtypename %in% retailtypename[7])
    df[w,]$retailtypename <- "mark"
    gc()
    w <- which(df$retailtypename %in% retailtypename[c(4,6,14,21)])
    df[w,]$retailtypename <- "other"
    gc()
  }
  cat <- df[,c(.(amt=sum(amt))),by="retailtypename"]
  summary(cat$amt)
  w <- which(cat$amt <= 50000000 & cat$amt > 5000000)
  cat$size <- "large" 
  cat[w,]$size <- "medium"
  w <- which(cat$amt <= 5000000)
  cat[w,]$size <- "small"
  cat
  
  #计算品类特征-HII
  hii <- df[,c(.(amt=sum(amt)),.(retailtypename=unique(retailtypename)),.(mprice=mean(price))),by="barcode"]
  hii
  retail_amt <- df[,c(.(amt=sum(amt))),by="retailtypename"]
  retail_amt
  hii <- left_join(hii,retail_amt,by="retailtypename")
  head(hii)
  hii$share <-  hii$amt.x / hii$amt.y
  hii$share2 <- hii$share ^ 2
  head(hii)
  {##计算品类特征- value density
    #去品类份额最大的sku的价格
    hii <- arrange(hii,desc(share))#先按照share排序
    head(hii)
    hii <- as.data.table(hii)
    vd <- hii[,.SD[1],by="retailtypename"]#取每个品类的最大值
    vd
    vd <- vd[,c(1,4)]
    vd
  }
  
  hii<- hii[,c(.(HII=sum(share2))),by="retailtypename"]
  hii
  hii <- left_join(hii,vd,by="retailtypename")#结合vd
  hii <- left_join(hii,cat,by="retailtypename")#结合cat
  hii
  hii <- hii[,-4]
  hii
  
  #回归数据
  lm_data <- left_join(sku_data,distribution,by=c("quarter","barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  
  #
  test <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
  test
  
  #结合特征数据
  lm_data <- left_join(lm_data,test,by="barcode")
  lm_data <- left_join(lm_data,hii,by="retailtypename")
  lm_data
  #write.csv(lm_data,file = "D:/D/data/xuhuibusiness/model_cat.csv",row.names = F)
  
  retailtypename <- unique(df$retailtypename)
  retailtypename <- retailtypename[order(retailtypename)]
  retailtypename
  
  {#测试回归####
    lm_data <- lm_data[,-c(3:4)]
    lm_data
    lm_data$size <- as.factor(lm_data$size)
    #retailtypename <- unique(lm_data$retailtypename)
    df1 <- subset(lm_data,retailtypename == retailtypename[1])
    df1 
    lm1 <- lm(df1$share~df1$sku_distribution + df1$sku_distribution2)
    summary(lm1)
  
    df1$c11 <- df1$HII * df1$sku_distribution
    df1$c12 <- df1$HII * df1$sku_distribution2
    df1$c21 <- df1$mprice * df1$sku_distribution
    df1$c22 <- df1$mprice * df1$sku_distribution2 
    #lm1 <- lm(df1$share~df1$HII * (df1$sku_distribution+df1$sku_distribution2) + df1$mprice * (df1$sku_distribution+df1$sku_distribution2) + df1$size * (df1$sku_distribution+df1$sku_distribution2))
    lm1 <- lm(df1$share~df1$c11 +df1$c12 )
    summary(lm1)
    lm1 <- lm(df1$share~df1$c21+ df1$c22)
    summary(lm1)
    lm1 <- lm(df1$share~(df1$sku_distribution+df1$sku_distribution2))
    summary(lm1)
  }
  
  result1 <- c()
  result <- c()
  for(i in 1:7){
    df1 <- subset(lm_data,retailtypename==retailtypename[i])
    lm1 <- lm(share~sku_distribution+sku_distribution2,data=df1)
    result1 <- data.frame(cat=retailtypename[i],main_effect=coef(lm1)[1],t_value=coef(summary(lm1))[,3][1],acv=coef(lm1)[2],t_value=coef(summary(lm1))[,3][2],acv2=coef(lm1)[3],t_value2=coef(summary(lm1))[,3][3],row.names = NULL)
    result <- rbind(result,result1)
  }
  result 
  
  #存一个表格形式
  write.csv(result,file="D:/D/data/xuhuibusiness/equation4.csv",row.names = F)
  
  #粘贴，存一个latex形式
  test <- result
  test$cat <- paste("cat type:",test$cat,sep="")
  test$main_effect <- round(test$main_effect,5)
  test$t_value <- round(test$t_value,1)
  test$t_value <- paste("(",test$t_value,")",sep="")
  test$acv <- round(test$acv,5)
  test$t_value.1 <- round(test$t_value.1,1)
  test$t_value.1 <- paste("(",test$t_value.1,")",sep="")
  test$acv2 <- round(test$acv2,5)
  test$t_value2 <- round(test$t_value2,1)
  test$t_value2 <- paste("(",test$t_value2,")",sep="")
  test
  x <- paste(test$cat,test$main_effect,test$t_value,test$acv,test$t_value.1,test$acv2,test$t_value2,sep="&")
  x 
  x <- paste(x,"\\\\",sep="")
  write.csv(x,file = "D:/D/data/xuhuibusiness/table_model3.csv",row.names = F,quote = F)
  
  {#其他特征
    lm_data
    result1 <- c()
    result <- c()
    size <- unique(lm_data$size)
    for(i in 1:3){
      df1 <- subset(lm_data,size==size[i])
      lm1 <- lm(share~sku_distribution+sku_distribution2,data=df1)
      result1 <- data.frame(size=size[i],main_effect=coef(lm1)[1],t_value=coef(summary(lm1))[,3][1],acv=coef(lm1)[2],t_value=coef(summary(lm1))[,3][2],acv2=coef(lm1)[3],t_value2=coef(summary(lm1))[,3][3],row.names = NULL)
      result <- rbind(result,result1)
    }
    result
    write.csv(result,file="D:/D/data/xuhuibusiness/equation4_1.csv",row.names = F)
    
    #粘贴，存一个latex形式
    test <- result
    test$size <- as.character(test$size)
    test[1,]$size <- paste("cat size:",test[1,]$size,"(500-5000M)",sep="")
    test[2,]$size <- paste("cat size:",test[2,]$size,"(5000M+)",sep="")
    test[3,]$size <- paste("cat size:",test[3,]$size,"(500M-)",sep="")
    test$main_effect <- round(test$main_effect,5)
    test$t_value <- round(test$t_value,1)
    test$t_value <- paste("(",test$t_value,")",sep="")
    test$acv <- round(test$acv,5)
    test$t_value.1 <- round(test$t_value.1,1)
    test$t_value.1 <- paste("(",test$t_value.1,")",sep="")
    test$acv2 <- round(test$acv2,5)
    test$t_value2 <- round(test$t_value2,1)
    test$t_value2 <- paste("(",test$t_value2,")",sep="")
    test
    x <- paste(test$size,test$main_effect,test$t_value,test$acv,test$t_value.1,test$acv2,test$t_value2,sep="&")
    x 
    x <- paste(x,"\\\\",sep="")
    write.csv(x,file = "D:/D/data/xuhuibusiness/table_model3_1.csv",row.names = F,quote = F)
  }
  
  {#其他特征2
    lm_data
    lm_data$c2 <- lm_data$share / lm_data$HII
    lm1 <- lm(lm_data$c2~lm_data$sku_distribution + lm_data$sku_distribution2)
    summary(lm1)
    result1 <- data.frame(HII="sku_level HHI",main_effect=coef(lm1)[1],t_value=coef(summary(lm1))[,3][1],acv=coef(lm1)[2],t_value=coef(summary(lm1))[,3][2],acv2=coef(lm1)[3],t_value2=coef(summary(lm1))[,3][3],row.names = NULL)
    result1
    
    #存一个表格形式
    write.csv(result1,file="D:/D/data/xuhuibusiness/equation4_2.csv",row.names = F)
    
    zz <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
    zz <- left_join(lm_data,zz,by="barcode")
    zz
    write.csv(zz,file="D:/D/data/xuhuibusiness/lm_data.csv",row.names = F)
    
    #存一个latex形式
    test <- result1
    test$HII <- paste("cat competitive:",test$HII)
    test$main_effect <- round(test$main_effect,5)
    test$t_value <- round(test$t_value,1)
    test$t_value <- paste("(",test$t_value,")",sep="")
    test$acv <- round(test$acv,5)
    test$t_value.1 <- round(test$t_value.1,1)
    test$t_value.1 <- paste("(",test$t_value.1,")",sep="")
    test$acv2 <- round(test$acv2,5)
    test$t_value2 <- round(test$t_value2,1)
    test$t_value2 <- paste("(",test$t_value2,")",sep="")
    test
    y <- paste(test$HII,test$main_effect,test$t_value,test$acv,test$t_value.1,test$acv2,test$t_value2,sep="&")
    y 
    y <- paste(y,"\\\\",sep="")
    x <- c(x,y)
    write.csv(x,file = "D:/D/data/xuhuibusiness/table_model3_1.csv",row.names = F,quote = F)
  }
  
  {#其他特征3
    summary(lm_data$mprice)
    w <- which(lm_data$mprice > 77.18)#第三分位数
    lm_data$vd <- "low"
    lm_data[w,]$vd <- "high"
    
    #存一个表格形式
    zz <-subset(lm_data,vd=="high")
    lm1 <- lm(zz$share~zz$sku_distribution + zz$sku_distribution2)
    summary(lm1)
    result1 <- data.frame(vd="high",main_effect=coef(lm1)[1],t_value=coef(summary(lm1))[,3][1],acv=coef(lm1)[2],t_value=coef(summary(lm1))[,3][2],acv2=coef(lm1)[3],t_value2=coef(summary(lm1))[,3][3],row.names = NULL)
    result1
    result <- result1
    zz <-subset(lm_data,vd=="low")
    lm1 <- lm(zz$share~zz$sku_distribution + zz$sku_distribution2)
    summary(lm1)
    result1 <- data.frame(vd="low",main_effect=coef(lm1)[1],t_value=coef(summary(lm1))[,3][1],acv=coef(lm1)[2],t_value=coef(summary(lm1))[,3][2],acv2=coef(lm1)[3],t_value2=coef(summary(lm1))[,3][3],row.names = NULL)
    result1
    result <- rbind(result,result1)
    result
    write.csv(result,file="D:/D/data/xuhuibusiness/equation4_3.csv",row.names = F)
    
    #存一个latex文本形式
    test <- result1
    test$vd <- paste("cat value density:",test$vd)
    test$main_effect <- round(test$main_effect,5)
    test$t_value <- round(test$t_value,1)
    test$t_value <- paste("(",test$t_value,")",sep="")
    test$acv <- round(test$acv,5)
    test$t_value.1 <- round(test$t_value.1,1)
    test$t_value.1 <- paste("(",test$t_value.1,")",sep="")
    test$acv2 <- round(test$acv2,5)
    test$t_value2 <- round(test$t_value2,1)
    test$t_value2 <- paste("(",test$t_value2,")",sep="")
    test
    z <- paste(test$vd,test$main_effect,test$t_value,test$acv,test$t_value.1,test$acv2,test$t_value2,sep="&")
    z 
    z <- paste(z,"\\\\",sep="")
    x <- c(x,z)
    x
    write.csv(x,file = "D:/D/data/xuhuibusiness/table_model3_1.csv",row.names = F,quote = F)
  }

}

{#具体品类中的领导者品牌模型####
  
}
