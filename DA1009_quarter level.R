{
  ###1.基本设置####
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

{#一般参数模型-季度####
  #计算季度份额
  gc()
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","quarter","retailtypename")]
  sku_data <- sku_data %>% group_by(retailtypename) %>% mutate(retailamt=sum(sku_amt))
  sku_data$share <- 100 * sku_data$sku_amt / sku_data$retailamt
  head(sku_data)
  
  #计算加权铺货
  distribution <- df[,c(.(sku_amt=sum(amt)),.(barcode=unique(barcode))),by=c("storecode","quarter")]#计算每个店铺的总销售额与barcode连接
  distribution <- distribution %>% group_by(barcode,quarter) %>%
    mutate(store_amt=sum(sku_amt))#以barcode与季度为分组依据，计算每个barcode的所有销售门店的总销售额
  head(distribution)
  distribution$sku_distribution <- 100 * distribution$store_amt / sum(df$amt)
  distribution <- distribution %>% select(.,c(quarter,barcode,sku_distribution))
  distribution <- unique(distribution)
  distribution
  
  #回归数据
  lm_data <- left_join(sku_data,distribution,by=c("barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  summary(lm_data$sku_distribution)
  model1 <- lm(share~sku_distribution+sku_distribution2,data=lm_data)
  summary(model1) 
  
  #输出
  model1 <- summary(lm1)
  model1 <- prettify(model1)
  model1$Estimate <- round(model1$Estimate,5)
  model1
  output1 <- data.frame(cat="common model",acv=model1$Estimate[2],pvalue=model1$`Pr(>|t|)`[2],acv2=model1$Estimate[3],pvalue2=model1$`Pr(>|t|)`[3],star1=model1$`   `[2],star2=model1$`   `[3])
  output1
  output1$acv <- paste(output1$acv,output1$star1,sep="")
  output1$acv2 <- paste(output1$acv2,output1$star2,sep="")
  output1 <- output1[,-c(6,7)]
  output1
  xtable(output1)#输出为latex
  write.csv(output1,file = "D:/D/data/xuhuibusiness/output1.csv",row.names = F) #输出为table
  
}


{#品类模型-季度####
  #lm_data，sku的份额要按品类算
  
  #计算季度份额
  gc()
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","retailtypename","quarter")]
  sku_data
  sku_data <- sku_data %>% group_by(retailtypename,quarter) %>% mutate(retailamt=sum(sku_amt))
  sku_data
  sku_data$share <- 100 * sku_data$sku_amt / sku_data$retailamt
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
  lm_data <- left_join(sku_data,distribution,by=c("barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  
  #
  retailtypename <- unique(df$retailtypename)
  retailtypename 
  
  {#原文品类模型####
    lm_data
    
    #加虚拟变量
    retailtypename
  
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
    
    model2 <- lm(lm_data$share~x)
    summary(model2)
    #列的命名
    retailtypename
    
    #输出结果
    j <-seq(1,48,by=2)
    result <- c()
    result1 <- c()
    zz <- prettify(summary(model2),signif.stars = getOption("show.signif.stars"))#用到显著性的星号
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
    xtable(output2)#输出为latex
    write.csv(output2,file = "D:/D/data/xuhuibusiness/output2.csv",row.names = F) #输出为table
    
  }
}


{#品类特征模型####
  #认为每个品类中的特征，现在考虑应该是按照特征区分为几种品类
  gc()
  unique(df$retailtypename)
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","retailtypename","quarter")]
  sku_data
  sku_data <- sku_data %>% group_by(retailtypename,quarter) %>% mutate(retailamt=sum(sku_amt))
  sku_data
  sku_data$share <- 100 * sku_data$sku_amt / sku_data$retailamt
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
  lm_data <- left_join(sku_data,distribution,by=c("barcode","quarter"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  
  #回归数据连接品类
  retail <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
  retail
  lm_data <- left_join(lm_data,retail,by=c("barcode","retailtypename"))
  lm_data
  
  #计算品类特征-cat_type；retailtypename 改为 
  #food0 开包即食类(retailtypename[c(1,2,15,16,17,18)])、
  #food1 加工即食类(retailtypename[c(3,9,10,13,23,24)])、
  #household 家庭/清洁类(retailtypename[c(8,19,20)])、
  #wear 服装(retailtypename[c(11,12,22)])、
  #person 个人护理(retailtypename[c(5)])、
  #mark 化妆(retailtypename[c(7)])、
  #other 其他(retailtypename[c(4,6,14,21)])
  retailtypename <- unique(df$retailtypename)
  retailtypename <- sort(retailtypename)
  lm_data$type <- "food0"
  w <- which(lm_data$retailtypename %in% retailtypename[c(3,9,10,13,23,24)])
  lm_data$type[w] <- "food1"
  w <- which(lm_data$retailtypename %in% retailtypename[c(8,19,20)])
  lm_data$type[w] <- "household"
  w <- which(lm_data$retailtypename %in% retailtypename[c(11,12,22)])
  lm_data$type[w] <- "wear"
  w <- which(lm_data$retailtypename %in% retailtypename[c(5)])
  lm_data$type[w] <- "person"
  w <- which(lm_data$retailtypename %in% retailtypename[c(7)])
  lm_data$type[w] <- "mark"
  w <- which(lm_data$retailtypename %in% retailtypename[c(4,6,14,21)])
  lm_data$type[w] <- "other"
  lm_data
  
  #品类特征cat_size
  cat <- df[,c(.(amt=sum(amt))),by="retailtypename"]
  summary(cat$amt)
  w <- which(cat$amt <= 15947831 & cat$amt > 3709005 )
  cat$size <- "large" 
  cat[w,]$size <- "medium"
  w <- which(cat$amt <= 3709005)
  cat[w,]$size <- "small"
  cat
  
  #连接品类特征cat_size
  lm_data
  lm_data <- left_join(lm_data,cat,by="retailtypename")
  lm_data
  
  #删除不必要变量
  lm_data <- lm_data[,-c(4,5,10)]
  lm_data
  
  #计算品类特征-HHI-sku level
  hhi <- df[,c(.(amt=sum(amt)),.(retailtypename=unique(retailtypename)),.(mprice=mean(price))),by="barcode"]
  hhi
  hhi <- hhi %>% group_by(retailtypename) %>% mutate(retail_amt=sum(amt))
  head(hhi)
  hhi$share <- hhi$amt / hhi$retail_amt
  hhi$share2 <- hhi$share ^ 2
  head(hhi)
  hhi <- hhi %>% group_by(retailtypename) %>% mutate(sku_HHI = sum(share2))
  hhi
  
  #计算品类特征-value_density：the typical SKU price: volume ratio?
  #不理解先按自己的理解算一个
  hhi <- arrange(hhi,desc(amt))#为找出销售额最大的SKU，先按照销售额从大到小排列
  hhi <- as.data.table(hhi)
  typical <- hhi[, .SD[1],by="retailtypename"]#提取出每个品类的第一个即可
  typical$value_density <- typical$mprice / typical$retail_amt
  typical <- typical[,-c(2:8)]
  typical
  
  #连接hhi
  hhi <- left_join(hhi,typical,by="retailtypename")
  hhi
  
  #删除hhi中的不必要变量
  hhi <- hhi[,-c(2,4,5,6,7)]
  head(hhi)#12046
  
  #连接lm_data 38924
  lm_data
  lm_data <- left_join(lm_data,hhi,by=c("barcode","retailtypename"))
  lm_data
   
  #回归模型数据准备
  length(unique(lm_data$type))#7
  length(unique(lm_data$size))#3
  type <- unique(lm_data$type)
  size <- unique(lm_data$size)

  
  #与品类模型类似，只不过品类换成了type size这些自己定义的品类特征变量
  #生成0变量，根据品类特征赋值1-type
  m <-nrow(lm_data)
  x <- matrix(0,m,14)
  x <- as.data.table(x)
  y <-c(x,lm_data)
  y <- as.data.table(y)
  head(y)
  x <- as.matrix(x)#将x转化为矩阵是因为data.table结构的数据，无法用DT[,i]取得列变量
  
  j <-seq(1,14,by=2)
  retailtypename <- type
  retailtypename <- rep(retailtypename,each=2)#由于后续重复赋值，14个变量只取一半，所以这里将品类名称重复2次。
  for(i in j){
    w <- which(y$type==retailtypename[i])
    x[w , i] <- 1
    x[, i+1] <- x[, i]
    x[, i] <- x[, i] * y$sku_distribution
    x[, i+1] <- x[, i+1] * y$sku_distribution2
  }
  
  lm1 <- lm(lm_data$share~x)
  summary(lm1)
  
  #生成0变量，根据品类特征赋值1-size
  size
  m <-nrow(lm_data)
  xx <- matrix(0,m,6)
  xx <- as.data.table(xx)
  y <-c(xx,lm_data)
  y <- as.data.table(y)
  head(y)
  xx <- as.matrix(xx)#将x转化为矩阵是因为data.table结构的数据，无法用DT[,i]取得列变量
  
  j <-seq(1,6,by=2)
  retailtypename <- size
  retailtypename <- rep(retailtypename,each=2)#由于后续重复赋值，48个变量只取一半，所以这里将品类名称重复2次。
  for(i in j){
    w <- which(y$size==retailtypename[i])
    xx[w , i] <- 1
    xx[, i+1] <- xx[, i]
    xx[, i] <- xx[, i] * y$sku_distribution
    xx[, i+1] <- xx[, i+1] * y$sku_distribution2
  }
  
  colnames(xx) <- c("s1","s2","s3","s4","s5","s6")
  xxx <-cbind(x,xx)
  nrow(xxx)
  
  model3 <- lm(lm_data$share~xxx)
  summary(model3)
  #列的命名type size
  
  #输出结果
  j <-seq(1,18,by=2)
  result <- c()
  result1 <- c()
  zz <- prettify(summary(model3),signif.stars = getOption("show.signif.stars"))#用到显著性的星号
  zz <- zz[-1,]
  retailtypename <- c(type,size)
  retailtypename <- rep(retailtypename,each=2)
  for(i in j){
    result1 <- data.frame(cat=retailtypename[i],acv=zz$Estimate[i],p_value=zz$`Pr(>|t|)`[i],acv2=zz$Estimate[i+1],p_value2=zz$`Pr(>|t|)`[i+1],star1=zz$`   `[i],star2=zz$`   `[i+1],row.names = NULL)
    result <- rbind(result,result1)
  }
  result
  result$acv <-round(result$acv,5)
  result$acv2 <- round(result$acv2,5)
  result
  
  output3 <- result
  output3$acv <- paste(output3$acv,output3$star1,sep="")
  output3$acv2 <- paste(output3$acv2,output3$star2,sep="")
  output3
  output3 <- output3[,-c(6,7)]
  xtable(output3)#输出为latex
  write.csv(output3,file = "D:/D/data/xuhuibusiness/output3.csv",row.names = F) #输出为table
  
  summary(model3)
  
  
  
  
  
}


{#品牌模型####
  #计算季度份额
  gc()
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","retailtypename","quarter")]
  sku_data
  sku_data <- sku_data %>% group_by(retailtypename,quarter) %>% mutate(retailamt=sum(sku_amt))
  sku_data
  sku_data$share <- 100 * sku_data$sku_amt / sku_data$retailamt
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
  
  #品牌占品类的份额
  brand <- df[,c(.(amt=sum(amt))),by=c("brandname","retailtypename","quarter")]
  brand <- brand %>% group_by(retailtypename,quarter) %>% mutate(retail_amt=sum(amt))
  brand$share <- brand$amt / brand$retail_amt
  brand
  brand <- arrange(brand,desc(share))
  brand
  
  
  #品类中每个品牌份额的排名
  brand <- brand %>% group_by(retailtypename,quarter) %>% mutate(rank=1:length(brandname))
  brand
  arrange(brand,retailtypename,quarter)
  
  #lm_data连接品牌与barcode与季度-1step
  barcode <- df[,c(.(barcode=unique(barcode))),by="brandname"]
  barcode
  lm_data <- left_join(lm_data,barcode,by="barcode")
  lm_data
  
  #lm_data连接品牌与barcode与季度-2step
  lm_data <- left_join(lm_data,brand,by=c("brandname","quarter","retailtypename"))
  lm_data
  
  #排名第9以后的通称为”第十“
  w <- which(lm_data$rank > 9)
  lm_data$rank[w] <- 10
  lm_data
  
  #
  #与品类模型类似，只不过品类换成了rank这些自己定义的品牌份额+品类排名
  lm_data$catrank <- paste(lm_data$retailtypename,lm_data$rank,sep="")
  lm_data
  length(unique(lm_data$catrank))#210
  
  #生成0变量，根据catrank赋值1
  m <-nrow(lm_data)
  x <- matrix(0,m,420)#210种排名20列
  x <- as.data.table(x)
  y <-c(x,lm_data)
  y <- as.data.table(y)
  head(y)
  x <- as.matrix(x)#将x转化为矩阵是因为data.table结构的数据，无法用DT[,i]取得列变量
  
  
  j <-seq(1,420,by=2)
  retailtypename <- unique(lm_data$catrank)
  retailtypename <- rep(retailtypename,each=2)#由于后续重复赋值，20个变量只取一半，所以这里将rank称重复2次。
  colnames(x)<-retailtypename
  
  for(i in j){
    w <- which(y$catrank==retailtypename[i])
    x[w , i] <- 1
    x[, i+1] <- x[, i]
    x[, i] <- x[, i] * y$sku_distribution
    x[, i+1] <- x[, i+1] * y$sku_distribution2
  }
  
  model4 <- lm(lm_data$share.x~x)
  summary(model4)
  
  #输出结果
  j <-seq(1,420,by=2)
  result <- c()
  result1 <- c()
  zz <- prettify(summary(model4),signif.stars = getOption("show.signif.stars"))#用到显著性的星号
  zz <- zz[-1,]
  zz
  
  retailtypename <- unique(lm_data$catrank)
  retailtypename <- rep(retailtypename,each=2)#由于后续重复赋值，20个变量只取一半，所以这里将catrank称重复2次。
  for(i in j){
    result1 <- data.frame(cat=retailtypename[i],acv=zz$Estimate[i],p_value=zz$`Pr(>|t|)`[i],acv2=zz$Estimate[i+1],p_value2=zz$`Pr(>|t|)`[i+1],star1=zz$`   `[i],star2=zz$`   `[i+1],row.names = NULL)
    result <- rbind(result,result1)
  }
  result
  result$acv <-round(result$acv,5)
  result$acv2 <- round(result$acv2,5)
  result
  
  output4 <- result
  output4$acv <- paste(output4$acv,output4$star1,sep="")
  output4$acv2 <- paste(output4$acv2,output4$star2,sep="")
  output4
  output4 <- output4[,-c(6,7)]
  xtable(output4)#输出为latex
  write.csv(output4,file = "D:/D/data/xuhuibusiness/output4.csv",row.names = F) #输出为完整版table
  
  summary(model4)
  head(output4)
  
  #输出1-3+9:提取排名
  catrank <- lm_data[,c("rank","catrank")]
  catrank <- unique(catrank)
  catrank
  names(catrank) <- c("rank","cat")
  
  #输出1-3+9:连接排名
  output4 <- left_join(output4,catrank,by="cat")
  head(output4)
  
  ##输出1-3+9
  output4_paper <- subset(output4,rank %in% c(1,2,3,9))
  output4_paper
  
  #提取品类名称
  w <- regexpr("[1-9]",output4_paper$cat)
  w
  output4_paper$cat2 <- substr(output4_paper$cat,1,w-1)
  output4_paper
  
  #分组输出
  rank1 <- subset(output4_paper,rank==1)
  rank1 <- rank1[,-c(1,6)]
  rank2 <- subset(output4_paper,rank==2)
  rank2 <- rank2[,-c(1,6)]
  rank3 <- subset(output4_paper,rank==3)
  rank3 <- rank3[,-c(1,6)]
  rank9 <- subset(output4_paper,rank==9)
  rank9 <- rank9[,-c(1,6)]
  
  rank1239 <- left_join(rank1,rank2,by="cat2")
  rank1239 <- left_join(rank1239,rank3,by="cat2")
  rank1239 <- left_join(rank1239,rank9,by="cat2")
  rank1239
  rank1239 <- rank1239 %>% select(c(5,1:4,6:17))
  write.csv(rank1239,file = "D:/D/data/xuhuibusiness/output4_paper.csv",row.names = F)
  xtable(rank1239)
  
  summary(model4)
}

summary(model1)
summary(model2)
summary(model3)
summary(model4)
