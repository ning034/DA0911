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

{#һ�����ģ��-����####
  #���㼾�ȷݶ�
  gc()
  sku_data <- df[,c(.(sku_amt=sum(amt))),by=c("barcode","quarter","retailtypename")]
  sku_data <- sku_data %>% group_by(retailtypename) %>% mutate(retailamt=sum(sku_amt))
  sku_data$share <- 100 * sku_data$sku_amt / sku_data$retailamt
  head(sku_data)
  
  #�����Ȩ�̻�
  distribution <- df[,c(.(sku_amt=sum(amt)),.(barcode=unique(barcode))),by=c("storecode","quarter")]#����ÿ�����̵������۶���barcode����
  distribution <- distribution %>% group_by(barcode,quarter) %>%
    mutate(store_amt=sum(sku_amt))#��barcode�뼾��Ϊ�������ݣ�����ÿ��barcode�����������ŵ�������۶�
  head(distribution)
  distribution$sku_distribution <- 100 * distribution$store_amt / sum(df$amt)
  distribution <- distribution %>% select(.,c(quarter,barcode,sku_distribution))
  distribution <- unique(distribution)
  distribution
  
  #�ع�����
  lm_data <- left_join(sku_data,distribution,by=c("barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  summary(lm_data$sku_distribution)
  model1 <- lm(share~sku_distribution+sku_distribution2,data=lm_data)
  summary(model1) 
  
  #���
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
  xtable(output1)#���Ϊlatex
  write.csv(output1,file = "D:/D/data/xuhuibusiness/output1.csv",row.names = F) #���Ϊtable
  
}


{#Ʒ��ģ��-����####
  #lm_data��sku�ķݶ�Ҫ��Ʒ����
  
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
  lm_data <- left_join(sku_data,distribution,by=c("barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  
  #
  retailtypename <- unique(df$retailtypename)
  retailtypename 
  
  {#ԭ��Ʒ��ģ��####
    lm_data
    
    #���������
    retailtypename
  
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
    write.csv(output2,file = "D:/D/data/xuhuibusiness/output2.csv",row.names = F) #���Ϊtable
    
  }
}


{#Ʒ������ģ��####
  #��Ϊÿ��Ʒ���е����������ڿ���Ӧ���ǰ�����������Ϊ����Ʒ��
  gc()
  unique(df$retailtypename)
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
  lm_data <- left_join(sku_data,distribution,by=c("barcode","quarter"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  
  #�ع���������Ʒ��
  retail <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
  retail
  lm_data <- left_join(lm_data,retail,by=c("barcode","retailtypename"))
  lm_data
  
  #����Ʒ������-cat_type��retailtypename ��Ϊ 
  #food0 ������ʳ��(retailtypename[c(1,2,15,16,17,18)])��
  #food1 �ӹ���ʳ��(retailtypename[c(3,9,10,13,23,24)])��
  #household ��ͥ/�����(retailtypename[c(8,19,20)])��
  #wear ��װ(retailtypename[c(11,12,22)])��
  #person ���˻���(retailtypename[c(5)])��
  #mark ��ױ(retailtypename[c(7)])��
  #other ����(retailtypename[c(4,6,14,21)])
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
  
  #Ʒ������cat_size
  cat <- df[,c(.(amt=sum(amt))),by="retailtypename"]
  summary(cat$amt)
  w <- which(cat$amt <= 15947831 & cat$amt > 3709005 )
  cat$size <- "large" 
  cat[w,]$size <- "medium"
  w <- which(cat$amt <= 3709005)
  cat[w,]$size <- "small"
  cat
  
  #����Ʒ������cat_size
  lm_data
  lm_data <- left_join(lm_data,cat,by="retailtypename")
  lm_data
  
  #ɾ������Ҫ����
  lm_data <- lm_data[,-c(4,5,10)]
  lm_data
  
  #����Ʒ������-HHI-sku level
  hhi <- df[,c(.(amt=sum(amt)),.(retailtypename=unique(retailtypename)),.(mprice=mean(price))),by="barcode"]
  hhi
  hhi <- hhi %>% group_by(retailtypename) %>% mutate(retail_amt=sum(amt))
  head(hhi)
  hhi$share <- hhi$amt / hhi$retail_amt
  hhi$share2 <- hhi$share ^ 2
  head(hhi)
  hhi <- hhi %>% group_by(retailtypename) %>% mutate(sku_HHI = sum(share2))
  hhi
  
  #����Ʒ������-value_density��the typical SKU price: volume ratio?
  #�������Ȱ��Լ���������һ��
  hhi <- arrange(hhi,desc(amt))#Ϊ�ҳ����۶�����SKU���Ȱ������۶�Ӵ�С����
  hhi <- as.data.table(hhi)
  typical <- hhi[, .SD[1],by="retailtypename"]#��ȡ��ÿ��Ʒ��ĵ�һ������
  typical$value_density <- typical$mprice / typical$retail_amt
  typical <- typical[,-c(2:8)]
  typical
  
  #����hhi
  hhi <- left_join(hhi,typical,by="retailtypename")
  hhi
  
  #ɾ��hhi�еĲ���Ҫ����
  hhi <- hhi[,-c(2,4,5,6,7)]
  head(hhi)#12046
  
  #����lm_data 38924
  lm_data
  lm_data <- left_join(lm_data,hhi,by=c("barcode","retailtypename"))
  lm_data
   
  #�ع�ģ������׼��
  length(unique(lm_data$type))#7
  length(unique(lm_data$size))#3
  type <- unique(lm_data$type)
  size <- unique(lm_data$size)

  
  #��Ʒ��ģ�����ƣ�ֻ����Ʒ�໻����type size��Щ�Լ������Ʒ����������
  #����0����������Ʒ��������ֵ1-type
  m <-nrow(lm_data)
  x <- matrix(0,m,14)
  x <- as.data.table(x)
  y <-c(x,lm_data)
  y <- as.data.table(y)
  head(y)
  x <- as.matrix(x)#��xת��Ϊ��������Ϊdata.table�ṹ�����ݣ��޷���DT[,i]ȡ���б���
  
  j <-seq(1,14,by=2)
  retailtypename <- type
  retailtypename <- rep(retailtypename,each=2)#���ں����ظ���ֵ��14������ֻȡһ�룬�������ｫƷ�������ظ�2�Ρ�
  for(i in j){
    w <- which(y$type==retailtypename[i])
    x[w , i] <- 1
    x[, i+1] <- x[, i]
    x[, i] <- x[, i] * y$sku_distribution
    x[, i+1] <- x[, i+1] * y$sku_distribution2
  }
  
  lm1 <- lm(lm_data$share~x)
  summary(lm1)
  
  #����0����������Ʒ��������ֵ1-size
  size
  m <-nrow(lm_data)
  xx <- matrix(0,m,6)
  xx <- as.data.table(xx)
  y <-c(xx,lm_data)
  y <- as.data.table(y)
  head(y)
  xx <- as.matrix(xx)#��xת��Ϊ��������Ϊdata.table�ṹ�����ݣ��޷���DT[,i]ȡ���б���
  
  j <-seq(1,6,by=2)
  retailtypename <- size
  retailtypename <- rep(retailtypename,each=2)#���ں����ظ���ֵ��48������ֻȡһ�룬�������ｫƷ�������ظ�2�Ρ�
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
  #�е�����type size
  
  #������
  j <-seq(1,18,by=2)
  result <- c()
  result1 <- c()
  zz <- prettify(summary(model3),signif.stars = getOption("show.signif.stars"))#�õ������Ե��Ǻ�
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
  xtable(output3)#���Ϊlatex
  write.csv(output3,file = "D:/D/data/xuhuibusiness/output3.csv",row.names = F) #���Ϊtable
  
  summary(model3)
  
  
  
  
  
}


{#Ʒ��ģ��####
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
  lm_data <- left_join(sku_data,distribution,by=c("quarter","barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  
  #Ʒ��ռƷ��ķݶ�
  brand <- df[,c(.(amt=sum(amt))),by=c("brandname","retailtypename","quarter")]
  brand <- brand %>% group_by(retailtypename,quarter) %>% mutate(retail_amt=sum(amt))
  brand$share <- brand$amt / brand$retail_amt
  brand
  brand <- arrange(brand,desc(share))
  brand
  
  
  #Ʒ����ÿ��Ʒ�Ʒݶ������
  brand <- brand %>% group_by(retailtypename,quarter) %>% mutate(rank=1:length(brandname))
  brand
  arrange(brand,retailtypename,quarter)
  
  #lm_data����Ʒ����barcode�뼾��-1step
  barcode <- df[,c(.(barcode=unique(barcode))),by="brandname"]
  barcode
  lm_data <- left_join(lm_data,barcode,by="barcode")
  lm_data
  
  #lm_data����Ʒ����barcode�뼾��-2step
  lm_data <- left_join(lm_data,brand,by=c("brandname","quarter","retailtypename"))
  lm_data
  
  #������9�Ժ��ͨ��Ϊ����ʮ��
  w <- which(lm_data$rank > 9)
  lm_data$rank[w] <- 10
  lm_data
  
  #
  #��Ʒ��ģ�����ƣ�ֻ����Ʒ�໻����rank��Щ�Լ������Ʒ�Ʒݶ�+Ʒ������
  lm_data$catrank <- paste(lm_data$retailtypename,lm_data$rank,sep="")
  lm_data
  length(unique(lm_data$catrank))#210
  
  #����0����������catrank��ֵ1
  m <-nrow(lm_data)
  x <- matrix(0,m,420)#210������20��
  x <- as.data.table(x)
  y <-c(x,lm_data)
  y <- as.data.table(y)
  head(y)
  x <- as.matrix(x)#��xת��Ϊ��������Ϊdata.table�ṹ�����ݣ��޷���DT[,i]ȡ���б���
  
  
  j <-seq(1,420,by=2)
  retailtypename <- unique(lm_data$catrank)
  retailtypename <- rep(retailtypename,each=2)#���ں����ظ���ֵ��20������ֻȡһ�룬�������ｫrank���ظ�2�Ρ�
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
  
  #������
  j <-seq(1,420,by=2)
  result <- c()
  result1 <- c()
  zz <- prettify(summary(model4),signif.stars = getOption("show.signif.stars"))#�õ������Ե��Ǻ�
  zz <- zz[-1,]
  zz
  
  retailtypename <- unique(lm_data$catrank)
  retailtypename <- rep(retailtypename,each=2)#���ں����ظ���ֵ��20������ֻȡһ�룬�������ｫcatrank���ظ�2�Ρ�
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
  xtable(output4)#���Ϊlatex
  write.csv(output4,file = "D:/D/data/xuhuibusiness/output4.csv",row.names = F) #���Ϊ������table
  
  summary(model4)
  head(output4)
  
  #���1-3+9:��ȡ����
  catrank <- lm_data[,c("rank","catrank")]
  catrank <- unique(catrank)
  catrank
  names(catrank) <- c("rank","cat")
  
  #���1-3+9:��������
  output4 <- left_join(output4,catrank,by="cat")
  head(output4)
  
  ##���1-3+9
  output4_paper <- subset(output4,rank %in% c(1,2,3,9))
  output4_paper
  
  #��ȡƷ������
  w <- regexpr("[1-9]",output4_paper$cat)
  w
  output4_paper$cat2 <- substr(output4_paper$cat,1,w-1)
  output4_paper
  
  #�������
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