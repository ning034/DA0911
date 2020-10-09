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
}

{#��ģ��֤��-����####
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

{#��ģ��֤��-���####
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


{#һ�����ģ��####
  #���㼾�ȷݶ�
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
  summary(lm_data$sku_distribution)
  lm1 <- lm(share~sku_distribution+sku_distribution2,data=lm_data)
  summary(lm1)
  
  {#��ֵ�̻���
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
  {#Ʒ�������Ϣ����
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
    #����ƽ��ms�ı���
    mean(smr_information$ms_max / smr_information$ms_avg)
    mean(smr_information$ms_avg / smr_information$ms_med)
    
    #����ƽ��acv�ı���
    mean(smr_information$acv_max / smr_information$acv_avg)
    mean(smr_information$acv_avg / smr_information$acv_med)
    
    #�������sku��һ��sku
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
  write.csv(result,file="D:/D/data/xuhuibusiness/equation3.csv",row.names = F)
  
  {#������
    lm1 <- lm(lm_data$share~lm_data$retailtypename * (lm_data$sku_distribution+lm_data$sku_distribution2))
    summary(lm1)
  }
}

{#Ʒ������ģ��-ԭʼ####
  #��Ϊÿ��Ʒ���е����������ڿ���Ӧ���ǰ�����������Ϊ����Ʒ��
  unique(df$retailtypename)
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
  
  #����Ʒ������-size
  cat <- df[,c(.(amt=sum(amt))),by="retailtypename"]
  w <- which(cat$amt <= 20000000 & cat$amt > 4000000)
  cat$size <- "large" 
  cat[w,]$size <- "medium"
  w <- which(cat$amt <= 4000000)
  cat[w,]$size <- "small"
  cat
  
  #����Ʒ������-HII
  hii <- df[,c(.(amt=sum(amt)),.(retailtypename=unique(retailtypename)),.(mprice=mean(price))),by="barcode"]
  hii
  retail_amt <- df[,c(.(amt=sum(amt))),by="retailtypename"]
  retail_amt
  hii <- left_join(hii,retail_amt,by="retailtypename")
  head(hii)
  hii$share <-  hii$amt.x / hii$amt.y
  hii$share2 <- hii$share ^ 2
  head(hii)
  {##����Ʒ������- value density
    #ȥƷ��ݶ�����sku�ļ۸�
    hii <- arrange(hii,desc(share))#�Ȱ���share����
    head(hii)
    hii <- as.data.table(hii)
    vd <- hii[,.SD[1],by="retailtypename"]#ȡÿ��Ʒ������ֵ
    vd <- vd[,c(1,4)]
    vd
  }
  
  hii<- hii[,c(.(HII=sum(share2))),by="retailtypename"]
  hii
  hii <- left_join(hii,vd,by="retailtypename")#���vd
  hii <- left_join(hii,cat,by="retailtypename")#���cat
  hii <- hii[,-4]
  hii
  
  #�ع�����
  lm_data <- left_join(sku_data,distribution,by=c("quarter","barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  #lm1 <- lm(share~sku_distribution+sku_distribution2+retailtypename,data=lm_data)#���濴��һ��
  #summary(lm1)
  
  #
  test <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
  test
  
  #�����������
  lm_data <- left_join(lm_data,test,by="barcode")
  lm_data <- left_join(lm_data,hii,by="retailtypename")
  lm_data
  #write.csv(lm_data,file = "D:/D/data/xuhuibusiness/model_cat.csv",row.names = F)
  
  retailtypename <- unique(df$retailtypename)
  retailtypename <- retailtypename[order(retailtypename)]
  retailtypename 
  
  {#���Իع�####
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
  
{#Ʒ������ģ��-����####
  #��Ϊÿ��Ʒ���е����������ڿ���Ӧ���ǰ�����������Ϊ����Ʒ��
  unique(df$retailtypename)
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
  
  #����Ʒ������-size��retailtypename ��Ϊ 
  #food0 ������ʳ��(retailtypename[c(1,2,13,15,16,17,18)])��
  #food1 �ӹ���ʳ��(retailtypename[c(3,9,10,13,23,24)])��
  #household ��ͥ/�����(retailtypename[c(8,19,20)])��
  #wear ��װ(retailtypename[c(11,12,22)])��
  #person ���˻���(retailtypename[c(5)])��
  #mark ��ױ(retailtypename[c(7)])��
  #other ����(retailtypename[c(4,6,14,21)])
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
  
  #����Ʒ������-HII
  hii <- df[,c(.(amt=sum(amt)),.(retailtypename=unique(retailtypename)),.(mprice=mean(price))),by="barcode"]
  hii
  retail_amt <- df[,c(.(amt=sum(amt))),by="retailtypename"]
  retail_amt
  hii <- left_join(hii,retail_amt,by="retailtypename")
  head(hii)
  hii$share <-  hii$amt.x / hii$amt.y
  hii$share2 <- hii$share ^ 2
  head(hii)
  {##����Ʒ������- value density
    #ȥƷ��ݶ�����sku�ļ۸�
    hii <- arrange(hii,desc(share))#�Ȱ���share����
    head(hii)
    hii <- as.data.table(hii)
    vd <- hii[,.SD[1],by="retailtypename"]#ȡÿ��Ʒ������ֵ
    vd
    vd <- vd[,c(1,4)]
    vd
  }
  
  hii<- hii[,c(.(HII=sum(share2))),by="retailtypename"]
  hii
  hii <- left_join(hii,vd,by="retailtypename")#���vd
  hii <- left_join(hii,cat,by="retailtypename")#���cat
  hii
  hii <- hii[,-4]
  hii
  
  #�ع�����
  lm_data <- left_join(sku_data,distribution,by=c("quarter","barcode"))
  lm_data$sku_distribution2 <- lm_data$sku_distribution ^ 2
  lm_data
  
  #
  test <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
  test
  
  #�����������
  lm_data <- left_join(lm_data,test,by="barcode")
  lm_data <- left_join(lm_data,hii,by="retailtypename")
  lm_data
  #write.csv(lm_data,file = "D:/D/data/xuhuibusiness/model_cat.csv",row.names = F)
  
  retailtypename <- unique(df$retailtypename)
  retailtypename <- retailtypename[order(retailtypename)]
  retailtypename
  
  {#���Իع�####
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
  
  #��һ��������ʽ
  write.csv(result,file="D:/D/data/xuhuibusiness/equation4.csv",row.names = F)
  
  #ճ������һ��latex��ʽ
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
  
  {#��������
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
    
    #ճ������һ��latex��ʽ
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
  
  {#��������2
    lm_data
    lm_data$c2 <- lm_data$share / lm_data$HII
    lm1 <- lm(lm_data$c2~lm_data$sku_distribution + lm_data$sku_distribution2)
    summary(lm1)
    result1 <- data.frame(HII="sku_level HHI",main_effect=coef(lm1)[1],t_value=coef(summary(lm1))[,3][1],acv=coef(lm1)[2],t_value=coef(summary(lm1))[,3][2],acv2=coef(lm1)[3],t_value2=coef(summary(lm1))[,3][3],row.names = NULL)
    result1
    
    #��һ��������ʽ
    write.csv(result1,file="D:/D/data/xuhuibusiness/equation4_2.csv",row.names = F)
    
    zz <- df[,c(.(barcode=unique(barcode))),by="retailtypename"]
    zz <- left_join(lm_data,zz,by="barcode")
    zz
    write.csv(zz,file="D:/D/data/xuhuibusiness/lm_data.csv",row.names = F)
    
    #��һ��latex��ʽ
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
  
  {#��������3
    summary(lm_data$mprice)
    w <- which(lm_data$mprice > 77.18)#������λ��
    lm_data$vd <- "low"
    lm_data[w,]$vd <- "high"
    
    #��һ��������ʽ
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
    
    #��һ��latex�ı���ʽ
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

{#����Ʒ���е��쵼��Ʒ��ģ��####
  
}