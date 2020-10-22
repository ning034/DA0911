{#��������
    setwd("D:/D/data/kaggle/")
    library(data.table)
    library(dplyr)
    #install.packages("papeR")
    library(papeR)
}

{#��������
    td<-fread("transaction_data.csv",header = T,sep=",")
    td
    gc()
    
    product<-fread("product.csv",header = T,sep=",")
    product
    length(unique(product$DEPARTMENT))
    unique(product$DEPARTMENT)
    
    #����Ʒ��
    td <- left_join(td,product,by="PRODUCT_ID")
    td <- as.data.table(td)
    td#2595732
    
    #������ϴ-�޳���ֵ
    summary(td$QUANTITY)#�й���
    quantile(td$QUANTITY,0.99)
    td <- subset(td,QUANTITY < 10)
    summary(td$SALES_VALUE)
    summary(td$RETAIL_DISC)#����ֵ
    td <- subset(td,RETAIL_DISC <= 0)#2568071���޳���Լ1%������
    td
    summary(td$COUPON_MATCH_DISC)
    
    #���۶���ߵ�Ʒ��
    td
    catsize <- td[,c(.(amt=sum(SALES_VALUE)),.(sku_num=length(unique(PRODUCT_ID)))),by="COMMODITY_DESC"]#SUB_COMMODITY_DESC  COMMODITY_DESC  DEPARTMENT
    catsize <- catsize[order(amt,decreasing = T),]
    catsize[1:30,]
    catname <- catsize$COMMODITY_DESC[1:30]
    
    #���㼾�������
    td$quarter <- cut(td$WEEK_NO,breaks = 8,labels =1:8 )
    td$quarter <- as.character(td$quarter)
    td$quarter <- as.numeric(td$quarter)
    td$year <- 1
    w <- which(td$quarter > 4)
    td$year[w] <- 2
    td
    td$quarter <- paste("q",td$quarter,sep = "" )
    td
    
    #ȡ����
    df <- subset(td,COMMODITY_DESC %in% catname)
    df
    
    #����ݶ�
    cat_amt <- df[,c(.(amt=sum(SALES_VALUE)),.(PRODUCT_ID=unique(PRODUCT_ID))),by=c("quarter","COMMODITY_DESC")]
    cat_amt
    sku_amt <- df[,c(.(sku_amt=sum(SALES_VALUE))),by=c("quarter","PRODUCT_ID")]
    sku_amt
    sku_share <- left_join(sku_amt,cat_amt,by=c("quarter","PRODUCT_ID"))
    sku_share$share <- 100 * sku_share$sku_amt / sku_share$amt
    sku_share
    
    #�����̻�
    store_amt <- df[,c(.(store_amt=sum(SALES_VALUE)),.(PRODUCT_ID=unique(PRODUCT_ID))),by=c("STORE_ID","quarter")]
    store_amt
    sku_distribution <- store_amt[,c(.(sum_amt=sum(store_amt))),by=c("quarter","PRODUCT_ID")]
    sku_distribution
    year_amt <- df[,c(.(quarteramt=sum(SALES_VALUE))),by="quarter"]
    year_amt
    sku_distribution <- left_join(sku_distribution,year_amt,by="quarter")
    sku_distribution$sku_dis <- 100 * sku_distribution$sum_amt / sku_distribution$quarteramt
    sku_distribution
    summary(sku_distribution$sku_dis)
    
    #�ع�����
    lm_data <- left_join(sku_distribution,sku_share,by=c("quarter","PRODUCT_ID"))
    lm_data
    lm_data$sku_dis2 <- lm_data$sku_dis ^ 2
    lm_data
    
    #test
    model0 <- lm(lm_data$share ~ lm_data$sku_dis + lm_data$sku_dis2)
    summary(model0)
    
    #�ع������󼾶�ƽ��
    lm_data$year <- 1
    w <- which(lm_data$quarter %in% c("q5","q6","q7","q8"))
    lm_data$year[w] <- 2
    lm_data <- as.data.table(lm_data)
    new_data <- lm_data[,c(.(meandis=mean(sku_dis)),.(meanshare=mean(share)),.(COMMODITY_DESC=unique(COMMODITY_DESC))),by=c("year","PRODUCT_ID")]
    new_data
    new_data$meandis2 <- new_data$meandis ^ 2
    new_data
    #write.csv(new_data,file="D:/D/data/kaggle/model_data.csv",row.names = F)
}

{#һ�����ģ��#####
    model1 <- lm(new_data$meanshare ~ new_data$meandis + new_data$meandis2)
    summary(model1)
}

{#Ʒ��ģ��####
    
    #����Ʒ��
    new_data
    length(unique(new_data$PRODUCT_ID))#24065
    cat_pro <- df[,c(.(PRODUCT_ID=unique(PRODUCT_ID))),by="COMMODITY_DESC"]
    length(unique(cat_pro$PRODUCT_ID))#24065
    new_data <- left_join(new_data,cat_pro,by="PRODUCT_ID")
    new_data
    
    #�������ͱ�������(0-1)-30��Ʒ�࣬2�����ͱ�������60�����ͱ���
    m <- nrow(new_data)
    x <- matrix(0,m,60)
    x <- as.data.table(x)
    y <-c(x,new_data)
    y <- as.data.table(y)
    head(y)
    x <- as.matrix(x)#��xת��Ϊ��������Ϊdata.table�ṹ�����ݣ��޷���DT[,i]ȡ���б���
    j <-seq(1,60,by=2)
    catname <- unique(new_data$COMMODITY_DESC)
    catname <- rep(catname,each=2)
    for(i in j){
        w <- which(y$COMMODITY_DESC==catname[i])
        x[w , i] <- 1
        x[, i+1] <- x[, i]
        x[, i] <- x[, i] * y$meandis
        x[, i+1] <- x[, i+1] * y$meandis2
    }
    
    #�ع�
    model2 <- lm(new_data$meanshare ~ x)#���⣺ƽ������һ���������ԣ���ɵĻع鹲���Ե����⣬��ô���⣿
    summary(model2) 
    
    #���ԡ�:��������ʲô��˼
    test <- lm(meanshare ~ meandis:COMMODITY_DESC + meandis2:COMMODITY_DESC , data=new_data)
    summary(test)
    
    #������
    j <-seq(1,60,by=2)
    result <- c()
    result1 <- c()
    zz <- prettify(summary(model2),signif.stars = getOption("show.signif.stars"))#�õ������Ե��Ǻ�
    zz <- zz[-1,]
    for(i in j){
        result1 <- data.frame(cat=catname[i],acv=zz$Estimate[i],t_value=zz$`t value`[i],acv2=zz$Estimate[i+1],t_value2=zz$`t value`[i+1],star1=zz$`   `[i],star2=zz$`   `[i+1],row.names = NULL)
        result <- rbind(result,result1)
    }
    result
    result$acv <-round(result$acv,5)
    result$acv2 <- round(result$acv2,5)
    result$t_value <- round(result$t_value,1)
    result$t_value2 <- round(result$t_value2,1)
    result
    
    output2 <- result
    output2$acv2 <- format(output2$acv2,scientific = F)
    output2$acv <- paste(output2$acv,output2$star1,sep="")
    output2$acv2 <- paste(output2$acv2,output2$star2,sep="")
    output2
    output2 <- output2[,-c(6,7)]#ע��acv2�е�ϵ���ǿ�ѧ��������
    xtable(output2)#���Ϊlatex
    write.csv(output2,file = "D:/D/data/kaggle/output2.csv",row.names = F) #���Ϊtable
    
}

{#Ʒ������ģ��####
    #�㰴����㼾��ƽ��������ֱ�������Ʒ������ָ�ꣿ
    
    #����Ʒ��-������
    new_data
    #mf <- df[,c(.(MANUFACTURER=unique(MANUFACTURER))),by=c("COMMODITY_DESC","PRODUCT_ID")]
    #mf
    #cat_data <- left_join(new_data,mf,by=c("COMMODITY_DESC","PRODUCT_ID"))
    
    
    #HHI-level
    #�ݶ����
    share_m <- df[,c(.(amt=sum(SALES_VALUE)),.(COMMODITY_DESC=unique(COMMODITY_DESC))),by=c("MANUFACTURER","year")]
    share_m <- share_m %>% group_by(year) %>% mutate(amt_year=sum(amt))
    share_m$share <- share_m$amt / share_m$amt_year
    share_m$share2 <- share_m$share ^ 2
    share_m
    
    #HHI����
    hhi <- select(share_m,c(1,2,4,7))
    hhi <- as.data.table(hhi)
    hhi <- hhi[,c(.(hhi=sum(share2))),by=c("year","COMMODITY_DESC")]
    hhi$hhi <- 100 * hhi$hhi
    hhi
    
    #����Ʒ������
    cat_data <- left_join(new_data,hhi,by=c("year","COMMODITY_DESC"))
    cat_data
    
    #����Ʒ���ģ
    catsize <- df[,c(.(amt=sum(SALES_VALUE))),by=c("year","COMMODITY_DESC")]
    catsize
    catsize$size <- ifelse(catsize$amt > quantile(catsize$amt,0.8),"large","medium")
    w <- which(catsize$amt < quantile(catsize$amt,0.15))
    catsize$size[w] <- "small"
    catsize <- catsize[,c(1,2,4)]
    
    #����Ʒ���ģ
    cat_data <- left_join(cat_data,catsize,by=c("year","COMMODITY_DESC"))
    cat_data
    
    #����size�������������
    cat_data$large <- ifelse(cat_data$size=="large",1,0)
    cat_data$medium <- ifelse(cat_data$size=="medium",1,0)
    cat_data$small <- ifelse(cat_data$size=="small",1,0)
    
    
    #�Զ���Ʒ��
    catname #ǰ�涨���ǰ30��Ʒ��
    tolower(catname)
    food <- catname[c(2,4,5,6,8,9,11,13,14,15,18,21,27,30)]
    drink <- catname[c(1,3,7,10,16,17,19,20,23,24,25,29)]
    baby <- catname[28]
    household <- catname[26]
    other <- catname[c(12,22)]
    
    #�����Զ���Ʒ����������
    cat_data$food <- ifelse(cat_data$COMMODITY_DESC %in% food,1,0)
    cat_data$drink <- ifelse(cat_data$COMMODITY_DESC %in% drink,1,0)
    cat_data$baby <- ifelse(cat_data$COMMODITY_DESC %in% baby,1,0)
    cat_data$household <- ifelse(cat_data$COMMODITY_DESC %in% household,1,0)
    cat_data$other <- ifelse(cat_data$COMMODITY_DESC %in% other,1,0)
    cat_data
    write.csv(cat_data,file="D:/D/data/kaggle/model3_data.csv",row.names = F)
    
    #�ع�����
    acv <- cat_data$meandis * cat_data[,c(7,9:10,12:15)]#ȥ��һ���������11��16
    acv <- as.matrix(acv)
    acv2 <- cat_data$meandis2 * cat_data[,c(7,9:10,12:15)]
    acv2 <- as.matrix(acv2)
    maineffect <- cat_data[,c(7,9:10,12:15)]
    maineffect <- as.matrix(maineffect)
    
    #�ع�
    model3 <- lm(cat_data$meanshare ~ maineffect + acv + acv2 )
    summary(model3)
    
    #������
    zz <- prettify(summary(model3))
    zz <- zz[-1,]
    
    #�������
    zz$Estimate <- round(zz$Estimate,digits = 5)
    zz$Estimate <- format(zz$Estimate,scientific = F)
    zz$`t value` <- round(zz$`t value`,digits = 1)
    zz$`t value` <- format(zz$`t value`,scientific = F)
    zz$Estimate <- paste(zz$Estimate,zz$`   `,sep="")
    
    output3 <- data.frame(char=c("hhi","large","medium","food","drink","baby","household"),maineffect=zz$Estimate[1:7],tvalue=zz$`t value`[1:7],acv=zz$Estimate[8:14],tvalue1=zz$`t value`[8:14],acv2=zz$Estimate[15:21],tvalue2=zz$`t value`[15:21])
    output3
    write.csv(output3,file = "D:/D/data/kaggle/output3.csv",row.names = F)
    xtable(output3)
}


{#Ʒ��ģ��####
    new_data
    
    #����Ʒ��#��������
    mf <- df[,c(.(MANUFACTURER=unique(MANUFACTURER))),by=c("year","PRODUCT_ID","COMMODITY_DESC")]
    mf
    brand_data <- left_join(new_data,mf,by=c("year","PRODUCT_ID","COMMODITY_DESC"))
    
    #Ʒ������
    mfrank <- df[,c(.(mfsales=sum(SALES_VALUE))),by=c("year","COMMODITY_DESC","MANUFACTURER")]
    mfrank <- mfrank[order(year,COMMODITY_DESC,mfsales,decreasing = T),]
    mfrank <- mfrank %>% group_by(year,COMMODITY_DESC) %>% mutate(rank=1:length(MANUFACTURER))
    #as.data.frame(table(mfrank$COMMODITY_DESC,mfrank$year))#�鿴Ʒ���ж���������(Ʒ��),���ٵ�4�������ļ��ٸ�
    mfrank
    
    #����ת��
    w <- which(mfrank$rank > 9)
    mfrank$rank[w] <- 10 
    mfrank$rank <- as.factor(mfrank$rank)
    rm(w)
    gc()
    mfrank <- mfrank[,-4]
    mfrank
    
    #�µĻع�����
    brand_data <- left_join(brand_data,mfrank,by=c("year","MANUFACTURER","COMMODITY_DESC"))
    brand_data
    #brand_data <- fread("model4_data.csv",header = T)
    #brand_data$rank <- as.factor(brand_data$rank)
    #write.csv(brand_data,file="D:/D/data/kaggle/model4_data.csv",row.names = F)
    
    #Ϊ�˱�֤���������������Ϊ3�Ժ�Ķ���Ϊ4
    {
        brand_data$rank <- as.character(brand_data$rank)
        brand_data$rank <- as.numeric(brand_data$rank)
        w <- which(brand_data$rank > 3)
        brand_data$rank[w] <- 4
        brand_data$rank <- as.factor(brand_data$rank)
        brand_data
    }
    
    #������
    model4 <- lm(meanshare ~ meandis:COMMODITY_DESC:rank + meandis2:COMMODITY_DESC:rank,data=brand_data)
    summary(model4)
    
    #zz <- summary(model4)
    #output4 <- as.data.frame(zz$coefficients)
    #output4 <- as.data.table(output4)
    #output4
    #��ȡ����
    #x <- dimnames(zz$cov.unscaled)
    #x <- x[[1]]
    
    #library(broom)
    #tidy(model4) 
    #��ȱʧֵ��prettify������
    output4 <- prettify(summary(model4))
    output4 <- output4[-1,]
    head(output4)
    output4$Estimate <- round(output4$Estimate,5)
    output4$`t value` <- round(output4$`t value`,1)
    output4$Estimate <- format(output4$Estimate,scientific = F)
    output4$Estimate <- paste(output4$Estimate,output4$`   `,sep="")
    output4 <- output4[,-c(7,8)]
    output4
    
    #��ȡ������
    w1 <- regexpr("_",output4$` `)
    w2 <- regexpr("[0-9]",output4$` `)
    output4$cat <- substr(output4$` `,w1+1,w2-6)
    head(output4)
    
    result4 <- data.frame(cat=output4$cat[1:30],acv_1=output4$Estimate[1:30],acv2_1=output4$Estimate[121:150],acv_2=output4$Estimate[31:60],acv2_2=output4$Estimate[151:180],acv_3=output4$Estimate[61:90],acv2_3=output4$Estimate[181:210],acv_4=output4$Estimate[91:120],acv2_4=output4$Estimate[211:240])
    result4$cat <- gsub("DESC","",result4$cat)
    result4
    write.csv(result4,file="D:/D/data/kaggle/output4.csv",row.names = F)
    xtable(result4)
    #install.packages("stargazer")
    #output4 <- prettify(zz, signif.stars = getOption("show.signif.stars"))
    
    #convex
    convex <- result4[,-c(2,4,6,8)]
    convex$acv2_1 <- as.character(convex$acv2_1)
    convex$acv2_2 <- as.character(convex$acv2_2)
    convex$acv2_3 <- as.character(convex$acv2_3)
    convex$acv2_4 <- as.character(convex$acv2_4)
    convex[c(11,12,15,20),]$acv2_1 <- "NO"
    convex[-c(11,12,15,20),]$acv2_1 <- "YES"
    convex[c(1,6,7,12,17,27),]$acv2_2 <- "NO"
    convex[-c(1,6,7,12,17,27),]$acv2_2 <- "YES"
    convex[c(2,6,7,14,16,17,19,20,22,27,28,30),]$acv2_3 <- "NO"
    convex[-c(2,6,7,14,16,17,19,20,22,27,28,30),]$acv2_3<- "YES"
    convex[c(8,12,17,19,22,24,26,27,30),]$acv2_4 <- "NO"
    convex[-c(8,12,17,19,22,24,26,27,30),]$acv2_4<- "YES"
    convex
    xtable(convex)
}


{#αR^2����
    
}