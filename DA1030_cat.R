{#基本设置
  ###1.基本设置####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(papeR)
  library(cowplot)
  dt2016<-fread("dt2016.csv",header = T,sep=",")#10175541
  gc()
 
}

{#test-price-qty-amt
  #原来做的销量或价格或销售额的分位数，应该按照品类去选择，去清洗。
  #想起来原来cody书中，清洗银行账户时要按照每个账户本身去做。
  dt2016
  price <- dt2016[,quantile(price,0.99),by="retailtypename"]
  price <- price[order(V1,decreasing = T),]
  price
  qty <- dt2016[,quantile(qty,0.99),by="retailtypename"]
  qty <- qty[order(V1,decreasing = T),]
  qty
  amt <- dt2016[,quantile(amt,0.99),by="retailtypename"]
  amt <- amt[order(V1,decreasing = T),]
  amt
  #但是按照原来做的清洗，也看不出有什么问题。
  #后面你试试两种不同的清洗方式对结果有什么影响？
}

{#storecode-top three####
  dt2016
  retailtypename <- unique(dt2016$retailtypename)
  brand <- unique(dt2016$brandname)
  
  #1.qty#####
  #10501-10502-10505####
  store_all <- dt2016[,c(.(qty=sum(qty)),.(amt=sum(amt)),.(mprice=mean(price))),by="storecode"]
  store_all <- store_all[order(qty,decreasing = T),]
  store_all
  
  #10501
  top1 <- subset(dt2016,storecode==10501)
  retail_top1 <- unique(top1$retailtypename)
  retail_top1
  no1_retail <- setdiff(retailtypename,retail_top1)
  no1_retail
  gc()
  {#
    length(unique(top1$saletime))# 45133
    length(unique(top1$saledate))#366
    length(unique(top1$poscode))#16，占总数的15.4%
    length(unique(top1$lessoncode))#5，占总数的20%
    length(unique(top1$areacode))#42，占总数的13.1%。
    length(unique(top1$countercode))#126，占总数的9.8%。
  }
  #brand_top1 <- unique(top1$brandname)
  #no1_brand <- setdiff(brand,brand_top1)
  
  #10502
  top1 <- subset(dt2016,storecode==10502)
  retail_top1 <- unique(top1$retailtypename)
  retail_top1
  no1_retail <- setdiff(retailtypename,retail_top1)
  no1_retail
  gc()
  {#
    length(unique(top1$saletime))# 45258
    length(unique(top1$saledate))#366
    length(unique(top1$poscode))#6，占总数的5.8%
    length(unique(top1$lessoncode))#5，占总数的20%
    length(unique(top1$areacode))#40，占总数的12.4%。
    length(unique(top1$countercode))#75，占总数的5.8%。
  }
  
  #10505
  top1 <- subset(dt2016,storecode==10505)
  retail_top1 <- unique(top1$retailtypename)
  retail_top1
  no1_retail <- setdiff(retailtypename,retail_top1)
  no1_retail
  gc()
  {#
    length(unique(top1$saletime))# 43167
    length(unique(top1$saledate))#366
    length(unique(top1$poscode))#8，占总数的7.7%
    length(unique(top1$lessoncode))#5，占总数的20%
    length(unique(top1$areacode))#39，占总数的12.1%。
    length(unique(top1$countercode))#105，占总数的8.2%。
  }
  
  
  #2.amt####
  #10201-10203-10202####
  #推测销售额较高的门店销售的品类，刚好更销量较高的门店相反。
  store_all <- store_all[order(amt,decreasing = T),]
  store_all
  
  #10201
  top1 <- subset(dt2016,storecode==10201)
  retail_top1 <- unique(top1$retailtypename)
  retail_top1
  no1_retail <- setdiff(retailtypename,retail_top1)
  no1_retail
  gc()
  {#比例远高于其他两个门店,POS机、课区柜编码数量；即使在所有门店对比中，它也是最好的。
    length(unique(top1$saletime))# 44378
    length(unique(top1$saledate))#366
    length(unique(top1$poscode))#35，占总数的33.7%
    length(unique(top1$lessoncode))#8，占总数的32%
    length(unique(top1$areacode))#44，占总数的13.7%。
    length(unique(top1$countercode))#467，占总数的36.3%。
  }
  
  #10203
  top1 <- subset(dt2016,storecode==10203)
  retail_top1 <- unique(top1$retailtypename)
  retail_top1
  no1_retail <- setdiff(retailtypename,retail_top1)
  no1_retail
  gc()
  {#
    length(unique(top1$saletime))# 42873
    length(unique(top1$saledate))#349
    length(unique(top1$poscode))#16，占总数的15.38%
    length(unique(top1$lessoncode))#4，占总数的16%
    length(unique(top1$areacode))#24，占总数的7.5%。
    length(unique(top1$countercode))#193，占总数的15%。
  }
  
  #10202
  top1 <- subset(dt2016,storecode==10202)
  retail_top1 <- unique(top1$retailtypename)
  retail_top1
  no1_retail <- setdiff(retailtypename,retail_top1)
  no1_retail
  gc()
  {#
    length(unique(top1$saletime))# 42445
    length(unique(top1$saledate))#366
    length(unique(top1$poscode))#16，占总数的15.4%
    length(unique(top1$lessoncode))#5，占总数的1/5
    length(unique(top1$areacode))#31，占总数的9.7%。
    length(unique(top1$countercode))#249，占总数的19.4%。
  }
  
  retailtypename <- dt2016[,c(.(qty=sum(qty)),.(amt=sum(amt)),.(mprice=mean(price))),by="retailtypename"]
  retailtypename <- retailtypename[order(mprice,decreasing = T),]
  retailtypename
  #结合以上每家门店的所销售的品类，基本可以确定，销售额最高的三家门店所销售的品类多是平均单价较高的品类(retailtypename前13)。
  
  #3.其他门店-销量与销售额都较低####
  #单纯就是销售情况不好。数据中，该门店的销售信息就不多。
  #10503-10506-10590####
  
  #10503
  top1 <- subset(dt2016,storecode==10503)
  retail_top1 <- unique(top1$retailtypename)
  retail_top1
  no1_retail <- setdiff(retailtypename$retailtypename,retail_top1)
  no1_retail
  gc()
  {#
    length(unique(top1$saletime))# 44264
    length(unique(top1$saledate))#366
    length(unique(top1$poscode))#4
    length(unique(top1$lessoncode))#5
    length(unique(top1$areacode))#39
    length(unique(top1$countercode))#79
  }
  
  #10506
  top1 <- subset(dt2016,storecode==10506)
  retail_top1 <- unique(top1$retailtypename)
  retail_top1
  no1_retail <- setdiff(retailtypename$retailtypename,retail_top1)
  no1_retail
  gc()
  {#
    length(unique(top1$saletime))# 45204
    length(unique(top1$saledate))#366
    length(unique(top1$poscode))#3
    length(unique(top1$lessoncode))#5
    length(unique(top1$areacode))#41
    length(unique(top1$countercode))#82
  }
  
  #10590
  top1 <- subset(dt2016,storecode==10590)
  retail_top1 <- unique(top1$retailtypename)
  retail_top1
  no1_retail <- setdiff(retailtypename$retailtypename,retail_top1)
  no1_retail
  gc()
  {#
    length(unique(top1$saletime))#1，只有一个时间10：00：00，很奇怪。超级异常。
    length(unique(top1$saledate))#5,只有5天的数据。但不连续3月12月都有。
    length(unique(top1$poscode))#1
    length(unique(top1$lessoncode))#3
    length(unique(top1$areacode))#21
    length(unique(top1$countercode))#33
  }
  
  {#5.整体汇总####
    tips <- function(data){length(unique(data))}
    store_all <- dt2016[,c(.(qty=sum(qty)),.(amt=sum(amt)),.(mprice=mean(price)),.(cat_num=tips(retailtypename))),by="storecode"]
    store_all <- store_all[order(qty,decreasing = T),]
    store_all$qty <- store_all$qty / 10000
    store_all$amt <- store_all$amt / 100000000
    store_all$amt <- round(store_all$amt,4)
    store_all$qty <- round(store_all$qty,4)
    store_all$mprice <- round(store_all$mprice,2)
    store_all
    xtable(store_all)
    
    duibi <- dt2016[,c(.(saletime=tips(saletime)),.(saledate=tips(saledate)),.(poscode=tips(poscode)),.(lessoncode=tips(lessoncode)),.(areacode=tips(areacode)),.(countercode=tips(countercode))),by="storecode"]
    duibi
    zongshu <- data.frame(storecode="sum",saletime=52139,saledate=366,poscode=105,lessoncode=45,areacode=321,countercode=1284)
    zongshu
    duibi <- rbind(duibi,zongshu)
    duibi
    
    duibi$storecode <- as.character(duibi$storecode)
    duibi$storecode <- as.numeric(duibi$storecode)
    x <- left_join(store_all,duibi,by="storecode")
    x <- arrange(x,desc(amt))
    x
    xtable(x)
    y <- apply(x,2,as.character)
    xtable(y)
  }
}

{#品类信息####
  #基本信息
  retailtypename <- dt2016[,c(.(qty=sum(qty)),.(amt=sum(amt)),.(mprice=mean(price)),.(brand_num=tips(brandname)),.(sku_num=tips(barcode))),by="retailtypename"]
  retailtypename <- retailtypename[order(brand_num,decreasing = T),]
  retailtypename
  
  #输出
  retailtypename$amt <- retailtypename$amt / 100000000
  retailtypename$qty <- round(retailtypename$qty,0)
  retailtypename$amt <- round(retailtypename$amt,4)
  retailtypename$mprice <- round(retailtypename$mprice,0)
  retailtypename
  y <- apply(retailtypename,2,as.character)
  xtable(y)
  
  #品类特征-从品牌或sku层面提取
  brand <- dt2016[,c(.(brand_amt=sum(amt))),by=c("retailtypename","brandname")]
  brand <- brand %>% group_by(retailtypename) %>% mutate(retail_amt=sum(brand_amt))
  brand$share <- 100 * brand$brand_amt / brand$retail_amt
  brand$share2 <- (brand$share / 100) ^ 2
  brand <- as.data.table(brand)
  brand
  
  hhi <- brand[,c(.(HHI=sum(share2)),.(mean_share=mean(share)),.(med_share=median(share)),.(max_share=max(share)),.(min_share=min(share))),by="retailtypename"]
  hhi <- hhi[order(HHI),]
  hhi
  xtable(hhi,digits = 3)
  
  #
  hhi$min_share <- round(hhi$min_share,3)
  hhi
  length(which(hhi$min_share > 0.0005))
  
  x <- left_join(retailtypename,hhi,by="retailtypename")
  x
  cor(x$HHI,x$brand_num)
  cor(x$HHI,x$mean_share)
  lm1 <- lm(x$HHI~x$brand_num + x$mean_share)
  summary(lm1)
}