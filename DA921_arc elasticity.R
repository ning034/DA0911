{
  ###1.基本设置####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
}
{
  dt_cat <- fread("data_category.csv",header = T,sep=",")#7588515
  length(unique(dt_cat$brandname))#2104
  gc()
  
}

{
  dt_cat$week <- week(dt_cat$saledate)
  gc()
  quantile(dt_cat$price,0.855)#396 #删除一些，防止异常值影响
  cond <- which(dt_cat$price > 396)
  dt_cat <- dt_cat[-cond,]#由于异常值，因此这里删除部分价格过高的数据
  gc()
  dt_cat
  {#brand price week
    p_elstic <- dt_cat[,c(.(qty=sum(qty)) ,.(price_ac=mean(amt/qty)) ,.(price_ori=mean(price))) ,by=c("brandname","week")]
    p_elstic
    #相对价格变化####
    {#选定一个品牌:光明,完美复现图1(相对价格与弧价格弹性)####
      brand <- dt_cat[, sum(amt),by="brandname"]
      brand <- brand[order(V1),]
      brand
      test <- subset(p_elstic,brandname=="光明")
      test <- test[order(week),]
      test
      test_dif <- diff(test$price_ac)
      test_dif <- test_dif / test$price_ac[-53]#价格弹性相对变化1-52 week
      test_dif
      pt <- test$price_ac[2:53]
      pt_1 <- test$price_ac[1:52]
      test <- subset(brand_share,brandname=="光明")#先算下面的brand share
      test <- test[order(week),]
      test
      st <- test$share[2:53]
      st_1 <- test$share[1:52]
      arc_elasticity <- ((st-st_1)/(st+st_1))/ ((pt-pt_1) / (pt+pt_1))
      arc_elasticity
      plot(test_dif,arc_elasticity)
      #去掉异常值更好看
      which(arc_elasticity >1000)#12
      which(arc_elasticity < (-1000))#33
      arc_elasticity <- arc_elasticity[-c(12,33)]
      test_dif <- test_dif[-c(12,33)]
      plot(test_dif,arc_elasticity)
      #结果：相对价格变化超出5%的区域，弧弹性就会区域平稳
    }
    
  }
  {#brand share 品牌占品类销量的比率#####
    brand_share<-dt_cat[ ,c(.(qty=sum(qty))),by=c("retailtypename","brandname","week")]
    brand_share
    brand_share<-brand_share %>% group_by(retailtypename,week) %>% mutate(sum=sum(qty))
    brand_share
    brand_share<-arrange(brand_share,retailtypename,week)
    brand_share
    brand_share<-as.data.table(brand_share)
    brand_share$share<-brand_share$qty/brand_share$sum
    brand_share
  }
   df <- left_join(brand_share,p_elstic,by=c("brandname","week"))#价格弹性与市场份额合一
   df
   write.csv(df,file = "D:/D/data/xuhuibusiness/arc_elasticity.csv", row.names = F)
   
   {#重新读入brand characteristic ######
     arc_elasticity <- fread("arc_elasticity.csv",header = T)
     arc_elasticity
     tips <- function(data){length(unique(data))}
     brand_num <- arc_elasticity[,c(.(num=tips(brandname))),by="retailtypename"]
     brand_num
     summary(brand_num$num)#太多了，可能需要选更细的分类或者挑选少量的品牌或者考虑就对这么多品牌进行研究
     arc_elasticity
     arc_elasticity$discount_mag <- (arc_elasticity$price_ori - arc_elasticity$price_ac) / arc_elasticity$price_ori#折扣幅度
     arc_elasticity$discount_freq <- 0
     w <- which(arc_elasticity$discount_mag > 0.05)
     arc_elasticity[w,]$discount_freq <- 1 #赋值1代表折扣幅度超过5%，后续统计数量作为品牌的特征
     #还差相对价格：品牌的平均非促销价格 / 品类的加权平均价格
     #还差价格变动系数：品牌价格标准差 / 品牌平均价格   关于这个需要查文献( Bolton 1989)，确定分子分母中的价格是真实价格还是原价
     
     
   }
}

{#数值铺货,测试
  dt_cat
  set.seed(10000)
  n <- sample(1:nrow(dt_cat),10000)
  dt_cat_sample <-  dt_cat[n,]
  write.csv(dt_cat_sample,file = "D:/D/data/xuhuibusiness/dt_cat_sample.csv", row.names = F)
}

{#重新读入数据，数值铺货率=(铺货的门店数)/总的门店数     年水平
  dt_cat_sample <- fread("dt_cat_sample.csv",header = T)
  dt_cat_sample
  length(unique(dt_cat_sample$storecode))#8个
  tips <- function(data){length(unique(data))}
  distribution_num <- dt_cat_sample[,c(.(store_num=tips(storecode))),by=c("barcode")]
  distribution_num
  distribution_num$distri_num <- distribution_num$store_num / length(unique(dt_cat_sample$storecode))
  summary(distribution_num$distri_num)
  distribution_num
  {#加权铺货率:铺货的门店数总销售金额)/所有门店销售金额
    product_amt <- dt_cat_sample[,c(.(amt_store=sum(amt))),by=c("barcode","storecode")]
    product_amt <- product_amt %>% group_by(barcode) %>%
      mutate(amt_sum=sum(amt_store))
    product_amt <- arrange(product_amt,barcode)
    product_amt
    distribution_weight <- product_amt %>% select(.,barcode,amt_sum)
    distribution_weight <- unique(distribution_weight)
    distribution_weight$distri_weight <- distribution_weight$amt_sum / sum(dt_cat_sample$amt)
    distribution_weight$distri_weight <- distribution_weight$distri_weight * 100
    summary(distribution_weight$distri_weight)
  }
  #两种铺货是不是相关——极低
  df <- left_join(distribution_weight,distribution_num,by="barcode")
  df
  cor(df$distri_num,df$distri_weight)#0.1871002
}
