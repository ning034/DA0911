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
    #相对价格变化
    {#选定一个品牌:光明,完美复现图1(相对价格与弧价格弹性)
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
      test <- subset(brand_share,brandname=="光明")
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
  {#brand share
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
}

{#数值铺货,测试
  dt_cat
  set.seed(10000)
  n <- sample(1:nrow(dt_cat),10000)
  dt_cat_sample <-  dt_cat[n,]
  write.csv(dt_cat_sample,file = "D:/D/data/xuhuibusiness/dt_cat_sample.csv", row.names = F)
}
