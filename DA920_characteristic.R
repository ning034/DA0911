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
  df <- fread("category_avgprice.csv",header = T)
  df
  
}

{
  
  {
    dt_cat$week <- week(dt_cat$saledate)
    gc()
    quantile(dt_cat$price,0.855)#396 #删除一些，防止异常值影响
    cond <- which(dt_cat$price > 396)
    dt_cat <- dt_cat[-cond,]#由于异常值，因此这里删除部分价格过高的数据
    gc()
    dt_cat
  }
  
  #计算品类价格弹性尝试
  #p_elstic<-dt_cat%>%group_by(retailtypename,week)%>%summarise(qty=sum(qty),price_ac=mean(amt/qty),price_ori=mean(price))
  #上一行代码中mean(amt/qty)计算出现了问题，不知道为什么？
  p_elstic <- dt_cat[,c(.(qty=sum(qty)) ,.(price_ac=mean(amt/qty)) ,.(price_ori=mean(price))) ,by=c("retailtypename","week")]
  gc()
  head(p_elstic)
  p_elstic<-p_elstic[order(retailtypename),]
  p_elstic
  lm_elstic<-lm(log(qty)~log(price_ac)+retailtypename,data=p_elstic)
  summary(lm_elstic)
  {
    cof<-lm_elstic$coefficients
    cof<-as.data.frame(cof)
    cof$cof<-cof$cof-0.085347#加上虚拟变量对照组信息
    cof
    rownames(cof)
    cof$retailtypename <- rownames(cof)
    cof
    cof<-cof[-c(1:2),]
    cof
  }
  
  {#品类竞争强度-qty做份额：brand-share^2
    cor(dt_cat$qty,dt_cat$amt)#0.2341666，这是什么意思
    brand_share<-dt_cat[ ,c(.(qty=sum(qty))),by=c("retailtypename","brandname")]
    brand_share
    brand_share<-brand_share %>% group_by(retailtypename) %>% mutate(sum=sum(qty))
    brand_share<-arrange(brand_share,retailtypename)
    brand_share<-as.data.table(brand_share)
    brand_share$share<-brand_share$qty/brand_share$sum
    brand_share$share_square <- brand_share$share * brand_share$share
    brand_share
    #加总 
    intensity<-brand_share[ ,c(.(intensity=sum(share_square))),by="retailtypename"]
    #与cof统一id：retailtypename
    intensity$retailtypename<-paste("retailtypename",intensity$retailtypename,sep="")
    intensity
    df<-left_join(cof,intensity,by="retailtypename")
    df
    lm_chr<-lm(cof~intensity,data=df)
    summary(lm_chr)
    #结论：竞争强度越强，价格弹性越高;虽然不显著
    {#char——storable
      df$storable <- 0
      #c(1,2,4,6,8,14,15,16,17,18,19,20,22,23)
      w <- c(1,2,4,6,8,14,15,16,17,18,19,20,22,23)
      df[w,]$storable <- 1
      lm_chr<-lm(cof~intensity+storable,data=df)
      summary(lm_chr)
    }
    
    {#char-food
      df
      df$food <- 0
      #c(1,2,8,9,12,14,15,16,17,18,22,23)
      w <- c(1,2,8,9,12,14,15,16,17,18,22,23)
      df[w,]$food <- 1
      lm_chr<-lm(cof~intensity+storable+food,data=df)
      summary(lm_chr)
    }
  }

}