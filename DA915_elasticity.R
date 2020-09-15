{
  ###1.基本设置####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
}

{#读入数据
  dt_cat<-fread("data_category.csv",header = T,sep=",")#10186302
  gc()
  dt_cat
  vb <-
    fread(
      "variable_code.csv",
      header = T,
      sep = ",",
      encoding = "UTF-8"
    )
  vb
}

{#计算品类价格弹性尝试
  dt_cat$week<-week(dt_cat$saledate)
  gc()
  dt_cat
  #p_elstic<-dt_cat%>%group_by(retailtypename,week)%>%summarise(qty=sum(qty),price_ac=mean(amt/qty),price_ori=mean(price))
  #上一行代码中mean(amt/qty)计算出现了问题，不知道为什么？
  p_elstic <- dt_cat[,c(.(qty=sum(qty)) ,.(price_ac=mean(amt/qty)) ,.(price_ori=mean(price))) ,by=c("retailtypename","week")]
  gc()
  head(p_elstic)
  p_elstic<-p_elstic[order(retailtypename),]
  p_elstic
  lm_elstic<-lm(log(qty)~log(price_ac)+retailtypename,data=p_elstic)
  summary(lm_elstic)
  {#考虑品类特征对价格弹性的影响
    cof<-lm_elstic$coefficients
    cof<-as.data.frame(cof)
    cof$cof<-cof$cof-0.004127701#加上虚拟变量对照组信息
    cof
    rownames(cof)
    cof$retailtypename <- rownames(cof)
    cof
    cof<-cof[-c(1:2),]
    cof
    {#品类竞争强度-qty做份额：brand-share^2
      cor(dt_cat$qty,dt_cat$amt)#-0.0001310391，这是什么意思
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
      #结论：竞争强度越强，价格敏感性越高;虽然不显著
    }
    
    {#品类竞争强度-amt做份额：brand-share^2
      cor(dt_cat$qty,dt_cat$amt)#-0.0001310391，这是什么意思
      brand_share<-dt_cat[ ,c(.(qty=sum(amt))),by=c("retailtypename","brandname")]
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
      #结论：竞争强度越强，价格敏感性越高;仍然不显著;因为还有其他因素没有考虑进去，所以残差项与因变量有相关性
      #后续复现论文：Understanding the Characteristics of Price Elasticities for Frequently Purchased Packaged Goods
      #再加上：品类特征的变量，如是否是食物、品类相对价格、可存储性
      #还有品牌特征变量，市场份额、相对价格、打折程度、价格方差(波动情况)、折扣频率
      #如果要按照这个方向写论文，又什么创新点？它是非参、周扫描数据；我们可以做到更细。
      #那做到更细的优势是什么？为什么要做到更细呢？对比两者不同，给出原因。——现在想不到更深入的问题，后续继续。
      #一个后续工作的方向：查找这篇论文的被引文献，看看大家引用这篇文章都做了什么？也许可以找到写作方向或者已经有人做过我上面说的东西。
    }
  }
  
  
}