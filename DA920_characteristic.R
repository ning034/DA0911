{
  ###1.��������####
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
    quantile(dt_cat$price,0.855)#396 #ɾ��һЩ����ֹ�쳣ֵӰ��
    cond <- which(dt_cat$price > 396)
    dt_cat <- dt_cat[-cond,]#�����쳣ֵ���������ɾ�����ּ۸���ߵ�����
    gc()
    dt_cat
  }
  
  #����Ʒ��۸��Գ���
  #p_elstic<-dt_cat%>%group_by(retailtypename,week)%>%summarise(qty=sum(qty),price_ac=mean(amt/qty),price_ori=mean(price))
  #��һ�д�����mean(amt/qty)������������⣬��֪��Ϊʲô��
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
    cof$cof<-cof$cof-0.085347#�������������������Ϣ
    cof
    rownames(cof)
    cof$retailtypename <- rownames(cof)
    cof
    cof<-cof[-c(1:2),]
    cof
  }
  
  {#Ʒ�ྺ��ǿ��-qty���ݶbrand-share^2
    cor(dt_cat$qty,dt_cat$amt)#0.2341666������ʲô��˼
    brand_share<-dt_cat[ ,c(.(qty=sum(qty))),by=c("retailtypename","brandname")]
    brand_share
    brand_share<-brand_share %>% group_by(retailtypename) %>% mutate(sum=sum(qty))
    brand_share<-arrange(brand_share,retailtypename)
    brand_share<-as.data.table(brand_share)
    brand_share$share<-brand_share$qty/brand_share$sum
    brand_share$share_square <- brand_share$share * brand_share$share
    brand_share
    #���� 
    intensity<-brand_share[ ,c(.(intensity=sum(share_square))),by="retailtypename"]
    #��cofͳһid��retailtypename
    intensity$retailtypename<-paste("retailtypename",intensity$retailtypename,sep="")
    intensity
    df<-left_join(cof,intensity,by="retailtypename")
    df
    lm_chr<-lm(cof~intensity,data=df)
    summary(lm_chr)
    #���ۣ�����ǿ��Խǿ���۸���Խ��;��Ȼ������
    {#char����storable
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