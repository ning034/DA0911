{
  ###1.��������####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
}

{#��������
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

{#����Ʒ��۸��Գ���
  dt_cat$week<-week(dt_cat$saledate)
  gc()
  dt_cat
  #p_elstic<-dt_cat%>%group_by(retailtypename,week)%>%summarise(qty=sum(qty),price_ac=mean(amt/qty),price_ori=mean(price))
  #��һ�д�����mean(amt/qty)������������⣬��֪��Ϊʲô��
  p_elstic <- dt_cat[,c(.(qty=sum(qty)) ,.(price_ac=mean(amt/qty)) ,.(price_ori=mean(price))) ,by=c("retailtypename","week")]
  gc()
  head(p_elstic)
  p_elstic<-p_elstic[order(retailtypename),]
  p_elstic
  lm_elstic<-lm(log(qty)~log(price_ac)+retailtypename,data=p_elstic)
  summary(lm_elstic)
  {#����Ʒ�������Լ۸��Ե�Ӱ��
    cof<-lm_elstic$coefficients
    cof<-as.data.frame(cof)
    cof$cof<-cof$cof-0.004127701#�������������������Ϣ
    cof
    rownames(cof)
    cof$retailtypename <- rownames(cof)
    cof
    cof<-cof[-c(1:2),]
    cof
    {#Ʒ�ྺ��ǿ��-qty���ݶbrand-share^2
      cor(dt_cat$qty,dt_cat$amt)#-0.0001310391������ʲô��˼
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
      #���ۣ�����ǿ��Խǿ���۸�������Խ��;��Ȼ������
    }
    
    {#Ʒ�ྺ��ǿ��-amt���ݶbrand-share^2
      cor(dt_cat$qty,dt_cat$amt)#-0.0001310391������ʲô��˼
      brand_share<-dt_cat[ ,c(.(qty=sum(amt))),by=c("retailtypename","brandname")]
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
      #���ۣ�����ǿ��Խǿ���۸�������Խ��;��Ȼ������;��Ϊ������������û�п��ǽ�ȥ�����Բв�����������������
      #�����������ģ�Understanding the Characteristics of Price Elasticities for Frequently Purchased Packaged Goods
      #�ټ��ϣ�Ʒ�������ı��������Ƿ���ʳ�Ʒ����Լ۸񡢿ɴ洢��
      #����Ʒ�������������г��ݶ��Լ۸񡢴��۳̶ȡ��۸񷽲�(�������)���ۿ�Ƶ��
      #���Ҫ�����������д���ģ���ʲô���µ㣿���ǷǲΡ���ɨ�����ݣ����ǿ���������ϸ��
      #��������ϸ��������ʲô��ΪʲôҪ������ϸ�أ��Ա����߲�ͬ������ԭ�򡣡��������벻������������⣬����������
      #һ�����������ķ��򣺲�����ƪ���ĵı������ף��������������ƪ���¶�����ʲô��Ҳ�������ҵ�д����������Ѿ���������������˵�Ķ�����
    }
  }
  
  
}