{
  ###1.��������####
  setwd("D:/D/data/xuhuibusiness/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(papeR)
  library(cowplot)
  #dt2016<-fread("data2016_price.csv",header = T,sep=",")#10186302
  
  #�ؼ�������ʲô��
  #(1)���ڡ����������۶�ۿ۶�۸񣬵��Ǽ۸����˼���
  #(2)������Ϣ�����̡�Ʒ�ࡢƷ�ƣ���Ʒ������̫��
  #(3)ʱ������Ҳ���ǿ��Դ��µĵ�
}


{#���������۶�ۿ۶�####
  
  #ȥ�������쳣ֵ
  dt2016
  summary(dt2016$qty)
  quantile(dt2016$qty,0.999)
  dt2016 <- subset(dt2016,qty <= 30)
  gc()
  dt2016
  
  #ȥ�����۶��쳣ֵ
  quantile(dt2016$amt,0.99)
  dt2016 <- subset(dt2016,amt <= 7960)
  gc()
  dt2016
  
  #����-��-����-����
  qty <- dt2016[,c(.(q=sum(qty)),.(amt=sum(amt)),.(discount=sum(posdiscountamt)+sum(vipdiscountamt)+sum(discountoutamt)+sum(discountinamt)+sum(merchantdiscountamt))),by="saledate"]
  qty$saledate <- as.Date(qty$saledate)
  qty$week2 <- weekdays(qty$saledate)
  qty$week <- week(qty$saledate)
  qty$month <- month(qty$saledate)
  qty$quarter <- quarter(qty$saledate)
  qty
  
  #2016���������仯��5-6���������߷壬��Ϊ��ʱ�Ǵ���ǰ��Ľ׶Σ���53������������Ϊ��53��ֻ��3�죻
  #2016�������۶�仯��37-52�ܵ������۶�Զ����20-36�ܣ���ˣ�9-12�»���ļ������۶�Ҳ�ϸ�
  #2016����/��/�����ۿ۶�仯�������۶�仯��ģʽ�Ƚ�����
  qty_w <- qty[,c(.(q=sum(q)),.(amt=sum(amt)),.(discount=sum(discount))),by="week"]
  qty_w$q <- qty_w$q / 10000 #��
  qty_w$amt <- qty_w$amt / 10000000 #ǧ��
  qty_w$discount <- qty_w$discount / 10000000 #ǧ��
  qty_w
  p1 <- ggplot(qty_w,aes(as.factor(week),q,group=1))+geom_line()+geom_point()+labs(x="����",y="����(��)")+theme_bw()
  p2 <- ggplot(qty_w,aes(as.factor(week),amt,group=1))+geom_line()+geom_point()+labs(x="����",y="���۶�(ǧ��Ԫ)")+theme_bw()
  p3 <- ggplot(qty_w,aes(as.factor(week),discount,group=1))+geom_line()+geom_point()+labs(x="����",y="�ۿ۶�(ǧ��Ԫ)")+theme_bw()
  plot_grid(p1,p2,p3,ncol = 1)
  
  #2016���¶������仯��3-6�·ݡ�9-12�·��������͹�
  qty_m <- qty[,c(.(q=sum(q)),.(amt=sum(amt)),.(discount=sum(discount))),by="month"]
  qty_m$q <- qty_m$q / 10000
  qty_m$amt <- qty_m$amt / 100000000 #��Ԫ
  qty_m$discount <- qty_m$discount / 100000000 #��Ԫ
  qty_m
  p1 <- ggplot(qty_m,aes(as.factor(month),q,group=1))+geom_line()+geom_point()+labs(x="�·�",y="����(��)")+theme_bw()
  p2 <- ggplot(qty_m,aes(as.factor(month),amt,group=1))+geom_line()+geom_point()+labs(x="�·�",y="���۶�(��Ԫ)")+theme_bw()
  p3 <- ggplot(qty_m,aes(as.factor(month),discount,group=1))+geom_line()+geom_point()+labs(x="�·�",y="�ۿ۶�(��Ԫ)")+theme_bw()
  plot_grid(p1,p2,p3,ncol = 1)
  
  #2016�꼾�������仯:���ļ����������͹�
  qty_q <- qty[,c(.(q=sum(q)),.(amt=sum(amt)),.(discount=sum(discount))),by="quarter"]
  qty_q$q <- qty_q$q / 10000
  qty_q$amt <- qty_q$amt / 100000000 #��Ԫ
  qty_q$discount <- qty_q$discount / 100000000 #��Ԫ
  qty_q
  p1 <- ggplot(qty_q,aes(quarter,q))+geom_line()+ geom_point()+labs(x="����",y="����(��)")+theme_bw()
  p2 <- ggplot(qty_q,aes(quarter,amt))+geom_line()+ geom_point()+labs(x="����",y="���۶�(��Ԫ)")+theme_bw()
  p3 <- ggplot(qty_q,aes(quarter,discount))+geom_line()+ geom_point()+labs(x="����",y="�ۿ۶�(��Ԫ)")+theme_bw()
  plot_grid(p1,p2,p3,ncol = 1)
}