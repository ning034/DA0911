{
  #读入数据
  library(dplyr)
  #经下面brandname的检查剔除:延世种超高价格的数据
}

{#
  head(dt2016)
  length(unique(dt2016$id))
  range(dt2016$saledate)
  length(unique(dt2016$saledate))
  store <- dt2016 %>% group_by(storecode) %>% summarise(num=length(storecode),amt=sum(amt),qty=sum(qty),mprice=mean(price))
  arrange(store,desc(amt))
  
  length(unique(dt2016$storecode))
  
  #poscode
  length(unique(dt2016$poscode))
  poscode <- dt2016 %>% group_by(poscode) %>% summarise(amt=sum(amt),qty=sum(qty))
  arrange(poscode,desc(amt))
  
  #possalecode
  length(unique(dt2016$possalecode))
  possalecode <- dt2016 %>% group_by(possalecode) %>% summarise(amt=sum(amt),qty=sum(qty))
  arrange(possalecode,desc(amt))
  w <- which(possalecode$amt > 30000000)
  
  #saletime
  head(dt2016$saletime)
  w <- which(dt2016$saletime < 100000)
  summary(dt2016$saletime)
  plot(density(dt2016$saletime))
  
  #lessoncode
  length(unique(dt2016$lessoncode))
  lessoncode <- dt2016 %>% group_by(lessoncode) %>% summarise(amt=sum(amt),qty=sum(qty))
  arrange(lessoncode,desc(amt))
  
  #areacode
  length(unique(dt2016$areacode))
  areacode <- dt2016 %>% group_by(areacode) %>% summarise(amt=sum(amt),qty=sum(qty))
  arrange(areacode,desc(amt))
  w <- which(areacode$amt > 100000000)
  
  #countercode
  length(unique(dt2016$countercode))
  countercode <- dt2016 %>% group_by(countercode) %>% summarise(amt=sum(amt),qty=sum(qty))
  arrange(countercode,desc(amt))
  w <- which(countercode$amt > 100000000)
  
  #itemcode
  length(unique(dt2016$itemcode))
  length(unique(dt2016$barcode))
  ic <- dt2016 %>% group_by(itemcode) %>% summarise(n=length(unique(barcode)))
  ic
  w <- which(ic$n>1)
  ic[w,]
  subset(dt2016,itemcode==2598)
  
  #qty-减去==0的数据#####
  summary(dt2016$qty)
  min(dt2016$qty)
  which(dt2016$qty==0)
  dt2016[9990309,]
  
  #price
  summary(dt2016$price)
  
  #posdiscountamt
  summary(dt2016$posdiscountamt)
  w <- which(dt2016$posdiscountamt > 0)
  694651/nrow(dt2016)
  length(unique(dt2016$posdiscountamt))
  
  #vipdiscountamt
  summary(dt2016$vipdiscountamt)
  w <- which(dt2016$vipdiscountamt > 0)
  95375/nrow(dt2016)
  length(unique(dt2016$vipdiscountamt))
  
  #2.贵宾卡重新清洗，加上x型####
  length(unique(dt2016$vipcardtype))
  unique(dt2016$vipcardtype)
  dt2016 %>% group_by(vipcardtype) %>% summarise(money=sum(vipdiscountamt))
  
  #discountoutamt
  summary(dt2016$discountoutamt)
  w <- which(dt2016$discountoutamt > 0)
  353316/nrow(dt2016)
  length(unique(dt2016$discountoutamt))
  
  #discountinamt
  summary(dt2016$discountinamt)
  w <- which(dt2016$discountinamt > 0)
  171708/nrow(dt2016)
  length(unique(dt2016$discountinamt))
  
  #surrogatemode
  unique(dt2016$surrogatemode)
  
  #merchantdiscountamt
  summary(dt2016$merchantdiscountamt)
  w <- which(dt2016$merchantdiscountamt > 0)
  16430/nrow(dt2016)
  length(unique(dt2016$merchantdiscountamt))
  
  #ordercode
  length(which(is.na(dt2016$ordercode)))
  9318835/nrow(dt2016)
  
  #barcode
  length(unique(dt2016$barcode))
  
  #itemname
  length(unique(dt2016$itemname))
  
  #itemshortname
  length(unique(dt2016$itemshortname))
  
  #brandcode
  length(unique(dt2016$brandcode))
  brand <- dt2016[,c(.(n=length(unique(brandname)))),by="brandcode"]
  brand
  subset(dt2016,brandcode==214)
  
  #retailtypecode
  length(unique(dt2016$retailtypecode))
  retail <- dt2016[,c(.(n=length(unique(retailtypename)))),by="retailtypecode"]
  retail
  subset(dt2016,retailtypecode==90)
  
  #itemtypecode
  length(unique(dt2016$itemtypecode))
  
  #producingarea
  w <- which(is.na(dt2016$producingarea))
  length(unique(dt2016$producingarea))
  unique(dt2016$producingarea)
  
  #returengoods
  w <- which(is.na(dt2016$returngoods))
  subset(dt2016,returngoods=="")
  length(unique(dt2016$returngoods))
  table(dt2016$returngoods)
  dt2016[,sum(qty),by="returngoods"]
  
  #specification
  w <- which(is.na(dt2016$specification))
  subset(dt2016,specification=="")#2684972
  2684972/nrow(dt2016)
  length(unique(dt2016$specification))
  
  #productcode
  length(unique(dt2016$productcode))
  
  #grade
  w <- which(is.na(dt2016$grade))
  subset(dt2016,grade=="")#1769020
  1769020/nrow(dt2016)
  length(unique(dt2016$grade))
  subset(dt2016,barcode==2000000435398)
  
  #color
  w <- which(is.na(dt2016$color))#2135096
  subset(dt2016,color=="")#8050783
  (8050783+2135096)/nrow(dt2016)
  length(unique(dt2016$color))
  unique(dt2016$color)
  
  #casing
  w <- which(is.na(dt2016$casing))#985095
  subset(dt2016,casing=="")#0
  (985095)/nrow(dt2016)
  length(unique(dt2016$casing))
  unique(dt2016$casing)
  
  #dimension
  w <- which(is.na(dt2016$dimension))#8724281
  subset(dt2016,dimension=="")#1462012
  (1462012+8724281)/nrow(dt2016)
  length(unique(dt2016$dimension))
  unique(dt2016$dimension)
  
  #itemcodetype
  w <- which(is.na(dt2016$itemcodetype))#
  subset(dt2016,itemcodetype=="")#1
  length(unique(dt2016$itemcodetype))
  unique(dt2016$itemcodetype)
  dt2016[,sum(amt),by="itemcodetype"]
  
  #unit
  w <- which(is.na(dt2016$unit))#
  subset(dt2016,unit=="")#
  length(unique(dt2016$unit))
  unique(dt2016$unit)
  
  #property
  w <- which(is.na(dt2016$property))#
  subset(dt2016,property=="")#
  length(unique(dt2016$property))
  unique(dt2016$property)
  dt2016[,sum(amt),by="property"]
  
  #itemtypename
  length(unique(dt2016$itemtypename))
  unique(dt2016$itemtypename)
  item <- dt2016[,sum(amt),by="itemtypename"]
  item[order(V1,decreasing = T),]
  
  #retailtypename
  length(unique(dt2016$retailtypename))
  unique(dt2016$retailtypename)
  retail <- dt2016[,sum(amt),by="retailtypename"]
  retail[order(V1,decreasing = T),]
  
  
  #brandname
  length(unique(dt2016$brandname))
  brandname <- dt2016[,sum(amt),by="brandname"]
  brandname[order(V1,decreasing = T),]
  
  #mccgroup
  length(unique(dt2016$mccgroup))
  unique(dt2016$mccgroup)
  retail <- dt2016[,sum(amt),by="mccgroup"]
  retail[order(V1,decreasing = T),]
  
  write.csv(dt2016,file="D:/D/data/xuhuibusiness/dt2016.csv",row.names = F)
  
}

{#
  dt2016
  w <- which(dt2016$qty==0)
  dt2016 <- dt2016[-w,]
  w <- which(is.na(dt2016$vipcardtype) & dt2016$vipdiscountamt > 0)
  dt2016[w,]$vipcardtype <- "x"#没成功
}