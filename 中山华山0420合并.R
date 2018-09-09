

##2018年4月26号
##将中山和华山实验组和对照组的数据进行合并
##author  xv
###
library(readr)
library(tidyverse)

zs.dz=read.csv('h:/data/gaoyue1221合并/0417增加/中山对照组0419随访2年结果.csv')
zs.sy=read.csv('h:/data/gaoyue1221合并/0417增加/中山实验组0419随访2年结果.csv')

hs.dz=read.csv('h:/data/gaoyue1221合并/0417增加/华山对照组0423随访2年.csv')
hs.sy=read.csv('h:/data/gaoyue1221合并/0417增加/华山实验组0423随访2年结果.csv')

library(knitr)
library(broom)
mystats<-function(x,na.omit=FALSE){
  y=sum(is.na(x))
  if(na.omit)
    x<-x[!is.na(x)]
  l=length(x)
  m<-round(mean(x),4)
  n<-round(sd(x),4)
  o<-round(median(x),4)
  p<-round(quantile(x,0.25),4)
  q<-round(quantile(x,0.75),4)
  r<-round(min(x),4)
  s<-round(max(x),4)
  t<-round(length(which(x==0)),4)
  u<-round(length(which(x>0)),4)
  return(c(n=l,缺失值=y,mean=m,sd=n,median=o,median1=p,
           median3=q,min=r,max=s,等于0的个数=t,大于0的个数=u))
}


#查看实验组数据的名字

cbind(kable(names(hs.sy)),kable(names(zs.sy)))%>%as.data.frame()


zs.sy$end_start1=ifelse(zs.sy$end_start1<1,1,zs.sy$end_start1)
zs.sy1=zs.sy%>%
  rename(ID=门诊号)%>%
  mutate(一年心血管次数=门诊心血管次数+住院心血管次数,
                门诊加住院所有心血管次数=门诊所有时间心血管次数+住院所有时间心血管次数,
                门诊发生频率=门诊所有时间心血管次数/end_start1,
                总发生频率=(门诊所有时间心血管次数+住院所有时间心血管次数)/end_start1,
                门诊加住院费用=门诊费用+住院费用,
                心血管资源门诊加住院费用=心血管资源门诊费用+心血管资源住院费用)%>%
  select(-c(住院药品费用,住院非药品费用,心血管资源住院药品费用,心血管资源非药品费用))
str(zs.sy1)
  
  hs.sy1=hs.sy%>%
    rename(门诊所有时间心血管次数=门诊所有心血管次数,住院所有时间心血管次数=住院所有心血管次数,
                      性别=MAX.DECODE.E.XINGB.0..男...女.,age=MAX.SENGR.,保险类别=MAX.DECODE.E.JZFSM..1...自费..,
                      科室名称=MAX.DEPTNAME.,心血管资源门诊费用=门诊心血管资源费用)%>%
    select(-c(性别,age,保险类别,科室名称))
  
  hs.sy1$一年心血管次数=hs.sy1$门诊心血管次数+hs.sy1$住院心血管次数
  hs.sy1$门诊加住院费用=hs.sy1$门诊费用+hs.sy1$住院费用
  hs.sy1$心血管资源门诊加住院费用=hs.sy1$心血管资源门诊费用+hs.sy1$心血管资源住院费用
  str(hs.sy1)

zs.hs.sy=rbind(zs.sy1,hs.sy1)

#查看是否有缺失值
kable(sapply(zs.hs.sy,function(x){sum(is.na(x))}))

str(zs.hs.sy)
#连续变量的统计
sy.tj=lapply(zs.hs.sy[,c(6:22)],function(x){mystats(x)})%>%as.data.frame.list()%>%t()%>%as.data.frame()


#########.............................................................

###对照组合并

cbind(kable(names(hs.dz)),kable(names(zs.dz)))%>%as.data.frame()

#整理华山医院对照组

hs.dz1=hs.dz%>%
  rename(门诊所有时间心血管次数=门诊所有心血管次数,住院所有时间心血管次数=住院所有心血管次数,
                    性别=MAX.DECODE.E.XINGB.0..男...女.,age=MAX.SENGR.,保险类别=MAX.DECODE.E.JZFSM..1...自费..,
                    科室名称=MAX.DEPTNAME.,门诊费用=门诊总费用,ID=门诊号)%>%
  mutate(一年心血管次数=门诊心血管次数+住院心血管次数,
                门诊加住院所有心血管次数=门诊所有时间心血管次数+住院所有时间心血管次数)
hs.dz1=hs.dz1%>%select(-c(17:23))


#整理中山医院对照组
zs.dz1=zs.dz%>%
  rename(ID=门诊号)%>%
  mutate(门诊加住院总费用=门诊费用+住院费用,心血管资源门诊加住院总费用=心血管资源门诊费用+心血管资源住院费用,
                 门诊发生频率=门诊所有时间心血管次数/end_start1,
                 总发生频率=(门诊所有时间心血管次数+住院所有时间心血管次数)/end_start1,
                 一年心血管次数=门诊心血管次数+住院心血管次数,
                 门诊加住院所有心血管次数=门诊所有时间心血管次数+住院所有时间心血管次数)%>%
  select(-c(住院药品费用,住院非药品费用,心血管资源住院药品费用,心血管资源非药品费用))

dz.name=cbind(kable(names(hs.dz1)),kable(names(zs.dz1)))%>%as.data.frame()











