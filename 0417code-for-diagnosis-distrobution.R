
library(tidyverse)
library(data.table)
library(readxl)


hs.dz=read.csv("h:/data/gaoyue1221合并/0205/华山对照组0205.csv")
hs.sy=read.csv("h:/data/gaoyue1221合并/0205/华山实验组0205.csv")


table2.dzmzhz<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_对照门诊汇总_new.csv",header=T)
table2.dzzyhz<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_对照住院汇总_new.csv",header=T)
table2.symzhz<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_实验门诊汇总_new.csv",header=T)
table2.syzyhz<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_实验住院汇总_new.csv",header=T)

#华山对照组ID起找

hs.dz.id=hs.dz%>%select(MEDCARD,startday,oneyear)
hs.dz.id[,1:3]=lapply(hs.dz.id[,1:3],function(x){as.character(x)})
hs.dz.id=hs.dz.id%>%rename(ID=MEDCARD)
#对照组格式处理
table2.dzmzhz$MAX.E.KHMZH.=as.character(table2.dzmzhz$MAX.E.KHMZH.)
table2.dzmzhz$MAX.KFRQ.=as.character(table2.dzmzhz$MAX.KFRQ.)
table2.dzmzhz=table2.dzmzhz%>%rename(ID=MAX.E.KHMZH.)

table2.dzzyhz$MEDCARD=as.character(table2.dzzyhz$MEDCARD)
table2.dzzyhz$ENTDATE=as.character(table2.dzzyhz$ENTDATE)
table2.dzzyhz=table2.dzzyhz%>%rename(ID=MEDCARD)
#1
hs.dz.id1=left_join(hs.dz.id,table2.dzmzhz,by='ID')
hs.dz.id2=left_join(hs.dz.id,table2.dzzyhz,by='ID')

hs.dz.id2=na.omit(hs.dz.id2)

#time filtrate
hs.dz.id1$MAX.KFRQ.=as.Date(hs.dz.id1$MAX.KFRQ.)
hs.dz.id1$startday=as.Date(hs.dz.id1$startday)
hs.dz.id1$oneyear=as.Date(hs.dz.id1$oneyear)

hs.dz.id1.1=hs.dz.id1%>%
  filter(MAX.KFRQ.>=startday&MAX.KFRQ.<=oneyear)

hs.dz.id2[,c(2,3,8)]=lapply(hs.dz.id2[,c(2,3,8)], function(x){as.Date(x)})
hs.dz.id2.2=hs.dz.id2%>%
  filter(ENTDATE>=startday&ENTDATE<=oneyear)

#########################################################..........................................................................................

#华山实验组ID起找

hs.sy.id=hs.sy%>%select(ID,startday,oneyear)
hs.sy.id[,1:3]=lapply(hs.sy.id[,1:3],function(x){as.character(x)})

#实验组格式处理
table2.symzhz$MAX.E.KHMZH.=as.character(table2.symzhz$MAX.E.KHMZH.)
table2.symzhz$MAX.KFRQ.=as.character(table2.symzhz$MAX.KFRQ.)
table2.symzhz=table2.symzhz%>%rename(ID=MAX.E.KHMZH.)

table2.syzyhz$MEDCARD=as.character(table2.syzyhz$MEDCARD)
table2.syzyhz$ENTDATE=as.character(table2.syzyhz$ENTDATE)
table2.syzyhz=table2.syzyhz%>%rename(ID=MEDCARD)

#2实验
hs.sy.id1=left_join(hs.sy.id,table2.symzhz,by="ID")
hs.sy.id2=left_join(hs.sy.id,table2.syzyhz,by='ID')

hs.sy.id2=na.omit(hs.sy.id2)

#time filtrate
hs.sy.id1[,c(2,3,9)]=lapply(hs.sy.id1[,c(2,3,9)], function(x){as.Date(x)})
hs.sy.id1.1=hs.sy.id1%>%
  filter(MAX.KFRQ.>=startday&MAX.KFRQ.<=oneyear)

hs.sy.id2$ENTDATE=substr(hs.sy.id2$ENTDATE,1,10)
hs.sy.id2[,c(2,3,8)]=lapply(hs.sy.id2[,c(2,3,8)], function(x){as.Date(x)})
hs.sy.id2.2=hs.sy.id2%>%
  filter(ENTDATE>=startday&ENTDATE<=oneyear)

#####...............................................................................................................................

#中山医院数据导入


zs.dz=read.csv("h:/data/gaoyue1221合并/0205/中山对照组0205.csv")
zs.sy=read.csv("h:/data/gaoyue1221合并/0205/中山实验组0205.csv")

#中山医院++实验组
data2.mz.zy=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛&注射用丹参多酚酸盐/1门诊-住院病人信息对照&丹参多酚酸盐.xlsx")
data2.mz.gh<-read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛&注射用丹参多酚酸盐/挂号信息.xlsx")
data2.zy.xx=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛&注射用丹参多酚酸盐/住院信息.xlsx")

#中山医院++对照组
data.dz.gh1=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛/挂号信息1.xlsx")
data.dz.gh2=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛/挂号信息2.xlsx")
data.dz.zy=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛/住院病人信息.xlsx")
data.dz.mz.zy=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛/2门诊-住院病人信息对照.xlsx")

data.dz.mz.zy1=data.dz.mz.zy%>%rename(门诊号=CARD_NO,住院流水号=INPATIENT_NO)


data.dz.gh=rbind(data.dz.gh1,data.dz.gh2)
data.dz.gh$year=substr(data.dz.gh$挂号时间,1,4)
data.dz.gh=data.dz.gh[!is.na(data.dz.gh$出生日期),]
data.dz.gh$age<-2017-as.numeric(substr(data.dz.gh$出生日期,1,4))
data.dz.gh=data.dz.gh%>%filter(age<120)

#去掉实际没有就诊的患者，挂号费为负值的
data.dz.gh3=data.dz.gh[duplicated(data.dz.gh$就诊流水号),][,"就诊流水号"]
data.dz.gh.4=anti_join(data.dz.gh,data.dz.gh3,by="就诊流水号")

#中山实验组 ID 起找

zs.sy.id=zs.sy%>%select(门诊号,startday,oneyear)
zs.sy.id1=left_join(zs.sy.id,data2.mz.gh,by='门诊号')
sum(duplicated(zs.sy.id1$就诊流水号))
zs.sy.id1=zs.sy.id1[!duplicated(zs.sy.id1$就诊流水号),]
zs.sy.id1[,c(2,3,10)]=lapply(zs.sy.id1[,c(2,3,10)], function(x){as.character(x)})
zs.sy.id1[,c(2,3,10)]=lapply(zs.sy.id1[,c(2,3,10)], function(x){as.Date(x)})


zs.sy.id1.1=zs.sy.id1%>%
  filter(挂号时间>=startday&挂号时间<=oneyear)

#住院号=MCARD_NO
#门诊号=CARD_NO
#住院流水号=INPATIENT_NO

data2.zy.xx1=left_join(zs.sy.id,data2.mz.zy%>%rename(门诊号=CARD_NO,住院流水号=INPATIENT_NO),by='门诊号')%>%
  left_join(data2.zy.xx,by='住院流水号')

data2.zy.xx1=data2.zy.xx1%>%
  filter(诊断类型=='主要诊断')

data2.zy.xx1[,c(2,3,11)]=lapply(data2.zy.xx1[,c(2,3,11)], function(x){as.character(x)})
data2.zy.xx1[,c(2,3,11)]=lapply(data2.zy.xx1[,c(2,3,11)], function(x){as.Date(x)})

zs.sy.id2.2=data2.zy.xx1%>%
  filter(入院日期>=startday&入院日期<=oneyear)

######.........................................................................................................................................................

#中山对照组

zs.dz.id=zs.dz%>%select(1,2,4)
data.dz.gh.4$门诊号=as.numeric(data.dz.gh.4$门诊号)
zs.dz.id1=left_join(zs.dz.id,data.dz.gh.4,by='门诊号')
zs.dz.id1[,c(2,3,10)]=lapply(zs.dz.id1[,c(2,3,10)], function(x){as.character(x)})
zs.dz.id1[,c(2,3,10)]=lapply(zs.dz.id1[,c(2,3,10)], function(x){as.Date(x)})


zs.dz.id1.1=zs.dz.id1%>%
  filter(挂号时间>=startday&挂号时间<=oneyear)


data.dz.mz.zy1$门诊号=as.numeric(data.dz.mz.zy1$门诊号)

data.dz.zy1=left_join(zs.dz.id,data.dz.mz.zy1,by='门诊号')%>%
  left_join(data.dz.zy,by='住院流水号')

data.dz.zy1=data.dz.zy1%>%
  filter(诊断类型=='主要诊断')

data.dz.zy1[,c(2,3,12)]=lapply(data.dz.zy1[,c(2,3,12)], function(x){as.character(x)})
data.dz.zy1[,c(2,3,12)]=lapply(data.dz.zy1[,c(2,3,12)], function(x){as.Date(x)})

zs.dz.id2.2=data.dz.zy1%>%
  filter(入院日期>=startday&入院日期<=oneyear)


############......................................................................................................................................................................

hs.sy.id1.1.1=hs.sy.id1.1%>%select(1,2,3,7,12)
hs.sy.id2.2.2=hs.sy.id2.2%>%select(1,2,3,5,7)


hs.sy.zd=c(hs.sy.id1.1.1[,5]%>%as.character(),hs.sy.id2.2.2[,5]%>%as.character())%>%
  as.data.frame()%>%
  table()%>%
  as.data.frame()
hs.sy.zd$zd=hs.sy.zd$.

hs.dz.id1.1.1=hs.dz.id1.1%>%select(1,2,3,7,12)
hs.dz.id2.2.2=hs.sy.id2.2%>%select(1,2,3,5,7)

hs.dz.zd=c(hs.dz.id1.1.1[,5]%>%as.character(),hs.dz.id2.2.2[,5]%>%as.character())%>%
  as.data.frame()%>%
  table()%>%
  as.data.frame()
hs.dz.zd$zd=hs.dz.zd$.

zs.sy.id1.1.1=zs.sy.id1.1%>%select(1,2,3,9,16)
zs.sy.id2.2.2=zs.sy.id2.2%>%select(1,2,3,5,15)

zs.sy.zd=c(zs.sy.id1.1.1[,5]%>%as.character(),zs.sy.id2.2.2[,5]%>%as.character())%>%
  as.data.frame()%>%
  table()%>%
  as.data.frame()
zs.sy.zd$zd=zs.sy.zd$.

zs.dz.id1.1.1=zs.dz.id1.1%>%select(1,2,3,9,16)
zs.dz.id2.2.2=zs.dz.id2.2%>%select(1,2,3,5,16)

zs.dz.zd=c(zs.dz.id1.1.1[,5]%>%as.character(),zs.dz.id2.2.2[,5]%>%as.character())%>%
  as.data.frame()%>%
  table()%>%
  as.data.frame()
zs.dz.zd$zd=zs.dz.zd$.


#实验组

hz.sy.zd=rbind(hs.sy.zd,zs.sy.zd)

for(i in 1:nrow(hz.sy.zd)){
  if(grepl("冠心病",hz.sy.zd[i,"zd"])) hz.sy.zd[i,"诊断类别"]<-"冠心病"
  if(grepl("心绞痛",hz.sy.zd[i,"zd"])) hz.sy.zd[i,"诊断类别"]<-"心绞痛"
  if((grepl("冠心病",hz.sy.zd[i,"zd"]))&(grepl("心绞痛",hz.sy.zd[i,"zd"]))) hz.sy.zd[i,"诊断类别"]<-"冠心病&心绞痛"
  if(grepl("脑梗",hz.sy.zd[i,"zd"])) hz.sy.zd[i,"诊断类别"]<-"卒中"
  if((grepl("冠心病",hz.sy.zd[i,"zd"]))&(grepl("脑梗",hz.sy.zd[i,"zd"]))) hz.sy.zd[i,"诊断类别"]="冠心病&卒中"
  if(grepl("心肌梗死",hz.sy.zd[i,"zd"])) hz.sy.zd[i,"诊断类别"]<-"心梗"
  if((grepl("冠心病",hz.sy.zd[i,"zd"]))&((grepl("心肌梗死",hz.sy.zd[i,"zd"]))|(grepl("心梗",hz.sy.zd[i,"zd"])))) hz.sy.zd[i,"诊断类别"]="冠心病&心梗"
}

hz.sy.zd1=na.omit(hz.sy.zd)

hz.sy.zd1=hz.sy.zd1%>%
  group_by(诊断类别)%>%
  summarise(频数=sum(Freq))%>%
  mutate(频率=round(频数/sum(频数),4))

write.table(hz.sy.zd1,file = '实验组诊断分布.csv',row.names=F)

#对照组
hz.dz.zd=rbind(hs.dz.zd,zs.dz.zd)

for(i in 1:nrow(hz.dz.zd)){
  if(grepl("冠心病",hz.dz.zd[i,"zd"])) hz.dz.zd[i,"诊断类别"]<-"冠心病"
  if(grepl("心绞痛",hz.dz.zd[i,"zd"])) hz.dz.zd[i,"诊断类别"]<-"心绞痛"
  if((grepl("冠心病",hz.dz.zd[i,"zd"]))&(grepl("心绞痛",hz.dz.zd[i,"zd"]))) hz.dz.zd[i,"诊断类别"]<-"冠心病&心绞痛"
  if(grepl("脑梗",hz.dz.zd[i,"zd"])) hz.dz.zd[i,"诊断类别"]<-"卒中"
  if((grepl("冠心病",hz.dz.zd[i,"zd"]))&(grepl("脑梗",hz.dz.zd[i,"zd"]))) hz.dz.zd[i,"诊断类别"]="冠心病&卒中"
  if(grepl("心肌梗死",hz.dz.zd[i,"zd"])) hz.dz.zd[i,"诊断类别"]<-"心梗"
  if((grepl("冠心病",hz.dz.zd[i,"zd"]))&((grepl("心肌梗死",hz.dz.zd[i,"zd"]))|(grepl("心梗",hz.dz.zd[i,"zd"])))) hz.dz.zd[i,"诊断类别"]="冠心病&心梗"
}

hz.dz.zd1=na.omit(hz.dz.zd)

hz.dz.zd1=hz.dz.zd1%>%
  group_by(诊断类别)%>%
  summarise(频数=sum(Freq))%>%
  mutate(频率=round(频数/sum(频数),4))

write.table(hz.dz.zd1,file = '对照组诊断分布.csv',row.names=F)

































