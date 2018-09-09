
library(readxl)
library(tidyverse)
library(data.table)

set.seed(108)
#中山医院
#实验组

#中山医院++实验组
data2.mz.zy=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛&注射用丹参多酚酸盐/1门诊-住院病人信息对照&丹参多酚酸盐.xlsx")
data2.mz.gh<-read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛&注射用丹参多酚酸盐/挂号信息.xlsx")
data2.mz.fy=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛&注射用丹参多酚酸盐/门诊费用记录.xlsx")
data2.zy.xx=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛&注射用丹参多酚酸盐/住院信息.xlsx")
data2.zy.fyp=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛&注射用丹参多酚酸盐/（冠心病or心绞痛）&注射用丹参多酚酸盐住院非药品明细.xlsx")
data2.zy.yp=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛&注射用丹参多酚酸盐/（冠心病or心绞痛）&注射用丹参多酚酸盐住院药品明细.xlsx")


#obj2


#数据预处理
table.mz.case.sum=data2.mz.gh[duplicated(data2.mz.gh$就诊流水号),][,"就诊流水号"]
table.mz.case.sum1=anti_join(data2.mz.gh,table.mz.case.sum,by="就诊流水号")
table.mz.case.sum1$year=substr(table.mz.case.sum1$挂号时间,1,4)

#时间窗

table.mz.case.sum1=data.table(table.mz.case.sum1)
setkey(table.mz.case.sum1,门诊号,挂号时间)
hhh.id1=table.mz.case.sum1[!duplicated(table.mz.case.sum1$门诊号),"门诊号"]
for(i in 1:nrow(hhh.id1)){
  x=which(table.mz.case.sum1[,"门诊号"]==as.numeric(hhh.id1[i,"门诊号"]))
  y=table.mz.case.sum1[min(x),"挂号时间"]
  z=table.mz.case.sum1[max(x),"挂号时间"]
  hhh.id1[i,"startday"]=y
  hhh.id1[i,"endday"]=z
}

table.mz.case.sum2=left_join(table.mz.case.sum1,hhh.id1,by="门诊号")
table.mz.case.sum2=table.mz.case.sum2%>%
  mutate(时间窗=difftime(table.mz.case.sum2$挂号时间,table.mz.case.sum2$startday,units = "days"),
            end_start=difftime(table.mz.case.sum2$endday,table.mz.case.sum2$startday,units = "days"))%>%
  mutate(end_start1=end_start/365)
##
data2.mz.zy=data2.mz.zy%>%rename(门诊号=CARD_NO,住院流水号=INPATIENT_NO)
data2.mz.zy$门诊号=as.numeric(data2.mz.zy$门诊号)

xv.mz.case=table.mz.case.sum2
xv.mz.case.id=xv.mz.case[!duplicated(xv.mz.case$门诊号),c("门诊号","startday","end_start1")]
xv.mz.case.id$oneyear=paste0(substr(xv.mz.case.id$startday,1,3),as.character(as.numeric(substr(xv.mz.case.id$startday,4,4))+1),substr(xv.mz.case.id$startday,5,19))
xv.mz.case.id$oneyear=ifelse(substr(xv.mz.case.id$oneyear,1,4)=='2001',paste0(substr(xv.mz.case.id$startday,1,2),10,substr(xv.mz.case.id$startday,5,19)),xv.mz.case.id$oneyear)


xv.mz.case.id$门诊次数=rep(0,nrow(xv.mz.case.id))
xv.mz.case.id$门诊心血管次数=rep(0,nrow(xv.mz.case.id))
xv.mz.case.id$门诊所有时间心血管次数=rep(0,nrow(xv.mz.case.id))


for(i in 1:nrow(table.mz.case.sum2)){
  for (j in 1:nrow(xv.mz.case.id)) {
    if((table.mz.case.sum2[i,"门诊号"]==xv.mz.case.id[j,"门诊号"])&(as.Date(as.character(table.mz.case.sum2[i,"挂号时间"]))>=as.Date(as.character(xv.mz.case.id[j,"startday"])))&(as.Date(as.character(table.mz.case.sum2[i,"挂号时间"]))<=as.Date(as.character(xv.mz.case.id[j,"oneyear"])))){
      xv.mz.case.id[j,"门诊次数"]<-as.numeric(xv.mz.case.id[j,"门诊次数"])+1
    }
  }
}

for (i in 1:nrow(table.mz.case.sum2)) {
  for (j in 1:nrow(xv.mz.case.id)) {
    if((table.mz.case.sum2[i,"门诊号"]==xv.mz.case.id[j,"门诊号"])&(table.mz.case.sum2[i,"挂号时间"]>=xv.mz.case.id[j,"startday"])&(table.mz.case.sum2[i,"挂号时间"]<=xv.mz.case.id[j,"oneyear"])
       &(grepl("心",table.mz.case.sum2[i,"诊断"]))){
      xv.mz.case.id[j,"门诊心血管次数"]<-as.numeric(xv.mz.case.id[j,"门诊心血管次数"])+1
    }
  }
}


for(i in 1:nrow(table.mz.case.sum2)){
  for (j in 1:nrow(xv.mz.case.id)) {
    if((table.mz.case.sum2[i,"门诊号"]==xv.mz.case.id[j,"门诊号"])&(grepl("心",table.mz.case.sum2[i,"诊断"]))){
      xv.mz.case.id[j,"门诊所有时间心血管次数"]<-as.numeric(xv.mz.case.id[j,"门诊所有时间心血管次数"])+1
    }
  }
}




#住院

xv.zy.case=data2.mz.zy[,c("门诊号","住院流水号")]%>%inner_join(data2.zy.xx,by="住院流水号")
xv.zy.case1=xv.zy.case%>%filter(诊断类型=="主要诊断")
xv.mz.case.id$住院次数=rep(0,nrow(xv.mz.case.id))
xv.mz.case.id$住院心血管次数=rep(0,nrow(xv.mz.case.id))
xv.mz.case.id$住院天数=rep(0,nrow(xv.mz.case.id))
xv.mz.case.id$住院所有时间心血管次数=rep(0,nrow(xv.mz.case.id))


for(i in 1:nrow(xv.zy.case1)){
  for (j in 1:nrow(xv.mz.case.id)) {
    if((xv.zy.case1[i,"门诊号"]==xv.mz.case.id[j,"门诊号"])&(xv.zy.case1[i,"入院日期"]>=xv.mz.case.id[j,"startday"])&(xv.zy.case1[i,"入院日期"]<=xv.mz.case.id[j,"oneyear"])){
      xv.mz.case.id[j,"住院次数"]<-as.numeric(xv.mz.case.id[j,"住院次数"])+1
    }
  }
}

for (i in 1:nrow(xv.zy.case1)) {
  for (j in 1:nrow(xv.mz.case.id)) {
    if((xv.zy.case1[i,"门诊号"]==xv.mz.case.id[j,"门诊号"])&(xv.zy.case1[i,"入院日期"]>=xv.mz.case.id[j,"startday"])&(xv.zy.case1[i,"入院日期"]<=xv.mz.case.id[j,"oneyear"])
       &((grepl("心",xv.zy.case1[i,"出院诊断"]))|(grepl("动脉",xv.zy.case1[i,"出院诊断"]))|(grepl("高血压",xv.zy.case1[i,"出院诊断"])))){
      xv.mz.case.id[j,"住院心血管次数"]<-as.numeric(xv.mz.case.id[j,"住院心血管次数"])+1
    }
  }
}

for (i in 1:nrow(xv.zy.case1)) {
  for (j in 1:nrow(xv.mz.case.id)) {
    if((xv.zy.case1[i,"门诊号"]==xv.mz.case.id[j,"门诊号"])&(xv.zy.case1[i,"入院日期"]>=xv.mz.case.id[j,"startday"])&(xv.zy.case1[i,"入院日期"]<=xv.mz.case.id[j,"oneyear"])
       &((grepl("心",xv.zy.case1[i,"出院诊断"]))|(grepl("动脉",xv.zy.case1[i,"出院诊断"]))|(grepl("高血压",xv.zy.case1[i,"出院诊断"])))){
      xv.mz.case.id[j,"住院天数"]<-as.numeric(xv.mz.case.id[j,"住院天数"])+xv.zy.case1[i,"住院天数"]
    }
  }
}

for (i in 1:nrow(xv.zy.case1)) {
  for (j in 1:nrow(xv.mz.case.id)) {
    if((xv.zy.case1[i,"门诊号"]==xv.mz.case.id[j,"门诊号"])&((grepl("心",xv.zy.case1[i,"出院诊断"]))|(grepl("动脉",xv.zy.case1[i,"出院诊断"]))|(grepl("高血压",xv.zy.case1[i,"出院诊断"])))){
      xv.mz.case.id[j,"住院所有时间心血管次数"]<-as.numeric(xv.mz.case.id[j,"住院所有时间心血管次数"])+1
    }
  }
}

#obj3

xv.mz.case.id$门诊费用=rep(0,nrow(xv.mz.case.id))
xv.mz.case.id$住院费用=rep(0,nrow(xv.mz.case.id))
xv.mz.case.id$住院药品费用=rep(0,nrow(xv.mz.case.id))
xv.mz.case.id$住院非药品费用=rep(0,nrow(xv.mz.case.id))

xv.mz.lsh=left_join(xv.mz.case.id,table.mz.case.sum1,by="门诊号")%>%filter(挂号时间>=startday&挂号时间<=oneyear)
data.sy.mz.id=xv.mz.lsh[,c("门诊号","就诊流水号")]
data.sy.mz.fy=left_join(data.sy.mz.id,data2.mz.fy,by="就诊流水号")
data.sy.mz.fy$合计金额[is.na(data.sy.mz.fy$合计金额)]<-0


#门诊费用
for (i in 1:nrow(xv.mz.case.id)) {
  x=which(data.sy.mz.fy[,"门诊号"]==as.character(xv.mz.case.id[i,"门诊号"]))
  xv.mz.case.id[i,"门诊费用"]=sum(data.sy.mz.fy[x,"合计金额"])
}

#心血管资源门诊费用

data.sy.mz.fy.heart=data.sy.mz.fy%>%
  left_join(data2.mz.gh,by="就诊流水号")

data.sy.mz.fy.heart1=data.sy.mz.fy.heart[c(grep("心",as.vector(as.matrix(as.data.frame(data.sy.mz.fy.heart[,"诊断"])))),
                                           grep("高血压",as.vector(as.matrix(as.data.frame(data.sy.mz.fy.heart[,"诊断"])))),
                                           grep("脑梗",as.vector(as.matrix(as.data.frame(data.sy.mz.fy.heart[,"诊断"])))),
                                           grep("动脉",as.vector(as.matrix(as.data.frame(data.sy.mz.fy.heart[,"诊断"]))))),]

data.sy.mz.fy.heart1=data.sy.mz.fy.heart1[!duplicated(data.sy.mz.fy.heart1$就诊流水号),]


data.sy.mz.fy1=data.sy.mz.fy.heart1%>%
  select(2)%>%
  left_join(data.sy.mz.fy,by="就诊流水号")

sum(!duplicated(data.sy.mz.fy1$就诊流水号))
sum(!duplicated(data.sy.mz.fy1$门诊号))

xv.mz.case.id$心血管资源门诊费用=rep(0,nrow(xv.mz.case.id))
for (i in 1:nrow(xv.mz.case.id)) {
  x=which(data.sy.mz.fy1[,"门诊号"]==as.character(xv.mz.case.id[i,"门诊号"]))
  xv.mz.case.id[i,"心血管资源门诊费用"]=sum(data.sy.mz.fy1[x,"合计金额"])
}


#住院费用
xv.zy.lsh=left_join(xv.mz.case.id,data2.mz.zy,by="门诊号")%>%
  inner_join(data2.zy.fyp%>%
               rename(住院流水号=住院号),by="住院流水号")%>%
  rename(非药品金额=金额)%>%
  filter(收费时间>=startday&收费时间<=oneyear)

xv.zy.lsh1=left_join(xv.mz.case.id,data2.mz.zy,by="门诊号")%>%
  inner_join(data2.zy.yp,by="住院流水号")%>%
  rename(药品金额=总金额)%>%
  filter(收费日期>=startday&收费日期<=oneyear)

for (i in 1:nrow(xv.mz.case.id)) {
  x=which(xv.zy.lsh1[,"门诊号"]==as.character(xv.mz.case.id[i,"门诊号"]))
  xv.mz.case.id[i,"住院药品费用"]=sum(xv.zy.lsh1[x,"药品金额"])
}

for (i in 1:nrow(xv.mz.case.id)) {
  x=which(xv.zy.lsh[,"门诊号"]==as.character(xv.mz.case.id[i,"门诊号"]))
  xv.mz.case.id[i,"住院非药品费用"]=sum(xv.zy.lsh[x,"非药品金额"])
}

xv.mz.case.id$住院费用=xv.mz.case.id$住院药品费用+xv.mz.case.id$住院非药品费用

#心血管资源住院费用

xv.zy.lsh.heart=xv.zy.case1%>%
  select(住院流水号)%>%
  left_join(xv.zy.lsh,by="住院流水号")

sum(is.na(xv.zy.lsh.heart$门诊号))
xv.zy.lsh.heart=na.omit(xv.zy.lsh.heart)

xv.zy.lsh1.heart=xv.zy.case1%>%
  select(住院流水号)%>%
  left_join(xv.zy.lsh1,by="住院流水号")

sum(is.na(xv.zy.lsh1.heart$门诊号))
xv.zy.lsh1.heart=na.omit(xv.zy.lsh1.heart)

xv.mz.case.id$心血管资源住院药品费用=rep(0,nrow(xv.mz.case.id))
xv.zy.lsh1.heart$门诊号=as.character(xv.zy.lsh1.heart$门诊号)
for(i in 1:nrow(xv.mz.case.id)){
  x=which(xv.zy.lsh1.heart[,"门诊号"]==as.character(xv.mz.case.id[i,"门诊号"]))
  if(length(x)!=0){
    xv.mz.case.id[i,"心血管资源住院药品费用"]=sum(xv.zy.lsh1.heart[x,"药品金额"])
  }
}
xv.mz.case.id$心血管资源非药品费用=rep(0,nrow(xv.mz.case.id))
xv.zy.lsh.heart$门诊号=as.character(xv.zy.lsh.heart$门诊号)
for(i in 1:nrow(xv.mz.case.id)){
  x=which(xv.zy.lsh.heart[,"门诊号"]==as.character(xv.mz.case.id[i,"门诊号"]))
  if(length(x)!=0){
    xv.mz.case.id[i,"心血管资源非药品费用"]=sum(xv.zy.lsh.heart[x,"非药品金额"])
  }
}

xv.mz.case.id$心血管资源住院费用=xv.mz.case.id$心血管资源住院药品费用+xv.mz.case.id$心血管资源非药品费用



#obj1基线

xv.mz.case.id1=xv.mz.case[!duplicated(xv.mz.case$门诊号),c("门诊号","出生日期","性别","保险类别","科室名称")]
xv.mz.case.id1$age=2017-as.numeric(substr(xv.mz.case.id1$出生日期,1,4))
xv.mz.case.id2=left_join(xv.mz.case.id,xv.mz.case.id1[,-2],by="门诊号")
write.table(xv.mz.case.id2,file = "h:/data/gaoyue1221合并/0110newcode/中山实验组.csv",sep=",",row.names=F)



#中山医院+对照组
data.dz.gh1=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛/挂号信息1.xlsx")
data.dz.gh2=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛/挂号信息2.xlsx")
data.dz.fyp=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛/冠心病or心绞痛住院非药品明细.xlsx")
data.dz.yp=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛/冠心病or心绞痛住院药品明细.xlsx")
data.dz.zy=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛/住院病人信息.xlsx")
data.dz.mz.zy=read_excel("h:/data/gaoyue/objective2&3/冠心病or心绞痛/2门诊-住院病人信息对照.xlsx")
data.dz.2006.2010=fread("h:/data/gaoyue/objective2&3/冠心病or心绞痛/2006-2010.csv")
data.dz.2011=fread("h:/data/gaoyue/objective2&3/冠心病or心绞痛/2011.csv")
data.dz.2012=fread("h:/data/gaoyue/objective2&3/冠心病or心绞痛/2012.csv")
data.dz.2013=fread("h:/data/gaoyue/objective2&3/冠心病or心绞痛/2013.csv")
data.dz.2014=fread("h:/data/gaoyue/objective2&3/冠心病or心绞痛/2014.csv")
data.dz.2015=fread("h:/data/gaoyue/objective2&3/冠心病or心绞痛/2015.csv")
data.dz.2016=fread("h:/data/gaoyue/objective2&3/冠心病or心绞痛/2016.csv")
data.dz.2017=fread("h:/data/gaoyue/objective2&3/冠心病or心绞痛/2017.csv")




#门诊


data.dz.gh=rbind(data.dz.gh1,data.dz.gh2)

data.dz.gh$year=substr(data.dz.gh$挂号时间,1,4)
data.dz.gh=data.dz.gh[!is.na(data.dz.gh$出生日期),]
data.dz.gh$age<-2017-as.numeric(substr(data.dz.gh$出生日期,1,4))
data.dz.gh=data.dz.gh%>%filter(age<120)

#去掉实际没有就诊的患者，挂号费为负值的
data.dz.gh3=data.dz.gh[duplicated(data.dz.gh$就诊流水号),][,"就诊流水号"]
data.dz.gh.4=anti_join(data.dz.gh,data.dz.gh3,by="就诊流水号")

data.dz.gh.4=data.table(data.dz.gh.4)
setkey(data.dz.gh.4,门诊号,挂号时间)
hhh.id4=data.dz.gh.4[!duplicated(data.dz.gh.4$门诊号),"门诊号"]

  
for(i in 1:nrow(hhh.id4)){
  x=which(data.dz.gh.4[,"门诊号"]==as.character(hhh.id4[i,"门诊号"]))
  y=data.dz.gh.4[min(x),"挂号时间"]
  z=data.dz.gh.4[max(x),"挂号时间"]
  hhh.id4[i,"startday"]=y
  hhh.id4[i,"endday"]=z
}
data.dz.gh.4=left_join(data.dz.gh.4,hhh.id4,by="门诊号")
data.dz.gh.4=data.dz.gh.4%>%
  mutate(时间窗=difftime(data.dz.gh.4$挂号时间,data.dz.gh.4$startday,units = "days"),
            end_start=difftime(data.dz.gh.4$endday,data.dz.gh.4$startday,units = "days"))%>%
  mutate(end_start1=end_start/365)


xv.dz.case=data.dz.gh.4

xv.dz.case.id=xv.dz.case[!duplicated(xv.dz.case$门诊号),c("门诊号","startday","end_start1")]
xv.dz.case.id$oneyear=paste0(substr(xv.dz.case.id$startday,1,3),as.character(as.numeric(substr(xv.dz.case.id$startday,4,4))+1),substr(xv.dz.case.id$startday,5,19))
xv.dz.case.id$oneyear=ifelse(substr(xv.dz.case.id$oneyear,1,4)=='2001',paste0(substr(xv.dz.case.id$startday,1,2),10,substr(xv.dz.case.id$startday,5,19)),xv.dz.case.id$oneyear)
xv.dz.case.id$门诊次数=rep(0,nrow(xv.dz.case.id))
xv.dz.case.id$门诊心血管次数=rep(0,nrow(xv.dz.case.id))
xv.dz.case.id$门诊所有时间心血管次数=rep(0,nrow(xv.dz.case.id))
set.seed(123)
xv.dz.case.id.sample=xv.dz.case.id[sample(1:nrow(xv.dz.case.id),1000,replace = F),]
data.dz.gh.4.sample=left_join(xv.dz.case.id.sample,data.dz.gh.4%>%
                                select(-startday),by="门诊号")



for(j in 1:nrow(xv.dz.case.id.sample)){
  x=which((data.dz.gh.4.sample[,"门诊号"]==xv.dz.case.id.sample[j,"门诊号"])&(data.dz.gh.4.sample[,"挂号时间"]>=xv.dz.case.id.sample[j,"startday"])&(data.dz.gh.4.sample[,"挂号时间"]<=xv.dz.case.id.sample[j,"oneyear"]))
  y=length(x)
  xv.dz.case.id.sample[j,"门诊次数"]=y
}

for(j in 1:nrow(xv.dz.case.id.sample)){
  x=which((data.dz.gh.4.sample[,"门诊号"]==xv.dz.case.id.sample[j,"门诊号"])&(data.dz.gh.4.sample[,"挂号时间"]>=xv.dz.case.id.sample[j,"startday"])&(data.dz.gh.4.sample[,"挂号时间"]<=xv.dz.case.id.sample[j,"oneyear"])
          &(grepl("心",data.dz.gh.4.sample[,"诊断"])))
  y=length(x)
  xv.dz.case.id.sample[j,"门诊心血管次数"]=y
}


for(j in 1:nrow(xv.dz.case.id.sample)){
  x=which((data.dz.gh.4.sample[,"门诊号"]==xv.dz.case.id.sample[j,"门诊号"])&(grepl("心",data.dz.gh.4.sample[,"诊断"])))
  y=length(x)
  xv.dz.case.id.sample[j,"门诊所有时间心血管次数"]=y
}

#住院
data.dz.mz.zy1=data.dz.mz.zy%>%rename(门诊号=CARD_NO,住院流水号=INPATIENT_NO)
xv.dz.zy.case=data.dz.mz.zy1[,c("门诊号","住院流水号")]%>%inner_join(data.dz.zy,by="住院流水号")


xv.dz.case.id.sample1=xv.dz.case.id.sample
xv.dz.case.id.sample1$门诊号=as.character(as.numeric(xv.dz.case.id.sample1$门诊号))
xv.dz.zy.case1=xv.dz.zy.case%>%filter(诊断类型=="主要诊断")%>%left_join(xv.dz.case.id.sample1%>%
                                                                  select(1,2,4),by="门诊号")
sum(is.na(xv.dz.zy.case1$startday))
xv.dz.zy.case1=xv.dz.zy.case1[!is.na(xv.dz.zy.case1$oneyear),]
xv.dz.zy.case1$门诊号=as.character(xv.dz.zy.case1$门诊号)
sum(is.na(xv.dz.case.id.sample1$门诊号))
sum(is.na(xv.dz.zy.case1$门诊号))
xv.dz.case.id.sample1=xv.dz.case.id.sample1[!is.na(xv.dz.case.id.sample1$门诊号),]
xv.dz.zy.case1=xv.dz.zy.case1[!is.na(xv.dz.zy.case1$门诊号),]

#所有时间心血管次数
xv.dz.zy.case1.heart=xv.dz.zy.case1[c(grep("心",as.vector(as.matrix(as.data.frame(xv.dz.zy.case1[,"出院诊断"])))),
                                      grep("高血压",as.vector(as.matrix(as.data.frame(xv.dz.zy.case1[,"出院诊断"])))),
                                      grep("脑梗",as.vector(as.matrix(as.data.frame(xv.dz.zy.case1[,"出院诊断"])))),
                                      grep("动脉",as.vector(as.matrix(as.data.frame(xv.dz.zy.case1[,"出院诊断"]))))),]
xv.dz.zy.case1.heart=xv.dz.zy.case1.heart[!duplicated(xv.dz.zy.case1.heart$住院流水号),]

#一年住院次数
xv.dz.zy.case1.year=xv.dz.zy.case1%>%
  filter(入院日期>=startday&入院日期<=oneyear)

#一年心血管次数+住院天数
xv.dz.zy.case1.year.heart=xv.dz.zy.case1.year[c(grep("心",as.vector(as.matrix(as.data.frame(xv.dz.zy.case1.year[,"出院诊断"])))),
                                                grep("高血压",as.vector(as.matrix(as.data.frame(xv.dz.zy.case1.year[,"出院诊断"])))),
                                                grep("脑梗",as.vector(as.matrix(as.data.frame(xv.dz.zy.case1.year[,"出院诊断"])))),
                                                grep("动脉",as.vector(as.matrix(as.data.frame(xv.dz.zy.case1.year[,"出院诊断"]))))),]
xv.dz.zy.case1.year.heart=xv.dz.zy.case1.year.heart[!duplicated(xv.dz.zy.case1.year.heart$住院流水号),]

#所有时间

xv.dz.case.id.sample1$住院所有时间心血管次数=rep(0,nrow(xv.dz.case.id.sample1))

for (i in 1:nrow(xv.dz.case.id.sample1)) {
  x=which(xv.dz.zy.case1.heart[,"门诊号"]==xv.dz.case.id.sample1[i,"门诊号"])
  if(length(x)!=0){
    xv.dz.case.id.sample1[i,"住院所有时间心血管次数"]=length(x)
  }
}

#一年住院次数

xv.dz.case.id.sample1$住院次数=rep(0,nrow(xv.dz.case.id.sample1))
for (i in 1:nrow(xv.dz.case.id.sample1)) {
  x=which(xv.dz.zy.case1.year[,"门诊号"]==xv.dz.case.id.sample1[i,"门诊号"])
  if(length(x)!=0){
    xv.dz.case.id.sample1[i,"住院次数"]=length(x)
  }
}

#一年心血管次数+住院天数
xv.dz.case.id.sample1$住院心血管次数=rep(0,nrow(xv.dz.case.id.sample1))
xv.dz.case.id.sample1$住院天数=rep(0,nrow(xv.dz.case.id.sample1))
xv.dz.zy.case1.year.heart$住院天数=as.numeric(xv.dz.zy.case1.year.heart$住院天数)
for (i in 1:nrow(xv.dz.case.id.sample1)) {
  x=which(xv.dz.zy.case1.year.heart[,"门诊号"]==xv.dz.case.id.sample1[i,"门诊号"])
  if(length(x)!=0){
    xv.dz.case.id.sample1[i,"住院心血管次数"]=length(x)
    xv.dz.case.id.sample1[i,"住院天数"]=sum(xv.dz.zy.case1.year.heart[x,"住院天数"])
  }
}


#obj3

data.dz.2006.2017=rbind(data.dz.2006.2010,data.dz.2011,data.dz.2012,data.dz.2013,data.dz.2014,data.dz.2015,data.dz.2016,data.dz.2017)



xv.dz.case.id.sample1$门诊费用=rep(0,nrow(xv.dz.case.id.sample1))
xv.dz.case.id.sample1$住院费用=rep(0,nrow(xv.dz.case.id.sample1))
xv.dz.case.id.sample1$住院药品费用=rep(0,nrow(xv.dz.case.id.sample1))
xv.dz.case.id.sample1$住院非药品费用=rep(0,nrow(xv.dz.case.id.sample1))
data.dz.gh.num=data.dz.gh
data.dz.gh.num$门诊号=as.character(as.numeric(data.dz.gh.num$门诊号))
sum(is.na(data.dz.gh.num$门诊号))
xv.dz.case.id.sample1.lsh=left_join(xv.dz.case.id.sample1,data.dz.gh.num,by="门诊号")%>%filter(挂号时间>=startday&挂号时间<=oneyear)
data.dz.mz.id=xv.dz.case.id.sample1.lsh[,c("门诊号","就诊流水号")]
data.dz.mz.fy=left_join(data.dz.mz.id,data.dz.2006.2017,by="就诊流水号")
data.dz.mz.fy$合计金额[is.na(data.dz.mz.fy$合计金额)]<-0
data.dz.mz.fy$合计金额=as.numeric(data.dz.mz.fy$合计金额)

for (i in 1:nrow(xv.dz.case.id.sample1)) {
  x=which(data.dz.mz.fy[,"门诊号"]==as.character(xv.dz.case.id.sample1[i,"门诊号"]))
  xv.dz.case.id.sample1[i,"门诊费用"]=sum(data.dz.mz.fy[x,"合计金额"])
}

#心血管资源门诊费用
data.dz.gh.4.sample.heart=data.dz.gh.4.sample%>%
  filter(挂号时间>=startday&挂号时间<=oneyear)


data.dz.gh.4.sample.heart1=data.dz.gh.4.sample.heart[c(grep("心",as.vector(as.matrix(as.data.frame(data.dz.gh.4.sample.heart[,"诊断"])))),
                                                       grep("高血压",as.vector(as.matrix(as.data.frame(data.dz.gh.4.sample.heart[,"诊断"])))),
                                                       grep("脑梗",as.vector(as.matrix(as.data.frame(data.dz.gh.4.sample.heart[,"诊断"])))),
                                                       grep("动脉",as.vector(as.matrix(as.data.frame(data.dz.gh.4.sample.heart[,"诊断"]))))),]

data.dz.gh.4.sample.heart1=data.dz.gh.4.sample.heart1[!duplicated(data.dz.gh.4.sample.heart1$就诊流水号),]

data.dz.mz.fy1=data.dz.gh.4.sample.heart1%>%
  select(13)%>%
  left_join(data.dz.mz.fy,by="就诊流水号")

sum(!duplicated(data.dz.mz.fy1$就诊流水号))
sum(!duplicated(data.dz.mz.fy1$门诊号))

xv.dz.case.id.sample1$心血管资源门诊费用=rep(0,nrow(xv.dz.case.id.sample1))
for(i in 1:nrow(xv.dz.case.id.sample1)){
  x=which(data.dz.mz.fy1[,"门诊号"]==as.character(xv.dz.case.id.sample1[i,"门诊号"]))
  xv.dz.case.id.sample1[i,"心血管资源门诊费用"]=sum(data.dz.mz.fy1[x,"合计金额"])
}



#住院
data.dz.mz.zy1$门诊号=as.character(data.dz.mz.zy1$门诊号)
data.dz.mz.zy1=data.dz.mz.zy1[!is.na(data.dz.mz.zy1$门诊号),]
xv.dz.zy.lsh=left_join(xv.dz.case.id.sample1,data.dz.mz.zy1,by="门诊号")%>%
  inner_join(data.dz.fyp%>%
               rename(住院流水号=住院号),by="住院流水号")%>%
  rename(非药品金额=金额)%>%
  filter(收费时间>=startday&收费时间<=oneyear)

xv.dz.zy.lsh1=left_join(xv.dz.case.id.sample1,data.dz.mz.zy1,by="门诊号")%>%
  inner_join(data.dz.yp,by="住院流水号")%>%
  rename(药品金额=总金额)%>%
  filter(收费日期>=startday&收费日期<=oneyear)

xv.dz.zy.lsh1$药品金额=as.numeric(xv.dz.zy.lsh1$药品金额)
for (i in 1:nrow(xv.dz.case.id.sample1)) {
  x=which(xv.dz.zy.lsh1[,"门诊号"]==as.character(xv.dz.case.id.sample1[i,"门诊号"]))
  xv.dz.case.id.sample1[i,"住院药品费用"]=sum(xv.dz.zy.lsh1[x,"药品金额"])
}
xv.dz.zy.lsh$非药品金额=as.numeric(xv.dz.zy.lsh$非药品金额)
for (i in 1:nrow(xv.dz.case.id.sample1)) {
  x=which(xv.dz.zy.lsh[,"门诊号"]==as.character(xv.dz.case.id.sample1[i,"门诊号"]))
  xv.dz.case.id.sample1[i,"住院非药品费用"]=sum(xv.dz.zy.lsh[x,"非药品金额"])
}


xv.dz.case.id.sample1$住院费用=xv.dz.case.id.sample1$住院药品费用+xv.dz.case.id.sample1$住院非药品费用

#心血管资源住院费用

xv.dz.zy.lsh.heart=xv.dz.zy.case1.year.heart%>%
  select(2,3)%>%
  left_join(xv.dz.zy.lsh,by="住院流水号")
sum(is.na(xv.dz.zy.lsh.heart$非药品金额))

xv.dz.zy.lsh1.heart=xv.dz.zy.case1.year.heart%>%
  select(住院流水号)%>%
  left_join(xv.dz.zy.lsh1,by="住院流水号")
sum(is.na(xv.dz.zy.lsh1.heart$药品金额))

xv.dz.case.id.sample1$心血管资源住院药品费用=rep(0,nrow(xv.dz.case.id.sample1))

for(i in 1:nrow(xv.dz.case.id.sample1)){
  x=which(xv.dz.zy.lsh1.heart[,"门诊号"]==as.character(xv.dz.case.id.sample1[i,"门诊号"]))
  if(length(x)!=0){
    xv.dz.case.id.sample1[i,"心血管资源住院药品费用"]=sum(xv.dz.zy.lsh1.heart[x,"药品金额"])
  }
}

xv.dz.case.id.sample1$心血管资源非药品费用=rep(0,nrow(xv.dz.case.id.sample1))
xv.dz.case.id.sample1$心血管资源非药品费用=as.numeric(xv.dz.case.id.sample1$心血管资源非药品费用)
for (i in 1:nrow(xv.dz.case.id.sample1)) {
  x=which(xv.dz.zy.lsh.heart[,"门诊号"]==as.character(xv.dz.case.id.sample1[i,"门诊号"]))
  if(length(x)!=0){
    xv.dz.case.id.sample1[i,"心血管资源非药品费用"]=sum(xv.dz.zy.lsh.heart[x,"非药品金额"])
  }
}

xv.dz.case.id.sample1$心血管资源住院费用=xv.dz.case.id.sample1$心血管资源住院药品费用+xv.dz.case.id.sample1$心血管资源非药品费用



#obj1对照基线

xv.mz.case.id1=data.dz.gh[!duplicated(data.dz.gh$门诊号),c("门诊号","出生日期","性别","保险类别","科室名称")]
xv.mz.case.id1$age=2017-as.numeric(substr(xv.mz.case.id1$出生日期,1,4))
xv.mz.case.id1$门诊号=as.character(as.numeric(xv.mz.case.id1$门诊号))
xv.mz.case.id2=left_join(xv.dz.case.id.sample1,xv.mz.case.id1[,-2],by="门诊号")
write.table(xv.mz.case.id2,file = "h:/data/gaoyue1221合并/0110newcode/中山对照组.csv",sep=",",row.names=F)






