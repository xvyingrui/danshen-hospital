
library(data.table)
library(tidyverse)
library(readxl)


#华山医院对照组....................................................................................................

set.seed(108)
table1.mz<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table1_mz.csv",header=T)
table1.zy<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table1_zy.csv",header=T)
table2.dzmzhz<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_对照门诊汇总_new.csv",header=T)
table2.dzmzmx<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_对照门诊明细_new.csv",header=T)
table2.dzzyhz<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_对照住院汇总_new.csv",header=T)
table2.dzzymx<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_对照住院明细_new.csv",header=T)
table2.symzhz<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_实验门诊汇总_new.csv",header=T)
table2.symzmx<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_实验门诊明细_new.csv",header=T)
table2.syzyhz<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_实验住院汇总_new.csv",header=T)
table2.syzymx<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table2_实验住院明细_new.csv",header=T)
table3.dzzmzsj<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table3_对照组门诊数据.csv",header=T)
table3.dzzzyhz<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table3_对照组住院汇总.csv",header=T)
table3.dzzzysj<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table3_对照组住院数据.csv",header=T)
table3.syzmzsj<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table3_实验组门诊数据.csv",header=T)
table3.syzzyhz<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table3_实验组住院汇总.csv",header=T)
table3.syzzysj<-read.csv("h:/data/gaoyue1/绿谷project/课题数据/原始数据/table3_实验组住院数据.csv",header=T)




table2.symzhz1=data.table(table2.dzmzhz)
table2.symzhz1=table2.symzhz1%>%rename(门诊号=MAX.E.JZBM.,挂号时间=MAX.KFRQ.)
setkey(table2.symzhz1,门诊号,挂号时间)
hhh.id1=table2.symzhz1[!duplicated(table2.symzhz1$门诊号),"门诊号"]
for(i in 1:nrow(hhh.id1)){
  x=which(table2.symzhz1[,"门诊号"]==as.numeric(hhh.id1[i,"门诊号"]))
  y=table2.symzhz1[min(x),"挂号时间"]
  z=table2.symzhz1[max(x),"挂号时间"]
  hhh.id1[i,"startday"]=y
  hhh.id1[i,"endday"]=z
}

table2.symzhz1=left_join(table2.symzhz1,hhh.id1,by="门诊号")
table2.symzhz1=table2.symzhz1%>%
  mutate(时间窗=difftime(table2.symzhz1$挂号时间,table2.symzhz1$startday,units = "days"),
            end_start=difftime(table2.symzhz1$endday,table2.symzhz1$startday,units = "days"))%>%
  mutate(end_start1=end_start/365)




table2.dzzyhz$住院天数=difftime(as.Date(table2.dzzyhz$OUTDATE),as.Date(table2.dzzyhz$ENTDATE),units = "days")


#读入筛选好的ID
dz.zyts=read_excel("h:/data/gaoyue1221合并/华山中山合并原始统计结果/华山对照/华山obj2对照组.xlsx")
dz.zyts$门诊号=as.character(dz.zyts$门诊号)
dz.zyts$startday=as.character(as.Date(dz.zyts$startday))
dz.zyts$oneyear=paste0(substr(dz.zyts$startday,1,3),as.character(as.numeric(substr(dz.zyts$startday,4,4))+1),substr(dz.zyts$startday,5,10))


dz.zyts1=dz.zyts%>%
  left_join(table2.dzmzhz%>%rename(门诊号=MAX.E.JZBM.),by="门诊号")%>%
  rename(ID=MAX.E.KHMZH.)%>%
  select(门诊号,startday,oneyear,ID)%>%
  group_by(ID,门诊号)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  left_join(table2.dzzyhz%>%rename(ID=MEDCARD),by="ID")%>%
  filter(ENTDATE!="NA")

dz.zyts1$startday=as.Date(as.character(dz.zyts1$startday))
dz.zyts1$oneyear=as.Date(as.character(dz.zyts1$oneyear))
dz.zyts1$ENTDATE=as.Date(dz.zyts1$ENTDATE)
dz.zyts1.1=dz.zyts1%>%
  filter(ENTDATE>=startday&ENTDATE<=oneyear)
dz.zyts2=dz.zyts1.1[c(grep("心",as.vector(as.matrix(as.data.frame(dz.zyts1.1[,"ZDMC"])))),
                      grep("高血压",as.vector(as.matrix(as.data.frame(dz.zyts1.1[,"ZDMC"])))),
                      grep("脑梗",as.vector(as.matrix(as.data.frame(dz.zyts1.1[,"ZDMC"])))),
                      grep("动脉",as.vector(as.matrix(as.data.frame(dz.zyts1.1[,"ZDMC"]))))),]


dz.zyts2=dz.zyts2[!duplicated(dz.zyts2$CURENO),]

dz.zyts2.1=dz.zyts1[c(grep("心",as.vector(as.matrix(as.data.frame(dz.zyts1[,"ZDMC"])))),
                      grep("高血压",as.vector(as.matrix(as.data.frame(dz.zyts1[,"ZDMC"])))),
                      grep("脑梗",as.vector(as.matrix(as.data.frame(dz.zyts1[,"ZDMC"])))),
                      grep("动脉",as.vector(as.matrix(as.data.frame(dz.zyts1[,"ZDMC"]))))),]


dz.zyts2.1=dz.zyts2.1[!duplicated(dz.zyts2.1$CURENO),]




#dz.zyts2.1       住院所有心血管次数
#dz.zyts1.1     住院次数
#dz.zyts       id
#dz.zyts2     住院天数、住院心血管次数

#住院所有心血管次数
dz.zyts$住院所有心血管次数=rep(0,nrow(dz.zyts))
for(i in 1:nrow(dz.zyts)){
  x=which(dz.zyts2.1[,"门诊号"]==as.character(dz.zyts[i,"门诊号"]))
  if(length(x)!=0){
    dz.zyts[i,"住院所有心血管次数"]=length(x)
  }
}


#住院次数
for(i in 1:nrow(dz.zyts)){
  for (j in 1:nrow(dz.zyts1.1)) {
    if(dz.zyts[i,"门诊号"]==as.character(dz.zyts1.1[j,"门诊号"])){
      dz.zyts[i,"住院次数"]=as.numeric(dz.zyts[i,"住院次数"])+1 
    }
  }
}
#住院天数
dz.zyts$住院天数=rep(0,nrow(dz.zyts))

for (i in 1:nrow(dz.zyts)) {
  for(j in 1:nrow(dz.zyts2)){
    if(dz.zyts[i,"门诊号"]==as.character(dz.zyts2[j,"门诊号"]))
      dz.zyts[i,"住院天数"]<-as.numeric(dz.zyts[i,"住院天数"])+as.numeric(dz.zyts2[j,"住院天数"])
  }
}
#住院心血管次数
for(i in 1:nrow(dz.zyts)){
  for(j in 1:nrow(dz.zyts2)){
    if(dz.zyts[i,"门诊号"]==as.character(dz.zyts2[j,"门诊号"])){
      dz.zyts[i,"住院心血管次数"]=as.numeric(dz.zyts[i,"住院心血管次数"])+1
    }
  }
}



#门诊次数1
#门诊心血管次数1

dz.zyts$门诊次数1=rep(0,nrow(dz.zyts))
dz.zyts$门诊心血管次数1=rep(0,nrow(dz.zyts))


dz.mzts=dz.zyts%>%
  left_join(table2.dzmzhz%>%rename(门诊号=MAX.E.JZBM.),by="门诊号")
sum(duplicated(dz.mzts$MZLSH))
dz.mzts$MAX.KFRQ.=as.character(dz.mzts$MAX.KFRQ.)
dz.mzts$startday=as.character(dz.mzts$startday)
dz.mzts$oneyear=as.character(dz.mzts$oneyear)
dz.mzts1=dz.mzts%>%
  filter(MAX.KFRQ.>=startday&MAX.KFRQ.<=oneyear)

dz.mzts2=dz.mzts1[c(grep("心",as.vector(as.matrix(as.data.frame(dz.mzts1[,"MAX.ICDDESC."])))),
                    grep("高血压",as.vector(as.matrix(as.data.frame(dz.mzts1[,"MAX.ICDDESC."])))),
                    grep("脑梗",as.vector(as.matrix(as.data.frame(dz.mzts1[,"MAX.ICDDESC."])))),
                    grep("动脉",as.vector(as.matrix(as.data.frame(dz.mzts1[,"MAX.ICDDESC."]))))),]
dz.mzts2=dz.mzts2[!duplicated(dz.mzts2$MZLSH),]

dz.mzts2.1=dz.mzts[c(grep("心",as.vector(as.matrix(as.data.frame(dz.mzts[,"MAX.ICDDESC."])))),
                     grep("高血压",as.vector(as.matrix(as.data.frame(dz.mzts[,"MAX.ICDDESC."])))),
                     grep("脑梗",as.vector(as.matrix(as.data.frame(dz.mzts[,"MAX.ICDDESC."])))),
                     grep("动脉",as.vector(as.matrix(as.data.frame(dz.mzts[,"MAX.ICDDESC."]))))),]
dz.mzts2.1=dz.mzts2.1[!duplicated(dz.mzts2.1$MZLSH),]



#门诊次数1+门诊心血管次数1

for(i in 1:nrow(dz.zyts)){
  x=which(dz.mzts1[,"门诊号"]==as.character(dz.zyts[i,"门诊号"]))
  dz.zyts[i,"门诊次数1"]=length(x)
  dz.zyts[i,"门诊心血管次数1"]=length(x)
}

#门诊所有心血管次数
dz.zyts$门诊所有心血管次数=rep(0,nrow(dz.zyts))
for(i in 1:nrow(dz.zyts)){
  x=which(dz.mzts2.1[,"门诊号"]==as.character(dz.zyts[i,"门诊号"]))
  if(length(x)!=0){
    dz.zyts[i,"门诊所有心血管次数"]=length(x)
  }
}

#加上总时间     table2.symzhz1
#计算发生频率
dz.zyts=dz.zyts%>%
  left_join(table2.symzhz1%>%
              select(门诊号,end_start1)%>%
              group_by(门诊号)%>%
              filter(row_number()==1)%>%
              ungroup(),by="门诊号")
dz.zyts$end_start1=ifelse(dz.zyts$end_start1<1,1,dz.zyts$end_start1)
dz.zyts=dz.zyts%>%
  mutate(门诊发生频率=门诊所有心血管次数/end_start1,总发生频率=(门诊所有心血管次数+住院所有心血管次数)/end_start1)


#obj3费用信息
table2.dzmzhz$MAX.E.KHMZH.=as.character(table2.dzmzhz$MAX.E.KHMZH.)
table2.dzzyhz$MEDCARD=as.character(table2.dzzyhz$MEDCARD)
hs.id.dz1=dz.zyts%>%
  left_join(table2.dzmzhz%>%rename(门诊号=MAX.E.JZBM.,MEDCARD=MAX.E.KHMZH.),by="门诊号")%>%
  left_join(table2.dzzyhz,by="MEDCARD")%>%
  group_by(门诊号,CURENO)%>%
  filter(row_number()==1)%>%
  ungroup()
hs.id.dz1=hs.id.dz1[,c(1:8,11:17,19,20,22,26)]
hs.id.dz1[is.na(hs.id.dz1)]<-0
#每次的费用
hs.id.dz2=hs.id.dz1[,c("MEDCARD","CURENO","startday","oneyear")]
hs.id.dz2[is.na(hs.id.dz2)]<-0
#每人的费用
hs.id.dz3=hs.id.dz1[!duplicated(hs.id.dz1$MEDCARD),]
hs.id.dz3[is.na(hs.id.dz3)]<-0

#每次
#华山对照组随访一年住院费用1
table3.dzzzysj1=left_join(hs.id.dz2,table2.dzzymx,by="CURENO")
table3.dzzzysj1=na.omit(table3.dzzzysj1)
table3.dzzzysj1$startday=as.Date(as.character(table3.dzzzysj1$startday))
table3.dzzzysj1$oneyear=as.Date(as.character(table3.dzzzysj1$oneyear))
table3.dzzzysj1$STARTTIME=as.Date(as.character(table3.dzzzysj1$STARTTIME))
table3.dzzzysj1=table3.dzzzysj1%>%
  filter(STARTTIME>=startday&STARTTIME<=oneyear)

hs.id.dz2$住院费用1=rep(0,nrow(hs.id.dz2))

for (i in 1:nrow(hs.id.dz2)) {
  x=which(as.data.frame(table3.dzzzysj1[,"CURENO"])==as.character(hs.id.dz2[i,"CURENO"]))
  if(length(x)!=0){
    hs.id.dz2[i,"住院费用1"]=sum(table3.dzzzysj1[x,"PRICE"])
  }
}
#每次
#华山对照组随访一年住院费用2
table3.dzzzysj2=hs.id.dz2%>%
  left_join(table3.dzzzysj,by="CURENO")
table3.dzzzysj2=na.omit(table3.dzzzysj2)
table3.dzzzysj2$startday=as.Date(as.character(table3.dzzzysj2$startday))
table3.dzzzysj2$oneyear=as.Date(as.character(table3.dzzzysj2$oneyear))
table3.dzzzysj2$LASTCREDITTIME=as.Date(as.character(table3.dzzzysj2$LASTCREDITTIME))
table3.dzzzysj2=table3.dzzzysj2%>%
  filter(LASTCREDITTIME>=startday&LASTCREDITTIME<=oneyear)

hs.id.dz2$住院费用2=rep(0,nrow(hs.id.dz2))

for (i in 1:nrow(hs.id.dz2)) {
  x=which(as.data.frame(table3.dzzzysj2[,"CURENO"])==as.character(hs.id.dz2[i,"CURENO"]))
  if(length(x)!=0){
    hs.id.dz2[i,"住院费用2"]=sum(table3.dzzzysj2[x,"TOTAL"])
  }
}

hs.id.dz2$总住院费用=hs.id.dz2$住院费用1+hs.id.dz2$住院费用2
#每人
hs.id.dz3$住院费用=rep(0,nrow(hs.id.dz3))
for(i in 1:nrow(hs.id.dz3)){
  x=which(hs.id.dz2[,"MEDCARD"]==as.character(hs.id.dz3[i,"MEDCARD"]))
  if(length(x)!=0){
    hs.id.dz3[i,"住院费用"]=sum(hs.id.dz2[x,"总住院费用"])
  }
}

#心血管资源住院费用

hs.id.dz.heart=dz.zyts%>%
  left_join(table2.dzmzhz%>%rename(门诊号=MAX.E.JZBM.,MEDCARD=MAX.E.KHMZH.),by="门诊号")%>%
  left_join(table2.dzzyhz,by="MEDCARD")%>%
  group_by(门诊号,CURENO)%>%
  filter(row_number()==1)%>%
  ungroup()

hs.id.dz.heart1=hs.id.dz.heart[c(grep("心",as.vector(as.matrix(as.data.frame(hs.id.dz.heart[,"ZDMC"])))),
                                 grep("高血压",as.vector(as.matrix(as.data.frame(hs.id.dz.heart[,"ZDMC"])))),
                                 grep("脑梗",as.vector(as.matrix(as.data.frame(hs.id.dz.heart[,"ZDMC"])))),
                                 grep("动脉",as.vector(as.matrix(as.data.frame(hs.id.dz.heart[,"ZDMC"]))))),]
hs.id.dz.heart1=hs.id.dz.heart1[!duplicated(hs.id.dz.heart1$CURENO),]
sum(is.na(hs.id.dz.heart1$CURENO))

hs.id.dz.heart2=hs.id.dz.heart1[,c("MEDCARD","CURENO","startday","oneyear")]

#每次心血管资源住院费用
#华山对照组随访一年住院费用1
table3.dzzzysj1=left_join(hs.id.dz.heart2,table2.dzzymx,by="CURENO")
table3.dzzzysj1=na.omit(table3.dzzzysj1)
table3.dzzzysj1$startday=as.Date(as.character(table3.dzzzysj1$startday))
table3.dzzzysj1$oneyear=as.Date(as.character(table3.dzzzysj1$oneyear))
table3.dzzzysj1$STARTTIME=as.Date(as.character(table3.dzzzysj1$STARTTIME))
table3.dzzzysj1=table3.dzzzysj1%>%
  filter(STARTTIME>=startday&STARTTIME<=oneyear)

hs.id.dz.heart2$住院费用1=rep(0,nrow(hs.id.dz.heart2))

for (i in 1:nrow(hs.id.dz.heart2)) {
  x=which(as.data.frame(table3.dzzzysj1[,"CURENO"])==as.character(hs.id.dz.heart2[i,"CURENO"]))
  if(length(x)!=0){
    hs.id.dz.heart2[i,"住院费用1"]=sum(table3.dzzzysj1[x,"PRICE"])
  }
}
#每次
#华山对照组随访一年住院费用2
table3.dzzzysj2=hs.id.dz.heart2%>%
  left_join(table3.dzzzysj,by="CURENO")
table3.dzzzysj2=na.omit(table3.dzzzysj2)
table3.dzzzysj2$startday=as.Date(as.character(table3.dzzzysj2$startday))
table3.dzzzysj2$oneyear=as.Date(as.character(table3.dzzzysj2$oneyear))
table3.dzzzysj2$LASTCREDITTIME=as.Date(as.character(table3.dzzzysj2$LASTCREDITTIME))
table3.dzzzysj2=table3.dzzzysj2%>%
  filter(LASTCREDITTIME>=startday&LASTCREDITTIME<=oneyear)

hs.id.dz.heart2$住院费用2=rep(0,nrow(hs.id.dz.heart2))

for (i in 1:nrow(hs.id.dz.heart2)) {
  x=which(as.data.frame(table3.dzzzysj2[,"CURENO"])==as.character(hs.id.dz.heart2[i,"CURENO"]))
  if(length(x)!=0){
    hs.id.dz.heart2[i,"住院费用2"]=sum(table3.dzzzysj2[x,"TOTAL"])
  }
}

hs.id.dz.heart2$总住院费用=hs.id.dz.heart2$住院费用1+hs.id.dz.heart2$住院费用2
#
#每人心血管资源住院费用
hs.id.dz3$心血管资源住院费用=rep(0,nrow(hs.id.dz3))
for(i in 1:nrow(hs.id.dz3)){
  x=which(hs.id.dz.heart2[,"MEDCARD"]==as.character(hs.id.dz3[i,"MEDCARD"]))
  if(length(x)!=0){
    hs.id.dz3[i,"心血管资源住院费用"]=sum(hs.id.dz.heart2[x,"总住院费用"])
  }
}

#门诊花费
#随访一年门诊费用1
table2.dzmzmx$JZBM=as.character(table2.dzmzmx$JZBM)
table2.dzmzmx1=hs.id.dz3%>%
  left_join(table2.dzmzmx%>%rename(门诊号=JZBM),by="门诊号")
table2.dzmzmx1$startday=as.character(table2.dzmzmx1$startday)
table2.dzmzmx1$oneyear=as.character(table2.dzmzmx1$oneyear)
table2.dzmzmx1$KFRQ=as.character(table2.dzmzmx1$KFRQ)
table2.dzmzmx1=table2.dzmzmx1%>%
  filter(KFRQ>=startday&KFRQ<=oneyear)

for (i in 1:nrow(hs.id.dz3)) {
  x=which(as.data.frame(table2.dzmzmx1[,1])==as.character(hs.id.dz3[i,1]))
  if(length(x)!=0){
    hs.id.dz3[i,"门诊费用1"]=sum(table2.dzmzmx1[x,"YPFY"])
  }
}


#随访一年门诊费用2
table3.dzzmzsj$JZBM=as.character(table3.dzzmzsj$JZBM)
table3.dzzmzsj1=hs.id.dz3%>%
  left_join(table3.dzzmzsj%>%rename(门诊号=JZBM),by="门诊号")
table3.dzzmzsj1$startday=as.character(table3.dzzmzsj1$startday)
table3.dzzmzsj1$oneyear=as.character(table3.dzzmzsj1$oneyear)
table3.dzzmzsj1$CZRQ=as.character(table3.dzzmzsj1$CZRQ)
table3.dzzmzsj1=table3.dzzmzsj1%>%
  filter(CZRQ>=startday&CZRQ<=oneyear)

for (i in 1:nrow(hs.id.dz3)) {
  x=which(as.data.frame(table3.dzzmzsj1[,1])==as.character(hs.id.dz3[i,1]))
  if(length(x)!=0){
    hs.id.dz3[i,"门诊费用2"]=sum(table3.dzzmzsj1[x,"YFJE"])
  }
}

hs.id.dz3$门诊总费用=hs.id.dz3$门诊费用1+hs.id.dz3$门诊费用2
hs.id.dz3$门诊加住院总费用=hs.id.dz3$门诊总费用+hs.id.dz3$住院费用


#心血管资源门诊费用
dz.mz.heart=dz.zyts%>%
  left_join(table2.dzmzhz%>%rename(门诊号=MAX.E.JZBM.),by="门诊号")
sum(duplicated(dz.mz.heart$MZLSH))
dz.mz.heart$MAX.KFRQ.=as.character(dz.mz.heart$MAX.KFRQ.)
dz.mz.heart$startday=as.character(dz.mz.heart$startday)
dz.mz.heart$oneyear=as.character(dz.mz.heart$oneyear)
dz.mz.heart1=dz.mz.heart%>%
  filter(MAX.KFRQ.>=startday&MAX.KFRQ.<=oneyear)

dz.mz.heart2=dz.mz.heart1[c(grep("心",as.vector(as.matrix(as.data.frame(dz.mz.heart1[,"MAX.ICDDESC."])))),
                            grep("高血压",as.vector(as.matrix(as.data.frame(dz.mz.heart1[,"MAX.ICDDESC."])))),
                            grep("脑梗",as.vector(as.matrix(as.data.frame(dz.mz.heart1[,"MAX.ICDDESC."])))),
                            grep("动脉",as.vector(as.matrix(as.data.frame(dz.mz.heart1[,"MAX.ICDDESC."]))))),]
dz.mz.heart2=dz.mz.heart2[!duplicated(dz.mz.heart2$MZLSH),]
dz.mz.heart2=dz.mz.heart2[,c("门诊号","startday","oneyear","MZLSH")]

#每次门诊费用1

dz.mz.heart2$门诊费用1=rep(0,nrow(dz.mz.heart2))

for (i in 1:nrow(dz.mz.heart2)) {
  x=which(as.data.frame(table3.dzzmzsj[,"MZLSH"])==as.character(dz.mz.heart2[i,"MZLSH"]))
  if(length(x)!=0){
    dz.mz.heart2[i,"门诊费用1"]=sum(table3.dzzmzsj[x,"YFJE"])
  }
}
#每次门诊费用2
dz.mz.heart2$门诊费用2=rep(0,nrow(dz.mz.heart2))

for (i in 1:nrow(dz.mz.heart2)) {
  x=which(as.data.frame(table2.dzmzmx[,"MZLSH"])==as.character(dz.mz.heart2[i,"MZLSH"]))
  if(length(x)!=0){
    dz.mz.heart2[i,"门诊费用2"]=sum(table2.dzmzmx[x,"YPFY"])
  }
}

dz.mz.heart2$总门诊费用=dz.mz.heart2$门诊费用1+dz.mz.heart2$门诊费用2

#
#每人心血管资源门诊费用
hs.id.dz3$心血管资源门诊费用=rep(0,nrow(hs.id.dz3))
for(i in 1:nrow(hs.id.dz3)){
  x=which(dz.mz.heart2[,"门诊号"]==as.character(hs.id.dz3[i,"门诊号"]))
  if(length(x)!=0){
    hs.id.dz3[i,"心血管资源门诊费用"]=sum(dz.mz.heart2[x,"总门诊费用"])
  }
}

hs.id.dz3$心血管资源门诊加住院总费用=hs.id.dz3$心血管资源住院费用+hs.id.dz3$心血管资源门诊费用

#hs.id.dz3  为华山对照组统计结果0204
write.table(hs.id.dz3,file = "华山对照组0205.csv",sep=',',row.names=F)

#############........................................................................................................................


##华山实验组..........................................................................................................
#obj2
table2.symz.case<-table2.symzmx[grepl("注射用丹参多酚酸盐",table2.symzmx[,5]),]
table2.symz.case$time<-substr(table2.symz.case[,13],1,10)
table2.symz.case1=table2.symzhz[,c("MAX.E.KHMZH.","MAX.E.JZBM.")]%>%
  rename(MEDCARD=MAX.E.KHMZH.,JZBM=MAX.E.JZBM.)%>%
  inner_join(table2.symz.case,by="JZBM")
table2.syzy.case<-table2.syzymx[grepl("注射用丹参多酚酸盐",table2.syzymx[,7]),]
table2.syzy.case$STARTTIME=as.Date(as.character(table2.syzy.case$STARTTIME))
table2.syzy.case$time<-substr(table2.syzy.case[,2],1,10)
table2.syzy.case1=table2.syzyhz[,c("MEDCARD","CURENO")]%>%
  inner_join(table2.syzy.case,by="CURENO")

table2.case.id1=table2.symz.case1[,c("MEDCARD","time")]
table2.case.id2=table2.syzy.case1[,c("MEDCARD","time")]
table2.case.id3=rbind(table2.case.id1,table2.case.id2)%>%
  rename(ID=MEDCARD,startdate=time)





table.mz.case.sum1=data.table(table2.case.id3)
setkey(table.mz.case.sum1,ID,startdate)
hhh.id1=table.mz.case.sum1[!duplicated(table.mz.case.sum1$ID),"ID"]
table.mz.case.sum1$ID=as.character(table.mz.case.sum1$ID)
hhh.id1$ID=as.character(hhh.id1$ID)
for(i in 1:nrow(hhh.id1)){
  x=which(table.mz.case.sum1[,"ID"]==as.character(hhh.id1[i,"ID"]))
  y=table.mz.case.sum1[min(x),"startdate"]
  z=table.mz.case.sum1[max(x),"startdate"]
  hhh.id1[i,"startday"]=y
  hhh.id1[i,"endday"]=z
}

table.mz.case.sum2=left_join(table.mz.case.sum1,hhh.id1,by="ID")
table.mz.case.sum2=table.mz.case.sum2%>%
  mutate(时间窗=difftime(table.mz.case.sum2$startdate,table.mz.case.sum2$startday,units = "days"),
            end_start=difftime(table.mz.case.sum2$endday,table.mz.case.sum2$startday,units = "days"))%>%
  mutate(end_start1=end_start/365)

table2.case.id<-table.mz.case.sum2[!duplicated(table.mz.case.sum2$ID),c("ID","startday","end_start1")]

table2.symzhz$time<-substr(table2.symzhz[,7],1,10)
table2.case.id$oneyear<-paste0(substr(table2.case.id[,2],1,3),as.character(as.numeric(substr(table2.case.id[,2],4,4))+1),substr(table2.case.id[,2],5,19))

table2.case.id$门诊次数=rep(0,nrow(table2.case.id))
table2.case.id$门诊心血管次数=rep(0,nrow(table2.case.id))
table2.case.id$门诊所有心血管次数=rep(0,nrow(table2.case.id))

table2.symzhz1=table2.symzhz%>%
  rename(ID=MAX.E.KHMZH.)%>%
  left_join(table2.case.id,by="ID")          #所有时间的门诊就诊记录
table2.symzhz2=table2.symzhz1%>%
  filter(time>=startday&time<=oneyear)            #一年的门诊就诊记录

table2.symzhz3=table2.symzhz2[c(grep("心",as.vector(as.matrix(as.data.frame(table2.symzhz2[,"MAX.ICDDESC."]))))),]  #一年心血管
sum(duplicated(table2.symzhz3$MZLSH))
table2.symzhz4=table2.symzhz1[c(grep("心",as.vector(as.matrix(as.data.frame(table2.symzhz1[,"MAX.ICDDESC."]))))),]  #所有心血管
sum(duplicated(table2.symzhz4$MZLSH))

#门诊次数（一年）
table2.case.id$门诊次数=rep(0,nrow(table2.case.id))
for (i in 1:nrow(table2.case.id)) {
  x=which(table2.symzhz2[,"ID"]==table2.case.id[i,"ID"])
  if(length(x)!=0){
    table2.case.id[i,"门诊次数"]=length(x)
  }
}

#门诊心血管次数（一年）
table2.case.id$门诊心血管次数=rep(0,nrow(table2.case.id))
for(i in 1:nrow(table2.case.id)){
  x=which(table2.symzhz3[,"ID"]==table2.case.id[i,"ID"])
  if(length(x)!=0){
    table2.case.id[i,"门诊心血管次数"]=length(x)
  }
}

#门诊所有心血管次数（所有时间）
table2.case.id$门诊所有心血管次数=rep(0,nrow(table2.case.id))
for(i in 1:nrow(table2.case.id)){
  x=which(table2.symzhz4[,"ID"]==table2.case.id[i,"ID"])
  if(length(x)!=0){
    table2.case.id[i,"门诊所有心血管次数"]=length(x)
  }
}


#住院次数
table2.syzyhz$MEDCARD=as.character(table2.syzyhz$MEDCARD)
table2.syzyhz1=table2.case.id%>%
  left_join(table2.syzyhz%>%rename(ID=MEDCARD),by="ID")
table2.syzyhz1=na.omit(table2.syzyhz1)                #所有时间住院就诊记录
table2.syzyhz2=table2.syzyhz1%>%
  filter(time>=startday&time<=oneyear)                #随访一年住院就诊记录

table2.syzyhz3=table2.syzyhz1[c(grep("心",as.vector(as.matrix(as.data.frame(table2.syzyhz1[,"ZDMC"])))),
                                grep("高血压",as.vector(as.matrix(as.data.frame(table2.syzyhz1[,"ZDMC"])))),
                                grep("脑梗",as.vector(as.matrix(as.data.frame(table2.syzyhz1[,"ZDMC"])))),
                                grep("动脉",as.vector(as.matrix(as.data.frame(table2.syzyhz1[,"ZDMC"]))))),]


table2.syzyhz3=table2.syzyhz3[!duplicated(table2.syzyhz3$CURENO),]      #住院所有心血管次数

table2.syzyhz4=table2.syzyhz2[c(grep("心",as.vector(as.matrix(as.data.frame(table2.syzyhz2[,"ZDMC"])))),
                                grep("高血压",as.vector(as.matrix(as.data.frame(table2.syzyhz2[,"ZDMC"])))),
                                grep("脑梗",as.vector(as.matrix(as.data.frame(table2.syzyhz2[,"ZDMC"])))),
                                grep("动脉",as.vector(as.matrix(as.data.frame(table2.syzyhz2[,"ZDMC"]))))),]


table2.syzyhz4=table2.syzyhz4[!duplicated(table2.syzyhz4$CURENO),]     #一年住院心血管次数  ；住院天数


#table2.syzyhz2     #住院次数
#table2.syzyhz3     #住院所有心血管次数 （发生频率）
#table2.syzyhz4     #住院天数、住院心血管次数（一年）


#住院次数
table2.case.id$住院次数=rep(0,nrow(table2.case.id))
for(i in 1:nrow(table2.case.id)){
  x=which(table2.syzyhz2[,"ID"]==table2.case.id[i,"ID"])
  if(length(x)!=0){
    table2.case.id[i,"住院次数"]=length(x)
  }
}

#住院心血管次数
table2.case.id$住院心血管次数=rep(0,nrow(table2.case.id))
for(i in 1:nrow(table2.case.id)){
  x=which(table2.syzyhz4[,"ID"]==table2.case.id[i,"ID"])
  if(length(x)!=0){
    table2.case.id[i,"住院心血管次数"]=length(x)
  }
}

#住院所有心血管次数
table2.case.id$住院所有心血管次数=rep(0,nrow(table2.case.id))
for(i in 1:nrow(table2.case.id)){
  x=which(table2.syzyhz3[,"ID"]==table2.case.id[i,"ID"])
  if(length(x)!=0){
    table2.case.id[i,"住院所有心血管次数"]=length(x)
  }
}

#住院天数
table2.case.id$住院天数=rep(0,nrow(table2.case.id))
for(i in 1:nrow(table2.case.id)){
  x=which(table2.syzyhz4[,"ID"]==table2.case.id[i,"ID"])
  if(length(x)!=0){
    y=sum(table2.syzyhz4[x,"住院天数"])
    table2.case.id[i,"住院天数"]=y
  }
}

#发生频率

table2.case.id$end_start1=ifelse(table2.case.id$end_start1<1,1,table2.case.id$end_start1)
table2.case.id$end_start1=as.numeric(table2.case.id$end_start1)
table2.case.id$门诊加住院所有心血管次数=table2.case.id$门诊所有心血管次数+table2.case.id$住院所有心血管次数
table2.case.id$门诊发生频率=table2.case.id$门诊所有心血管次数/table2.case.id$end_start1
table2.case.id$总发生频率=table2.case.id$门诊加住院所有心血管次数/table2.case.id$end_start1


#人口学信息基线
table2.symzhz$MAX.E.KHMZH.=as.character(table2.symzhz$MAX.E.KHMZH.)
names(table2.symzhz)
table2.case.id1=table2.case.id%>%
  left_join(table2.symzhz%>%
              rename(ID=MAX.E.KHMZH.)%>%
              select(ID,MAX.DECODE.E.XINGB.0..男...女.,MAX.SENGR.,MAX.DECODE.E.JZFSM..1...自费..,MAX.DEPTNAME.),by="ID")

table2.case.id1=table2.case.id1[!duplicated(table2.case.id1$ID),]


#费用信息



table3.syzmzsj$MEDCARD=as.character(table3.syzmzsj$MEDCARD)

table3.syzmzsj1=table2.case.id1%>%
  left_join(table3.syzmzsj%>%
              rename(ID=MEDCARD),by="ID")%>%
  filter(time>=startday&time<=oneyear)

#门诊费用
table2.case.id1$门诊费用=rep(0,nrow(table2.case.id1))
for(i in 1:nrow(table2.case.id1)){
  x=which(table3.syzmzsj1[,"ID"]==table2.case.id1[i,"ID"])
  if(length(x)!=0){
    table2.case.id1[i,"门诊费用"]=sum(table3.syzmzsj1[x,"YFJE"])
  }
}


#心血管资源门诊费用

table3.syzmzsj1.heart=table3.syzmzsj1%>%
  left_join(table2.symzhz,by="MZLSH")%>%
  group_by(MZLSH)%>%
  filter(row_number()==1)%>%
  ungroup()

table3.syzmzsj1.heart1=table3.syzmzsj1.heart[grep("心",as.vector(as.matrix(as.data.frame(table3.syzmzsj1.heart[,"MAX.ICDDESC."])))),]%>%
  select(MZLSH)%>%
  left_join(table3.syzmzsj1,by="MZLSH")

table2.case.id1$门诊心血管资源费用=rep(0,nrow(table2.case.id1))
for(i in 1:nrow(table2.case.id1)){
  x=which(table3.syzmzsj1.heart1[,"ID"]==table2.case.id1[i,"ID"])
  if(length(x)!=0){
    table2.case.id1[i,"门诊心血管资源费用"]=sum(table3.syzmzsj1.heart1[x,"YFJE"])
  }
}


#住院费用
table3.syzzysj$MEDCARD=as.character(table3.syzzysj$MEDCARD)

table3.syzzysj1=table2.case.id1%>%
  left_join(table3.syzzysj%>%
              rename(ID=MEDCARD),by="ID")%>%
  filter(time>=startday&time<=oneyear)

table2.case.id1$住院费用=rep(0,nrow(table2.case.id1))
for(i in 1:nrow(table2.case.id1)){
  x=which(table3.syzzysj1[,"ID"]==table2.case.id1[i,"ID"])
  if(length(x)!=0){
    table2.case.id1[i,"住院费用"]=sum(table3.syzzysj1[x,"TOTAL"])
  }
}


#心血管资源住院费用

table3.syzzysj1.heart=table3.syzzysj1%>%
  left_join(table2.syzyhz,by="CURENO")%>%
  group_by(CURENO)%>%
  filter(row_number()==1)%>%
  ungroup()

table3.syzzysj1.heart1=table3.syzzysj1.heart[c(grep("心",as.vector(as.matrix(as.data.frame(table3.syzzysj1.heart[,"ZDMC"])))),
                                               grep("高血压",as.vector(as.matrix(as.data.frame(table3.syzzysj1.heart[,"ZDMC"])))),
                                               grep("脑梗",as.vector(as.matrix(as.data.frame(table3.syzzysj1.heart[,"ZDMC"])))),
                                               grep("动脉",as.vector(as.matrix(as.data.frame(table3.syzzysj1.heart[,"ZDMC"])))),
                                               grep("脑栓塞",as.vector(as.matrix(as.data.frame(table3.syzzysj1.heart[,"ZDMC"]))))),]

table3.syzzysj1.heart1=table3.syzzysj1.heart1[!duplicated(table3.syzzysj1.heart1$CURENO),]
sum(is.na(table3.syzzysj1.heart1$CURENO))
table3.syzzysj1.heart1=table3.syzzysj1.heart1%>%
  select(CURENO)%>%
  left_join(table3.syzzysj1,by="CURENO")

table2.case.id1$心血管资源住院费用=rep(0,nrow(table2.case.id1))
for(i in 1:nrow(table2.case.id1)){
  x=which(table3.syzzysj1.heart1[,"ID"]==table2.case.id1[i,"ID"])
  if(length(x)!=0){
    table2.case.id1[i,"心血管资源住院费用"]=sum(table3.syzzysj1.heart1[x,"TOTAL"])
  }
}


write.table(table2.case.id1,file = "华山实验组0204.csv",sep=',',row.names=F)








