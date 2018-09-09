

library(readxl)
library(tidyverse)
library(data.table)

set.seed(108)

#华山医院
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




#华山医院++实验组

#obj2
table2.symz.case<-table2.symzmx[grepl("注射用丹参多酚酸盐",table2.symzmx[,5]),]
table2.symz.case$time<-substr(table2.symz.case[,13],1,10)
table2.syzy.case<-table2.syzymx[grepl("注射用丹参多酚酸盐",table2.syzymx[,7]),]
table2.syzy.case$time<-substr(table2.syzy.case[,2],1,10)


table2.case.id<-c(table2.symz.case[,3],table2.syzy.case[,1])
table2.case.id<-table2.case.id[!duplicated(table2.case.id)]
table2.case.id<-cbind(table2.case.id,rep(99999,length(table2.case.id)))
colnames(table2.syzy.case)[c(1,9)]<-c("ID","startdate")
colnames(table2.symz.case)[c(3,14)]<-c("ID","startdate")
table2.case.date<-rbind(table2.syzy.case[,c(1,9)],table2.symz.case[,c(3,14)])
head(table2.case.date)


table.mz.case.sum1=data.table(table2.case.date)
setkey(table.mz.case.sum1,ID,startdate)
hhh.id1=table.mz.case.sum1[!duplicated(table.mz.case.sum1$ID),"ID"]
for(i in 1:nrow(hhh.id1)){
  x=which(table.mz.case.sum1[,"ID"]==as.numeric(hhh.id1[i,"ID"]))
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
table2.case.id$门诊所有时间心血管次数=rep(0,nrow(table2.case.id))


for(i in 1:nrow(table2.symzhz)){
  for (j in 1:nrow(table2.case.id)) {
    if((table2.symzhz[i,3]==table2.case.id[j,"ID"])&(table2.symzhz[i,"time"]>=table2.case.id[j,"startday"])&(table2.symzhz[i,"time"]<=table2.case.id[j,"oneyear"])){
      table2.case.id[j,"门诊次数"]<-as.numeric(table2.case.id[j,"门诊次数"])+1
    }
  }
}

for (i in 1:nrow(table2.symzhz)) {
  for (j in 1:nrow(table2.case.id)) {
    if((as.character(table2.symzhz[i,3])==as.character(table2.case.id[j,1]))&(table2.symzhz[i,"time"]>=table2.case.id[j,"startday"])&(table2.symzhz[i,"time"]<=table2.case.id[j,"oneyear"])&(grepl("心",table2.symzhz[i,"诊断"]))){
      table2.case.id[j,"门诊心血管次数"]<-as.numeric(table2.case.id[j,"门诊心血管次数"])+1
    }
  }
}


for(i in 1:nrow(table2.symzhz)){
  for (j in 1:nrow(table2.case.id)) {
    if((table2.symzhz[i,3]==table2.case.id[j,"ID"])&(grepl("心",table2.symzhz[i,"诊断"]))){
      table2.case.id[j,"门诊所有时间心血管次数"]<-as.numeric(table2.case.id[j,"门诊所有时间心血管次数"])+1
    }
  }
}


#住院

table2.case.id$住院次数=rep(0,nrow(table2.case.id))
table2.case.id$住院心血管次数=rep(0,nrow(table2.case.id))
table2.case.id$住院天数=rep(0,nrow(table2.case.id))
table2.case.id$住院所有时间心血管次数=rep(0,nrow(table2.case.id))

#...........................
#住院次数
table2.syzyhz$住院天数=difftime(as.Date(table2.syzyhz$OUTDATE),as.Date(table2.syzyhz$ENTDATE),units = "days")
table2.case.id1=table2.case.id[,c("ID","startday","oneyear")]%>%
  left_join(table2.syzyhz%>%rename(ID=CURENO),by="ID")%>%
  filter(time>=startday&time<=oneyear)
table2.case.id1.cs=table(table2.case.id1$ID)%>%
  as.data.frame()%>%rename(住院次数=Freq,ID=Var1)
table2.case.id1.cs$ID=as.numeric(as.character(table2.case.id1.cs$ID))
#住院心血管次数
table2.case.id2=table2.case.id[,c("ID","startday","oneyear")]%>%
  left_join(table2.syzyhz%>%rename(ID=CURENO),by="ID")%>%
  filter(time>=startday&time<=oneyear)
table2.case.id2=table2.case.id2[grep("心",table2.case.id2[,"ZDMC"]),]
table2.case.id1.cs1=table(table2.case.id2$ID)%>%
  as.data.frame()%>%rename(住院心血管次数=Freq,ID=Var1)
table2.case.id1.cs1$ID=as.numeric(as.character(table2.case.id1.cs1$ID))
#住院天数
table2.case.id2=table2.case.id[,c("ID","startday","oneyear")]%>%
  left_join(table2.syzyhz%>%rename(ID=CURENO),by="ID")%>%
  filter(time>=startday&time<=oneyear)
table2.case.id2=table2.case.id2[grep("心",table2.case.id2[,"ZDMC"]),]
table2.case.id1.cs2=table(table2.case.id2$ID,table2.case.id2$住院天数)%>%
  as.data.frame()%>%filter(Freq>0)%>%rename(住院天数=Var2,ID=Var1)
table2.case.id1.cs2$ID=as.numeric(as.character(table2.case.id1.cs2$ID))
table2.case.id1.cs2$住院天数=as.numeric(as.character(table2.case.id1.cs2$住院天数))
#住院所有时间心血管次数
table2.case.id2=table2.case.id[,c("ID","startday","oneyear")]%>%
  left_join(table2.syzyhz%>%rename(ID=CURENO),by="ID")
table2.case.id2=table2.case.id2[c(grep("心",table2.case.id2[,"ZDMC"]),grep("动脉",table2.case.id2[,"ZDMC"]),grep("高血压",table2.case.id2[,"ZDMC"])),]
table2.case.id1.cs3=table(table2.case.id2$ID)%>%
  as.data.frame()%>%rename(住院所有时间心血管次数=Freq,ID=Var1)
table2.case.id1.cs3$ID=as.numeric(as.character(table2.case.id1.cs3$ID))
table2.case.id.x=left_join(table2.case.id,table2.case.id1.cs3,by="ID")%>%
  left_join(table2.case.id1.cs2,by="ID")%>%
  left_join(table2.case.id1.cs1,by="ID")%>%
  left_join(table2.case.id1.cs,by="ID")%>%
  select(-Freq)

table2.case.id.x[is.na(table2.case.id.x)]<-0

write.table(table2.case.id.x,file = "h:/data/gaoyue1221合并/0110newcode/华山实验组obj2.csv",sep = ",",row.names = F)
###########...................
#















