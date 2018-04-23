library(dplyr)
library(doBy)
library(corrplot)
library(ggplot2)
data('dataCar')
attach(dataCar)
glimpse(dataCar)
str(dataCar)
names(dataCar)


table(veh_body, clm)
table(veh_age, clm)
table(veh_value, clm)
table(agecat, clm)
table(area, clm)
table(gender, clm)


table(veh_body, numclaims)
table(veh_age, numclaims)
table(veh_value, numclaims)
table(agecat, numclaims)
table(area, numclaims)
table(gender,numclaims)


#2)
# 차종에 따른 손해 발생 여부
ggplot(dataCar,aes(x=veh_body,fill=factor(clm)))+geom_bar(aes(y=(..count..)/sum(..count..)))+theme_bw()

ggplot(subset(dataCar,veh_body %in% c("HBACK","SEDAN","STNWG")),aes(x=veh_body,fill=factor(clm)))+
  geom_bar(aes(y=(..count..)/sum(..count..)))+facet_wrap(~agecat)+theme_bw()

# 연령대에 따른 손해 발생 여부
ggplot(dataCar,aes(x=agecat,fill=factor(clm)))+geom_bar(aes(y=(..count..)/sum(..count..)))+theme_bw()

# 성별에 따른 손해 발생 여부
ggplot(dataCar,aes(x=gender,fill=factor(clm)))+geom_bar(aes(y=(..count..)/sum(..count..)))+theme_bw()

# 연령대 x 성별에 따른 손해 발생 여부
dataCar$gender_agecat<-paste(dataCar$gender,dataCar$agecat,sep="")
ggplot(dataCar,aes(x=gender_agecat,fill=factor(clm)))+geom_bar(aes(y=(..count..)/sum(..count..)))+theme_bw()


# 상위 5개 차종에 대한 손해금액 확인 (0 제외)
top5_veh<-table(dataCar$veh_body)[order(-table(dataCar$veh_body))][1:5]
ggplot(data=subset(dataCar,clm==1 & veh_body %in% names(top5_veh)),aes(x=veh_body,y=claimcst0,color=veh_body))+geom_boxplot()+theme_bw()

summaryBy(claimcst0~veh_body,
          data=subset(dataCar,clm==1 & veh_body %in% names(top5_veh)),
          FUN=c(mean,median,length))

#3) vehicle value
names(dataCar)
Vehicle_value<-summaryBy(veh_value~veh_body+veh_age,data=dataCar,FUN=c(mean,length))
summary(Vehicle_value)

summary(dataCar_final[c("veh_body", "veh_age", "gender","area", "agecat")])

#참조수준 재설정
dataCar_final$veh_body<-relevel(dataCar_final$veh_body, "SEDAN")
dataCar_final$veh_body<-relevel(dataCar_final$veh_body, "SEDAN")


#
rm(list=ls())

# 데이터 불러오기
require(insuranceData)
require(doBy);require(ggplot2)
data(dataCar)
options(digits=2,scipen=10)

#2)
# 차종에 따른 손해 발생 여부
ggplot(dataCar,aes(x=veh_body,fill=factor(clm)))+geom_bar(aes(y=(..count..)/sum(..count..)))+theme_bw()

ggplot(subset(dataCar,veh_body %in% c("HBACK","SEDAN","STNWG")),aes(x=veh_body,fill=factor(clm)))+geom_bar(aes(y=(..count..)/sum(..count..)))+facet_wrap(~agecat)+theme_bw()

# 연령대에 따른 손해 발생 여부
ggplot(dataCar,aes(x=agecat,fill=factor(clm)))+geom_bar(aes(y=(..count..)/sum(..count..)))+theme_bw()

# 성별에 따른 손해 발생 여부
ggplot(dataCar,aes(x=gender,fill=factor(clm)))+geom_bar(aes(y=(..count..)/sum(..count..)))+theme_bw()

# 연령대 x 성별에 따른 손해 발생 여부
dataCar$gender_agecat<-paste(dataCar$gender,dataCar$agecat,sep="")
ggplot(dataCar,aes(x=gender_agecat,fill=factor(clm)))+geom_bar(aes(y=(..count..)/sum(..count..)))+theme_bw()


# 상위 5개 차종에 대한 손해금액 확인 (0 제외)
top5_veh<-table(dataCar$veh_body)[order(-table(dataCar$veh_body))][1:5]
ggplot(data=subset(dataCar,clm==1 & veh_body %in% names(top5_veh)),aes(x=veh_body,y=claimcst0,color=veh_body))+geom_boxplot()+theme_bw()

summaryBy(claimcst0~veh_body,
          data=subset(dataCar,clm==1 & veh_body %in% names(top5_veh)),
          FUN=c(mean,median,length))

#3) vehicle value
names(dataCar)
a<-summaryBy(veh_value~veh_body+veh_age,data=dataCar,FUN=c(mean,length))

#4) 

#6)
# 클레임이 발생한 상위 5개 차종의 케이스 확인
dim(subset(dataCar,clm==1))
dataCar_final<-subset(dataCar,clm==1 & veh_body %in% names(top5_veh),select=-c(11))

# factor 유형 변환
str(dataCar_final)
dataCar_final$veh_age<-as.factor(dataCar_final$veh_age)
dataCar_final$agecat<-as.factor(dataCar_final$agecat)



# 범주형 변수들의 수준별 빈도 확인
summary(dataCar_final[c("veh_body","veh_age","gender","area","agecat")])

# 참조 수준 재설정
dataCar_final$veh_body<-relevel(dataCar_final$veh_body,"SEDAN")
dataCar_final$veh_age<-relevel(dataCar_final$veh_age,"3")
dataCar_final$area<-relevel(dataCar_final$area,"C")
dataCar_final$agecat<-relevel(dataCar_final$agecat,"3")

#6)
# 일반화 선형 모형 적합
names(dataCar_final)
fit<-glm(claimcst0~.,data=subset(dataCar_final,select=c(veh_value,exposure,numclaims,claimcst0,veh_body,veh_age,gender,area,agecat)),Gamma(link="log"))

summary(fit)

ls(fit) 
a<-data.frame(exp(fit$coefficients))
                      