##########기본패키지 및 설정 #####################
setwd("C:/Users/Admin/Desktop/끝난플젝/2018ZPER 2ndCompetitionData")

rm(list = ls())
gc()
library(caret)
library(dplyr)
library(reshape)
library(Amelia)
require(mice)
library(ROSE)
library(DMwR)
library(devtools);#install_github("RomeroBarata/bimba",force=TRUE)
library(caret)
library(xgboost)
library(kernlab)
library(e1071)
library(data.table)
### skip###############
train =read.csv("train_nona921.csv")
test =read.csv("test_nona921.csv")
sum(is.na(train))
sum(is.na(test))
str(train)
str(test)

train$instkind <-as.factor(train$instkind)
test$instkind <-as.factor(test$instkind)
train$sido <-as.factor(train$sido)
test$sido <-as.factor(test$sido)
train$openDate<-as.numeric(substr(train$openDate,1,4))
test$openDate<-as.numeric(substr(test$openDate,1,4))

#data_set 만들기 
colnames(train)
train_OC <- train$OC;
write.csv(train_OC,"train_OC.csv",row.names = F)
head(train_OC)
train =train[,-58]
data_set =data.frame(rbind(train,test))

fac =sapply(data_set,is.factor)
num =sapply(data_set,is.numeric)
num #inst_id sgg openDate employee1 employee2
a=data_set[fac]; str(a)
b=data_set[num]; str(b)

#수치형 데이터들 scale  
b1 =scale(b[,-c(1:4,53:54)]) 
head(b1)

ds=cbind(b[c(1:4,53:54)],b1)
ds=cbind(ds,a) # scale +factor 
data_set =ds

train =data_set[1:293,]
train = cbind(train,"OC"=train_OC); 
str(train)

#write.csv(data_set,"scale_data.csv",row.names = F)
data_set <-read.csv("scale_data.csv") #스케일된 데이터 
#factor처리 
data_set$instkind <-as.factor(data_set$instkind)
data_set$sido <-as.factor(data_set$sido)

str(data_set)

###0 원본데이터 및 전처리 #########################################
rm(list = ls())
gc()
#raw data
train =read.csv("train.csv")
test =read.csv("test.csv")

table(train$OC) #tartget값이 매우 불균형함 .

str(train)
str(test)

###1 이상한 데이터& 결측치 처리 #####################

#결측치 확인 
sum(is.na(train)) #424
colSums(is.na(train)) %>% as.data.frame() 

length(which(is.na(train$debt1))) #8
length(which(is.na(train$ownerChange))) # 12
length(which(is.na(train$bedCount))) # 5
length(which(is.na(train$employee2))) # 13
length(which(is.na(train$employee1))) # 10

sum(is.na(test)) #247개 
colSums(is.na(test)) %>% as.data.frame()
which(is.na(test$debt1)) # 2
sum(is.na(test$openDate)) #1
sum(is.na(test$bedCount)) #8
sum(is.na(test$ownerChange)) #15

#->  특정변수의 경우 결측치가 매우 많음 

str(train)
str(test)
# "" 데이터 처리 -> NA로 
#test의 employee1,2는 factor형태 

train$instkind = ifelse(train$instkind=="",NA,train$instkind)
test$instkind = ifelse(test$instkind=="",NA,test$instkind) # 2개 
test$employee1 = ifelse(test$employee1==""|test$employee1=="_",NA,test$employee1)
test$employee2 = ifelse(test$employee1==""|test$employee2=="_",NA,test$employee2)
test$employee1<-test$employee1 %>% as.character() %>% as.integer()
test$employee2<-test$employee2 %>% as.character() %>% as.integer()

str(train)
str(test)
sum(is.na(train))
sum(is.na(test))

#train 재무제표 결측치 제거
#train %>% View()
missmap(train) #결측치 시각화 결과 revenue1가 NA인 경우, 나머지 재무제표 요소들도 NA임
               #따라서 revenue1가 NA인 데이터를 제거 
train1 =train[-c(which(is.na(train$revenue1))),] 
missmap(train1) #결측치가 거의 제거된 것을 확인 

colSums(is.na(train1)) %>% as.data.frame() 
#나머지 결측치 ->  bedCount:5 / instkind:1 / employee1:8 /employee2:11 / ownerChange:10
#bedcount / employee1 / employee2의 경우 NA를 0으로 대체 
#ownerchange의 경우 범주형으로  "X"라는 범주를 생성 
#instkinds는 보다 관찰 후 mice패키지로 채움.
train1[is.na(train1$bedCount),]$bedCount <-0
train1[is.na(train1$employee1),]$employee1 <-0
train1[is.na(train1$employee2),]$employee2 <-0
train1$ownerChange<-train1$ownerChange %>% as.character()
train1$ownerChange<- ifelse(is.na(train1$ownerChange),"non",train1$ownerChange) %>% as.factor()
missmap(train1)

#mice
imp<-mice(train1,method = "cart",seed=1234)
train_mice<-complete(imp)
missmap(train_mice)
str(train_mice)

#test 재무제표 결측치 -> 0으로 처리 (test셋은 제거 불가능 )
colnames(test[,7:54])
missmap(test)
test<-test %>% select(-OC) #test OC행 결측치 제거
colSums(is.na(test)) %>% as.data.frame()
test[,7:54][is.na(test[,7:54])]<-0

#test 행 결측치
missmap(test)
colSums(is.na(test)) %>% as.data.frame()
#openDate:1 / bedCount:8 / instkind:2 / employee1:8 / employee2:8 / ownerChange:15
test[is.na(test$bedCount),]$bedCount <-0
test[is.na(test$employee1),]$employee1 <-0
test[is.na(test$employee2),]$employee2 <-0
test$ownerChange<-test$ownerChange %>% as.character()
test$ownerChange<- ifelse(is.na(test$ownerChange),"non",test$ownerChange) %>% as.factor()

imp<-mice(test,method = "cart",seed=1234)
test_mice<-complete(imp)
missmap(test_mice)
#md.pattern(train)
sum(is.na(train_mice))
sum(is.na(test_mice))
#http://blog.naver.com/PostView.nhn?blogId=mokeya1&logNo=221337295488&categoryNo=0&parentCategoryNo=0&viewDate=&currentPage=1&postListTopCurrentPage=1&from=postView
##mice sample 
# mice(data[,-2],m=5,maxit=10,method='cart',seed=1234)
#mice_train <- mice(train[,-1],m=5,maxit=10,meth='cart',seed=1234) #나중에 m, maxit 조정 
#train_re = complete(mice_train,3) #N개중 하나만, 이 후 각각의 데이터의 앙상블도 결과 나쁘지 않음, or 평균으로 

#save(mice_train, file="mice_train.RData")
#save(mice_test, file="mice_test.RData")
#save(mice_train, file="mice_train2.RData")
#save(mice_test, file="mice_test2.RData")
#load(file="mice_train2.RData")
#load(file="mice_test2.RData")

str(train_mice)
str(test_mice)
target<-train_mice$OC
train_mice<-train_mice %>% select(-OC)
dim(train_mice); dim(test_mice)

#compare two dataframe column class
m <- sapply(list(df1 = train_mice, df2 = test_mice), sapply, class)
m[m[, "df1"] != m[, "df2"],]

# 합치기 전에 전처리
train_mice$openDate<-train_mice$openDate %>% as.numeric()
train_mice$inventoryAsset1<-train_mice$inventoryAsset1 %>% as.numeric()
train_mice$receivableL1<-train_mice$receivableL1 %>% as.numeric()
train_mice$inventoryAsset2<-train_mice$inventoryAsset2 %>% as.numeric()
train_mice$receivableL2<-train_mice$receivableL2 %>% as.numeric()

all_data<-rbind(train_mice,test_mice)
str(all_data)
sum(is.na(all_data))

# idx 합치기 ? skip
#train =rename(train,c("train$inst_id"="inst_id"))
#test =rename(test,c("test$inst_id"="inst_id"))
str(all_data)

# 사실 best는 train-test merge - 전처리 - 분리지만, 뭐 이미 해버렸으니깐. 
all_data$sgg<-as.factor(all_data$sgg)
all_data$instkind<-as.factor(all_data$instkind)
str(target)
#write.csv(all_data,"cleaning_data.csv",row.names = F)
#train_re<-all_data %>% semi_join(train_mice,by=c('inst_id'))
#test_re<-all_data %>% semi_join(test_mice,by=c('inst_id'))
#train_re$OC <- target

str(train_re)
str(test_re)

dim(train_re)
dim(test_re)

#factor 맞추기 
colN = c("busan", "choongbuk", "choongnam", "daegu", "daejeon",
         "gangwon", "gwangju", "gyeongbuk", "gyeonggi", "gyeongnam",
         "incheon", "jeonbuk", "jeonnam", "sejong", "seoul",
         "ulsan", "jeju")


train_re$sido = factor(train_re$sido, levels = colN)
test_re$sido = factor(test_re$sido, levels = colN)


#결측치 제거한 데이터
#write.csv(target,"target.csv",row.names = F)
#write.csv(train_re,"train_non.csv",row.names = F)
#write.csv(test_re,"test_non.csv",row.names = F)

#### clening data 불러오기 ######################
rm(list = ls())
gc()
#### 개폐업 비교  
#train<-read.csv("train_non.csv")
#test<-read.csv("test_non.csv")
all_data<-read.csv("cleaning_data.csv")
str(all_data)
###2-1 데이터 분석1 기본변수 #####################################
train<-all_data[1:293,]
test<-all_data[294:420,]
#다시 불러오니까 factor reset됨 (주의할것)  ->합친거 불러와서 나누기 

table(train$OC) #301개 병원중 15개 병원이 폐업함 -> 개/폐업 class

open =train[train$OC=='open',]
close=train[train$OC==' close',]

graphics.off()
par("mar")
par(mar=c(1,1,1,1))

###연속형

par(mfrow=c(1,3))
colnames(train)

#bedCount
boxplot(open$bedCount,main ="open"
        ,ylim=c(0,375)
)$stats
boxplot(close$bedCount,main ="close"
        ,ylim=c(0,375)
)$stats
boxplot(test$bedCount,main ="test"
        ,ylim=c(0,375)
)$stats

#revenue1
boxplot(open$revenue1,main ="open"
        ,ylim=c(0,3.0e+10)
)$stats
summary(open$revenue1)
boxplot(close$revenue1,main ="close"
        ,ylim=c(0,3.0e+10)
)$stats
boxplot(test$revenue1,main ="test"
        ,ylim=c(-0.6,3.0e+10)
)$stats

a<-ifelse(train$revenue1> -0.24 ,0,1)
table(train$OC,a)

#salescost1  
boxplot(open$salescost1,main ="open"
        ,ylim=c(-0.3,0)
)$stats
boxplot(close$salescost1,main ="close"
        ,ylim=c(-0.3,0)
)$stats
boxplot(test$salescost1,main ="test"
        ,ylim=c(-0.3,0)
)$stats
a<-ifelse(train$salescost1 > -0.176 &train$salescost1< -0.03,0,1)
table(train$OC,a)

#sga1   #-0.2
boxplot(open$sga1,main ="open"
        #,ylim=c(-0.7,0)
)$stats
boxplot(close$sga1 ,main ="close"
        #,ylim=c(-0.7,0)
)$stats
boxplot(test$sga1 ,main ="test"
        #,ylim=c(-0.7,0)
)$stats
a<-ifelse(train$sga1 > -0.2,0,1)
table(train$OC,a)

#salary1    
boxplot(open$salary1,main ="open"
        ,ylim=c(-0.7,0)
)$stats
boxplot(close$salary1 ,main ="close"
        ,ylim=c(-0.7,0)
)$stats

a<-ifelse(train$salary1 > -0.19,0,ifelse(train$salary1 > -0.42&train$salary1 < -0.19,1,2))
table(train$OC,a)

#noi1  #큰 의미 없을 
boxplot(open$noi1 ,main ="open"
        ,ylim=c(-0.4,0.2)
)$stats
boxplot(close$noi1 ,main ="close"
        ,ylim=c(-0.4,0.2)
)$stats

#noe1 #큰 차이 없음 
boxplot(open$noe1 ,main ="open"
        ,ylim=c(-0.45,0.2)
)$stats
boxplot(close$noe1  ,main ="close"
        ,ylim=c(-0.45,0.2)
)$stats

#interest1  #큰차이 없지만 close가 더 4분위가 높은 경우   
boxplot(open$interest1,main ="open"
        ,ylim=c(-0.6,0.8)
)$stats
boxplot(close$interest1 ,main ="close"
        ,ylim=c(-0.6,0.8)
)$stats

a<-ifelse(train$interest1>0.69,0,1)
table(train$OC,a)

#ctax1  #좋아 
boxplot(open$ctax1,main ="open"
        ,ylim=c(-0.35,0.1)
)$stats
boxplot(close$ctax1 ,main ="close"
        ,ylim=c(-0.35,0.1)
)$stats

a<- ifelse(train$ctax1>-0.28,0,1)
table(train$OC,a)

#profit1  
boxplot(open$profit1,main ="open"
        ,ylim=c(-0.5,0.3)
)$stats
boxplot(close$profit1 ,main ="close"
        ,ylim=c(-0.5,0.3)
)$stats

a<-ifelse(train$profit1>-0.187&train$profit1< 0.02,0,1)
table(train$OC,a)

#liquidAsset1   
boxplot(open$liquidAsset1,main ="open"
        ,ylim=c(-0.6,0.5)
)$stats
boxplot(close$liquidAsset1 ,main ="close"
        ,ylim=c(-0.6,0.5)
)$stats

#quickAsset1    
boxplot(open$quickAsset1 ,main ="open"
        ,ylim=c(-0.6,0.5)
)$stats
boxplot(close$quickAsset1  ,main ="close"
        ,ylim=c(-0.6,0.5)
)$stats

#receivableS1    
boxplot(open$receivableS1 ,main ="open"
        ,ylim=c(-0.45,0.66)
)$stats
boxplot(close$receivableS1  ,main ="close"
        ,ylim=c(-0.45,0.66)
)$stats
#흠 이런 경우는 ?
a<-ifelse(train$receivableS1>-0.035& train$receivableS1<0.21,1,0)
table(train$OC,a)

#inventoryAsset1    
boxplot(open$inventoryAsset1 ,main ="open"
        ,ylim=c(-0.5,0.5)
)$stats
boxplot(close$inventoryAsset1  ,main ="close"
        ,ylim=c(-0.5,0.5)
)$stats

#nonCAsset1   #큰차이 없을듯    
boxplot(open$nonCAsset1 ,main ="open"
        ,ylim=c(-0.7,0.8)
)$stats
boxplot(close$nonCAsset1  ,main ="close"
        ,ylim=c(-0.7,0.8)
)$stats

#tanAsset1  #비
boxplot(open$tanAsset1 ,main ="open"
        ,ylim=c(-0.7,0.8)
)$stats
boxplot(close$tanAsset1  ,main ="close"
        ,ylim=c(-0.7,0.8)
)$stats

#OnonCAsset1  의미있을듯
boxplot(open$OnonCAsset1 ,main ="open"
        ,ylim=c(-0.4,0.1)
)$stats
boxplot(close$OnonCAsset1  ,main ="close"
        ,ylim=c(-0.4,0.1)
)$stats

a= ifelse(train$OnonCAsset1>-0.29,1,0)
table(train$OC,a)

#receivableL1  의미없는 변수 
boxplot(open$receivableL1 ,main ="open"
        ,ylim=c(-0.7,0.8)
)$stats
boxplot(close$receivableL1  ,main ="close"
        ,ylim=c(-0.7,0.8)
)$stats

#debt1    X
boxplot(open$debt1 ,main ="open"
        ,ylim=c(-0.7,0.6)
)$stats
boxplot(close$debt1  ,main ="close"
        ,ylim=c(-0.7,0.6)
)$stats

#liquidLiabilities1  
boxplot(open$liquidLiabilities1 ,main ="open"
        ,ylim=c(-0.7,0.8)
)$stats
boxplot(close$liquidLiabilities1  ,main ="close"
        ,ylim=c(-0.7,0.8)
)$stats

#shortLoan1  #의미 있
boxplot(open$shortLoan1 ,main ="open"
        ,ylim=c(-0.5,0.8)
)$stats
boxplot(close$shortLoan1  ,main ="close"
        ,ylim=c(-0.5,0.8)
)$stats

a= ifelse(train$shortLoan1>-0.37,1,0)
table(train$OC,a)

#NCLiabilities1   X 
boxplot(open$NCLiabilities1 ,main ="open"
        ,ylim=c(-0.6,0.7)
)$stats
boxplot(close$NCLiabilities1  ,main ="close"
        ,ylim=c(-0.6,0.7)
)$stats

#longLoan1    X
boxplot(open$longLoan1 ,main ="open"
        ,ylim=c(-0.65,0.95)
)$stats
boxplot(close$longLoan1  ,main ="close"
        ,ylim=c(-0.65,0.95)
)$stats

#netAsset1     
boxplot(open$netAsset1 ,main ="open"
        ,ylim=c(-0.9,0.6)
)$stats
boxplot(close$netAsset1  ,main ="close"
        ,ylim=c(-0.9,0.6)
)$stats

a = ifelse(train$netAsset1 >=-0.51626238& train$netAsset1<=0.09261104,1,0)
table(train$OC,a)

#surplus1     
boxplot(open$surplus1 ,main ="open"
        ,ylim=c(-0.22,-0.14)
)$stats
boxplot(close$surplus1  ,main ="close"
        ,ylim=c(-0.22,-0.14)
)$stats

#이상치 inx 추출  -skip
#out <- sapply(train$revenue1, function(x) x <0| x > 26700482717)
#sum(out) #총 32개, close는 0
#train$revenue1<-ifelse(train$revenue1<0 |train$revenue1>26700482717,NA,train$revenue1)
#summary(train$revenue1,na.rm=T)

###범주형 
str(train)
table(train$OC,train$sido)
table(train$OC,train$ownerChange) #영향력 크다고 볼수 있음 
table(train$OC,train$openDate)
table(train$OC,train$instkind) #일반화의 가능성 있음. 

str(train$ownerChange)
change =train[train$ownerChange=="change"&train$OC==" close",]
table(change$OC)
str(train)
###2-2 데이터 분석2 파생변수#######################
#시각화는 변수를 만들고 open/close 재설정 후 해야함 
#new-variable 2017 year  #scale된 데이터를 통한 파생변수
str(data_set)

# 당좌비율 = Qratio
train$Qratio<-(train$liquidAsset1-train$inventoryAsset1)/train$liquidLiabilities1
train$Qratio<-ifelse(is.na(train$Qratio), 0, train$Qratio)
summary(train$Qratio)
sort(train$Qratio)
train$Qratio<-ifelse(train$Qratio==Inf, 1, ifelse(train$Qratio==-Inf,-1,train$Qratio ))

boxplot(open$Qratio ,main ="open"
        ,ylim=c(-1,4)
)$stats
boxplot(close$Qratio  ,main ="close"
        ,ylim=c(-1,4)
)$stats

a<-ifelse(train$Qratio>1.18&train$Qratio<2.59,1,0)
table(train$OC,a)


# 부채비율 = Dratio
train$Dratio<- (train$debt1 / train$netAsset1)
train$Dratio<-ifelse(is.na(train$Dratio), 0, train$Dratio)
summary(train$Dratio)
sort(train$Dratio)
train$Dratio<-ifelse(train$Dratio==Inf, 1, ifelse(train$Dratio==-Inf,-1,train$Dratio ))

boxplot(open$Dratio ,main ="open"
        ,ylim=c(-1,4)
)$stats
boxplot(close$Dratio  ,main ="close"
        ,ylim=c(-1,4)
)$stats

a<-ifelse(train$Dratio>0.46&train$Dratio<2.01,1,0)
table(train$OC,a)


# 자기자본승수 = Ocapital
train$Ocapital <- (train$debt1 + train$netAsset1) / train$netAsset1
train$Ocapital<-ifelse(is.na(train$Ocapital), 0, train$Ocapital)
summary(train$Ocapital)
sort(train$Ocapital)
train$Ocapital<-ifelse(train$Ocapital==Inf, 1, ifelse(train$Ocapital==-Inf,-1,train$Ocapital ))

boxplot(open$Ocapital ,main ="open"
        ,ylim=c(-0.2,4.5)
)$stats
boxplot(close$Ocapital  ,main ="close"
        ,ylim=c(-0.2,4.5)
)$stats

a<-ifelse(train$Ocapital>1.46&train$Ocapital<3.1,1,0)
table(train$OC,a)


# 이자보상비율 = ICratio    애매함 
train$ICratio <- (train$profit1+train$ctax1+train$interest1) / train$interest1
train$ICratio<-ifelse(is.na(train$ICratio), 0, train$ICratio)
summary(train$ICratio)
sort(train$ICratio)
train$ICratio<-ifelse(train$ICratio==Inf, 1, ifelse(train$ICratio==-Inf,-1,train$ICratio ))

boxplot(open$ICratio ,main ="open"
        ,ylim=c(-2,7)
)$stats
boxplot(close$ICratio  ,main ="close"
        ,ylim=c(-2,7)
)$stats


# 재고자산회전율 = Iturn
# 재고자산    애매함 
k<-(train$inventoryAsset1 + train$inventoryAsset2) /2
train$Iturn <- train$salescost1 / k
train$Iturn<-ifelse(is.na(train$Iturn), 0, train$Iturn)
summary(train$Iturn)
sort(train$Iturn)

boxplot(open$Iturn ,main ="open"
        ,ylim=c(-70,65)
)$stats
boxplot(close$Iturn  ,main ="close"
        ,ylim=c(-70,65)
)$stats

a<-ifelse(train$Iturn<-54.2&train$Iturn>-70,1,0)
table(train$OC,a)


# 재고일수 = Iday    극단적임 
train$Iday<- 365/train$Iturn
train$Iday<-ifelse(is.na(train$Iday), 0, train$Iday)
summary(train$Iday)
sort(train$Iday)
train$Iday<-ifelse(train$Iday==Inf, 1, ifelse(train$Iday==-Inf,-1,train$Iday))

boxplot(open$Iday ,main ="open"
        ,ylim=c(-340,240)
)$stats
boxplot(close$Iday  ,main ="close"
        ,ylim=c(-340,240)
)$stats

a<-ifelse(train$Iday>17,1,0)
table(train$OC,a)


# 매출액 이익률 = PMratio  흠 
train$PMratio<-train$profit1 /train$revenue1
train$PMratio<-ifelse(is.na(train$PMratio), 0, train$PMratio)
summary(train$PMratio)
sort(train$PMratio)
train$PMratio<-ifelse(train$PMratio==Inf, 1, ifelse(train$PMratio==-Inf,-1,train$PMratio))

boxplot(open$PMratio ,main ="open"
        ,ylim=c(-4,4.5)
)$stats
boxplot(close$PMratio  ,main ="close"
        ,ylim=c(-4,4.5)
)$stats

a<-ifelse(train$PMratio <4.22 & train$PMratio>1.5 ,1,0)
a<-ifelse(train$PMratio< -1 & train$PMratio>-4,1,0)
a<-ifelse((train$PMratio <4.22 & train$PMratio>1.5)|(train$PMratio< -1 & train$PMratio>-4),1,0)
table(train$OC,a)


# ROA 
train$ROA<-train$profit1 / (train$debt1 + train$netAsset1)
train$ROA<-ifelse(is.na(train$ROA), 0, train$ROA)
summary(train$ROA)
sort(train$ROA)
train$ROA<-ifelse(train$ROA==Inf, 1, ifelse(train$ROA==-Inf,-1,train$ROA))

boxplot(open$ROA ,main ="open"
        ,ylim=c(-.6,1.4)
)$stats
boxplot(close$ROA  ,main ="close"
        ,ylim=c(-.6,1.4)
)$stats

a<-ifelse(train$ROA< -0.25&train$ROA>-0.58,1,0)
table(train$OC,a)


#ROE X
train$ROE <- train$profit1 / train$netAsset1
train$ROE<-ifelse(is.na(train$ROE), 0, train$ROE)
summary(train$ROE)
sort(train$ROE)

boxplot(open$ROE ,main ="open"
        ,ylim=c(-1.3,2.7)
)$stats
boxplot(close$ROE  ,main ="close"
        ,ylim=c(-1.3,2.7)
)$stats

a<-ifelse(train$ROE>17,1,0)
table(train$OC,a)


#이상치 영향 적게 -> Median Absolute Deviance
mad(open$ROE)
mad(close$ROE)
#현재 기본 변수들. 
# 시도 번호/ 지역별 번호/ 개업날짜/ 침상수/ 병원종류/ 2016/2017 재무제표내용(순이익 등..) /
# 2016/2017 고용직원 수/ 병원대표 변동여부/ 

###3-1 변수 만들기1 파생변수 #######
str(all_data)
#date to year/ 2017- year
all_data$year<-substr(all_data$openDate,1,4)
all_data$year<-substr(all_data$openDate,1,4)
all_data$difyea<-2017-all_data$year %>% as.numeric()
#규모 = bedcount+employee1+emlpoyee2 
#employeeall = employee1+employee2
all_data$employee_mean<-(all_data$employee1+all_data$employee2)/2
all_data$size <-all_data$bedCount+all_data$employee_mean

#2017-2016 관련 데이터 전부
colnames(all_data) #7~31 : 24
difvar<-data.frame('inst_id'=1:420)
for(i in 7:30){
  dif=all_data[,i]-all_data[,i+24] %>% as.data.frame()
  difvar<-cbind(difvar,dif)
}
n=c()
for ( i in 1:24){
  a=paste0("dif",i)
  n=append(n,a)
}
colnames(difvar)<-c("inst_id",n)
all_data2<-cbind(all_data,difvar)
colnames(all_data2)
###3-2 변수 만들기2 파생변수 ########
#### 기본 2017 year + 2016-2017 제무재표관련 변수 

# 당좌비율 = Qratio
all_data$Qratio<-((all_data$liquidAsset1-all_data$inventoryAsset1)/all_data$liquidLiabilities1)
all_data$Qratio<-ifelse(is.na(all_data$Qratio), 0, all_data$Qratio)
sort(all_data$Qratio)
all_data$Qratio<-ifelse(all_data$Qratio==Inf, 1, ifelse(all_data$Qratio==-Inf,-1,all_data$Qratio ))

# 부채비율 = Dratio
all_data$Dratio<- (all_data$debt1 / all_data$netAsset1)
all_data$Dratio<-ifelse(is.na(all_data$Dratio), 0, all_data$Dratio)
sort(all_data$Dratio)
all_data$Dratio<-ifelse(all_data$Dratio==Inf, 1, ifelse(all_data$Dratio==-Inf,-1,all_data$Dratio ))

# 자기자본승수 = Ocapital
all_data$Ocapital <- (all_data$debt1 + all_data$netAsset1) / all_data$netAsset1
all_data$Ocapital<-ifelse(is.na(all_data$Ocapital), 0, all_data$Ocapital)
sort(all_data$Ocapital)
all_data$Ocapital<-ifelse(all_data$Ocapital==Inf, 1, ifelse(all_data$Ocapital==-Inf,-1,all_data$Ocapital ))

# 이자보상비율 = ICratio
all_data$ICratio <- (all_data$profit1+all_data$ctax1+all_data$interest1) / all_data$interest1
all_data$ICratio<-ifelse(is.na(all_data$ICratio), 0, all_data$ICratio)
sort(all_data$ICratio)
all_data$ICratio<-ifelse(all_data$ICratio==Inf, 1, ifelse(all_data$ICratio==-Inf,-1,all_data$ICratio ))

# 재고자산회전율 = Iturn
# 재고자산
k<-(all_data$inventoryAsset1 + all_data$inventoryAsset2) /2
all_data$Iturn <- all_data$salescost1 / k
all_data$Iturn<-ifelse(is.na(all_data$Iturn), 0, all_data$Iturn)
sort(all_data$Iturn)
all_data$Iturn<-ifelse(all_data$Iturn==Inf, 1, ifelse(all_data$Iturn==-Inf,-1,all_data$Iturn ))

# 재고일수 = Iday
all_data$Iday<- 365/all_data$Iturn
all_data$Iday<-ifelse(is.na(all_data$Iday), 0, all_data$Iday)
sort(all_data$Iday)
all_data$Iday<-ifelse(all_data$Iday==Inf, 1, ifelse(all_data$Iday==-Inf,-1,all_data$Iday ))

# 매출액 이익률 = PMratio
all_data$PMratio<-all_data$profit1 / all_data$revenue1
all_data$PMratio<-ifelse(is.na(all_data$PMratio), 0, all_data$PMratio)
sort(all_data$PMratio)
all_data$PMratio<-ifelse(all_data$PMratio==Inf, 1, ifelse(all_data$PMratio==-Inf,-1,all_data$PMratio ))

# ROA 
all_data$ROA<-all_data$profit1 / (all_data$debt1 + all_data$netAsset1)
all_data$ROA<-ifelse(is.na(all_data$ROA), 0, all_data$ROA)
sort(all_data$ROA)
all_data$ROA<-ifelse(all_data$ROA==Inf, 1, ifelse(all_data$ROA==-Inf,-1,all_data$ROA ))

#ROE
all_data$ROE <- all_data$profit1 / all_data$netAsset1
all_data$ROE<-ifelse(is.na(all_data$ROE), 0, all_data$ROE)
sort(all_data$ROE)
all_data$ROE<-ifelse(all_data$ROE==Inf, 1, ifelse(all_data$ROE==-Inf,-1,all_data$ROE ))

write.csv(all_data2,"cleaning_data2.csv",row.names = F)
### 정규화/factor to 더미변수화 및 train/test쪼개기###########
#정규화 
all_data <-read.csv("cleaning_data2.csv")
target<-read.csv("target.csv")
colnames(all_data)

str(all_data)
str(target)
all_data$instkind<-as.factor(all_data$instkind)
sapply(all_data,is.factor) %>% as.data.frame()
#scale
#data_set[,c(7:54)]<-scale(data_set[,c(7:54)])
#write.csv(data_set,"scale.csv",row.names = FALSE)

#log 는 시각화시 

#더미변수화
str(all_data)
colnames(all_data)
all_data2<- all_data

dum1=model.matrix(~instkind,all_data2)
colnames(dum1)<-paste(colnames(dum1),"_1",sep = "")
dum2=model.matrix(~ownerChange,all_data2)
colnames(dum2)<-paste(colnames(dum2),"_2",sep = "")
dum3=model.matrix(~sido,all_data2)
colnames(dum3)<-paste(colnames(dum3),"_3",sep = "")
dum<-cbind(dum1,dum2,dum3)
colnames(dum)
colnames(all_data2)
all_data2= all_data2 %>% select(-c(sido,ownerChange,instkind))
all_data2 =cbind(all_data2,dum)

#+더미변수 caret 사용 
#dum=dummyVars(~instkind+ownerChange,data_set)
#dum=predict(dum,data_set)
#colnames(dum)
#colnames(data_set)
#data_set1= data_set[,-c(6,57)]
#data_set1 =cbind(data_set1,dum)

#쪼개기
train =all_data2[1:293,]
test =all_data2[294:420,]
train$OC<-target$x

dim(train)
dim(test)

colnames(train)
colnames(test)
#train$ownerChangesame<-as.factor(train$ownerChangesame)
#test$ownerChangesame<-as.factor(test$ownerChangesame)

### 변수 선택 #########################
library(mlbench)
zv =nearZeroVar(train, saveMetrics=TRUE) #nzv이 TRUE인 것 제거 
zv$nzv
colnames(train[zv$nzv])
colnames(data_set1)
data_set2 = data_set1[,-c(41,46,70,71)]

library(FSelector)
str(data_set1)
dcor =cor(data_set2)
findCorrelation(dcor,cutoff = 0.9)
data_set2 =data_set2[,-c(30,6,45,19,23,32,42,20,8,24,9, 15, 16, 41, 48, 50, 31, 12, 52 ,51, 55, 69)]

train1 =data_set2[1:293,]
test =data_set2[294:420,]
train =read.csv("train_nona1.csv")
train1$OC <- train$OC
train <- train1
dim(train)

### 샘플링 ###########################

## under sampling
table(train$OC) #278 / 15 
cl = train[train$OC!="open",] #15개 close 데이터 
op = train[train$OC=="open",] 

idx = sample(1:nrow(op), 90) #open 데이터를  278에서 90개 추출 
op = op[idx, ]
train1 =rbind(cl,op)
str(train1)
table(train1$OC)

## ROSE -skip
## SMOTE
table(train$OC)
sample =SMOTE(OC~.,data =train, 
              perc.over=900 ,# 적은 쪽의 데이터를 얼마나 추가로 샘플링해야 하는지 (n+1배 )
              k=5           # 고려할 최근접 이웃의 수
              # 적은 쪽의 데이터를 추가로 샘플링할 때 각 샘플에 대응해서 많은 쪽의 데이터를
              # 얼마나 샘플링할지 지
)

table(sample$OC) #300 / 165 
str(sample)

### 해당 알고리즘들은 numeric 만 가능 -skip###############
train1 =train
str(train1)
table(train1$OC)
dum=model.matrix(~instkind+ownerChange,train1)
colnames(dum)
colnames(train1)
train1= train1[,-c(6,9)]
train1 =cbind(train1,dum)

#str(train1)
#train1$OC <-as.character(train1$OC)
#train1$OC<-ifelse(train1$OC=='open',1,0)

dum_test=model.matrix(~instkind+ownerChange,test)
colnames(dum_test)
colnames(test)
test = test[,-c(6,9)]
test =cbind(test,dum_test)
###
#sample2 =bimba::BDLSMOTE(OC~.,data =train1[,-1],perc_over = 800,k=5,classes = "Minority")
#sample2= bimba::ADASYN(OC~.data=train1[,-1],perc_over =800,k=5,classes=T)
#
library(smotefamily)

### use validation (to smote) #####################
str(all_data2)
colnames(train)
sapply(train,is.factor) %>% as.data.frame()

idx<- createDataPartition(train$OC, p = 0.3, list = FALSE)
val_train<-train[-idx,]
val_test<-train[idx,]

table(val_train$OC)
table(val_test$OC)

val_sample =SMOTE(OC~.,data =val_train, 
              perc.over=900 ,# 적은 쪽의 데이터를 얼마나 추가로 샘플링해야 하는지 (n+1배 )
              k=5           # 고려할 최근접 이웃의 수
              # 적은 쪽의 데이터를 추가로 샘플링할 때 각 샘플에 대응해서 많은 쪽의 데이터를
              # 얼마나 샘플링할지 지
)
table(val_sample$OC)
### 모델링 ##########################
train =all_data2[1:293,]
test =all_data2[294:420,]
train$OC<-target$x

##---
sapply(all_data, class) %>% as.data.frame()
str(val_train)

######################## XGB

#xgb factor to numeric
#preparing matrix 
set.seed(1234)

##1. 기존의 불균형한 val_train으로 
control <- trainControl(method="repeatedcv", 
                        number=10 ,
                        repeats=2,
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE
)

tune_grid <- expand.grid(nrounds = 300,
                         eta =0.05,
                         max_depth = c(4, 6, 8),
                         gamma = c(1, 3), 
                         subsample = 0.5,
                         min_child_weight = 1, 
                         colsample_bytree = 1
)
levels(val_train$OC) <- make.names(levels(factor(val_train$OC)))
str(val_train)
start<-Sys.time()
xgb_fit <- train(OC ~., data = val_train, 
                 method = "xgbTree",
                 objective = "binary:logistic",
                 #names(getModelInfo())
                 trControl=control,
                 tuneGrid = tune_grid
                 #preProcess =
                 #tuneLength = 10
                 )
end<-Sys.time()
end-start

a=predict(xgb_fit,val_test %>% select(-OC));
str(a)

###2. SMTOE: val_sample으로 
control <- trainControl(method="repeatedcv", 
                        number=10 ,
                        repeats=2,
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE
)

tune_grid <- expand.grid(nrounds = 300,
                         eta =0.05,
                         max_depth = c(4, 6, 8),
                         gamma = c(1, 3), 
                         subsample = 0.5,
                         min_child_weight = 1, 
                         colsample_bytree = 1
)
levels(val_sample$OC) <- make.names(levels(factor(val_sample$OC)))
str(val_sample)
start<-Sys.time()
xgb_fit <- train(OC ~., data = val_sample, 
                 method = "xgbTree",
                 objective = "binary:logistic",
                 #names(getModelInfo())
                 trControl=control,
                 tuneGrid = tune_grid
                 #preProcess =
                 #tuneLength = 10
)
end<-Sys.time()
end-start
a=predict(xgb_fit,val_test %>% select(-OC));

### confusion matrix 
str(a)
a<-revalue(a,c("X.close"="close"))
str(val_test$OC)
table(a,val_test$OC) %>% confusionMatrix() #기존 : 0.9326
                                           #SMOTE : 0.8989
#결과물 생성 
b=predict(xgb_fit, test, type="prob")
b
c=ifelse(b$X.close>=0.15,0,1) ##강제로 close를 배정 
table(c)
final =data.frame(cbind(test$inst_id,c))

final= data.frame(predict(xgb_fit,test))
table(final)
colnames(final)=c("inst_id","OC")
head(final)
write.csv(final,file="submission_926xgb_bang.csv",row.names = FALSE)

#변수 중요도 
xgb.importance(xgb_fit$coefnames,xgb_fit$finalModel)
varname =xgb.importance(xgb_fit$coefnames,xgb_fit$finalModel)$Feature[1:28]  #0.01이상 변수 추출 
varname
varImp(xgb_fit)
#recall = A/(A+C)
#precision = A/(A+B)
#F_i = (1+i^2)*prec*recall/((i^2 * precision)+recall)

table(a)
train1$OC
sum(ifelse(a==test_OC,1,0))

final=data.frame(predict(xgb_fit, test))
final <- ifelse(final$predict.xgb_fit..test.=="open",1,0)
final =data.frame(cbind(test$inst_id,final))
colnames(final)=c("inst_id","OC")
head(final,10)
table(final$OC)

write.csv(final,file="submission_919xgb_undersample.csv",row.names = FALSE)


#########################SVM #범주형 데이터가 서열일 경우 숫자로 변환해 사용, 아니면 dummy 사용
trainCtrl <- trainControl(method = "repeatedcv", repeats=2,number=5,classProbs =  TRUE)

   #sigmaRange <- sigest(as.matrix(train[cont][-72]))
str(train)
svm.Grid <- expand.grid(
  .sigma = seq(0.4,1,0.1),
  .C = 2^(seq(-5,5,2)))
levels(train$OC) <- make.names(levels(factor(train$OC)))
str(train)
svm_fit <- train(OC ~ .,
                   data = train[-1],
                   method = 'svmRadial',
                   tuneGrid = svm.Grid,
                   preProc = c('scale'),#YeoJohnson
                   #metric = 'spec',    
                   trControl = trainCtrl)
svm_fit
final=data.frame(predict(svm_fit, test))
table(final)
final <- ifelse(final$predict.xgb_fit..test.=="open",1,0)
final =data.frame(cbind(test$inst_id,final))
colnames(final)=c("inst_id","OC")
head(final,10)
table(final$OC)

write.csv(final,file="submission_919xgb_undersample.csv",row.names = FALSE)
#
CV_Folds <- createMultiFolds(train$OC, k = 10, times = 5)
svm_model <- train(OC~.,data =train ,method="svmlinear",tuneLength=5,
                 trControl=trainControl(method='repeatedCV',index=CV_Folds))

predict(svm_model,test)
predict(svm,test)
test$OC



#################################one-classification
str(train)
str(test)

set.seed(1234)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,sampling = "rose")

svm_Linear <- train(OC ~., data = train[,-1], method = "svmLinear",
                    trControl=control,
                    #preProcess = c("center"),
                    tuneLength = 10)
a=predict(svm_Linear,test)
table(a)

#
str(train)
grid_radial <- expand.grid(sigma = c(), C = c())
svm_Radial <- train(OC ~., data = train, 
                    method = "svmRadial",
                    trControl=control,
                    tuneGrid = grid_radial
                    #type='one-classification'
                    #nu=0.10
                    #preProcess = c("center", "scale"),
                    #tuneLength = 10
                    )
a=predict(svm_Radial,test)
table(a)


set.seed(1234)
nb_model <- naiveBayes(OC~.,data = train)
a=predict(nb_model,test);a
table(a)


final=data.frame(predict(svm_Linear, test));final;table(final$predict.svm_Linear..test.)
final=ifelse(final$predict.svm_Linear..test.=="open",1,0)
final =data.frame(cbind(test$inst_id,final))
colnames(final)=c("inst_id","OC")
head(final)
write.csv(final,file="submission_920_1_svmline.csv",row.names = FALSE)

table(final$OC)
###
###확인해보기 
test = train[-idx,]
final$OC
table(test$OC)


######################## rf
control <- trainControl(method="repeatedcv", number=5, repeats=2)
#metric <- "Sensitivity"
mtry <- sqrt(ncol(sample))
tunegrid <- expand.grid(.mtry=mtry)
rf_fit <- train(OC~., data=sample, method="rf", tuneGrid=tunegrid, trControl=control
                #metric=metric
                )
final=data.frame(predict(rf_fit, test))
table(final)

final=ifelse(final$predict.rf_fit..test.=="open",1,0)
test1=read.csv("test.csv")
final =data.frame(cbind(test1$inst_id,final))
colnames(final)=c("inst_id","OC")
head(final)
write.csv(final,file="submission_928_rf_sample2.csv",row.names = FALSE)
final
###Isolation Forest
