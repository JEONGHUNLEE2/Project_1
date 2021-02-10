#모델작업
save.image("model최종작업_20200126.RData")
rm(list = ls())
library(tidyverse)
train_data <- read.csv("train2015.csv")
str(train_data)
#상권_코드, 서비스_업종코드 제거
train_data <- train_data %>% select(-상권_코드, -서비스_업종_코드)
train_data$기준_년_코드<-as.factor(train_data$기준_년_코드)
train_data$기준_분기_코드<-as.factor(train_data$기준_분기_코드)


df<-read.csv("train_mod.csv")
str(df)
#평균_매출금액으로 돌려봤을 떄 
train_data <- train_data %>% mutate(분기_평균매출금액 = 분기_매출_금액 / 점포_수)
colnames(train_data)

train_data <- train_data %>% select(-분기_매출_건수, -점포_수)
train_data <- train_data %>% select(-분기_매출_금액)

str(train_data)
train_data <- train_data[,c(1:6,54,7:53)]

#상관관계
cor_data<-train_data[,-c(1:6,54)]
str(cor_data)
cor <- map_lgl(.x = cor_data, .f = function(x) {
  test<- cor.test(x= x,y= train_data$분기_평균매출금액)
  result<- test$p.value>0.05
  return(result)
})
cor

#필요없는 컬럼 제거 
train_data <- train_data %>% select(-c(연령대_10_유동인구_수,연령대_60_이상_유동인구_수,시간대_00_06_유동인구_수,시간대_06_11_유동인구_수,
                             시간대_21_24_유동인구_수,토요일_유동인구_수,일요일_유동인구_수,총.상주인구.수,남성.상주인구.수,여성.상주인구.수,
                             연령대.40.상주인구수,연령대.50.상주인구수,연령대.60.이상.상주인구수,총_직장_인구_수,남성_직장_인구_수,여성_직장_인구_수,연령대_10_직장인구_수,
                             연령대_20_직장인구_수,연령대_30_직장인구_수,연령대_40_직장인구_수,연령대_50_직장인구_수,폐업_점포_수))
library(tidyverse)
real_data <- real_data %>% select(-c(연령대_10_유동인구_수,연령대_60_이상_유동인구_수,시간대_00_06_유동인구_수,시간대_06_11_유동인구_수,
                                           시간대_21_24_유동인구_수,토요일_유동인구_수,일요일_유동인구_수,총.상주인구.수,남성.상주인구.수,여성.상주인구.수,
                                           연령대.40.상주인구수,연령대.50.상주인구수,연령대.60.이상.상주인구수,총_직장_인구_수,남성_직장_인구_수,여성_직장_인구_수,연령대_10_직장인구_수,
                                           연령대_20_직장인구_수,연령대_30_직장인구_수,연령대_40_직장인구_수,연령대_50_직장인구_수,폐업_점포_수))
str(real_data)
colnames(df)
colnames(train_data)

#범주형 변수들 ANOVA로 확인 
#서비스_업종_명
shapiro.test(train_data$분기_평균매출금액) #정규성 X -> 중심극한정리로 패스
bartlett.test(분기_평균매출금액~as.factor(서비스_업종_코드_명), data = train_data) #등분산성 x
oneway.test(분기_평균매출금액~as.factor(서비스_업종_코드_명), data = train_data, var.equal = F) #적어도 하나의 집단 다르다
a1<-aov(분기_평균매출금액~as.factor(서비스_업종_코드_명), data = train_data)
library(laercio)
LDuncan(a1, "group")
TukeyHSD(aov(분기_평균매출금액~as.character(서비스_업종_코드_명),data=train_data))
plot(TukeyHSD(aov(분기_평균매출금액~as.character(서비스_업종_코드_명),data=train_data)))

#상권명
shapiro.test(train_data$분기_평균매출금액) #정규성 X -> 중심극한정리로 패스
bartlett.test(분기_평균매출금액~as.factor(상권명), data = train_data) #등분산성 x
oneway.test(분기_평균매출금액~as.factor(상권명), data = train_data, var.equal = F) #적어도 하나의 집단 다르다
a1<-aov(분기_평균매출금액~as.factor(상권명), data = train_data)
library(laercio)
LDuncan(a1, "group")
TukeyHSD(aov(분기_평균매출금액~as.character(서비스_업종_코드_명),data=train_data))
plot(TukeyHSD(aov(분기_평균매출금액~as.character(서비스_업종_코드_명),data=train_data)))

#여기서부터 train_data 를 컬럼 축소해보기 위해 test_data 생성
test_data <- train_data

#다중선형회귀분석 
str(test_data)

full <- lm(분기_평균매출금액 ~., data = test_data)
null <- lm(분기_평균매출금액~ 1, data = test_data)
fit1<- step(null, 
            scope = list(lower = null, upper = full),
            direction = 'both')
summary(fit1)
library(car)
vif(fit1) 
test_data <- test_data %>% select(-수요일_유동인구_수)
test_data <- train_data
#10이상에서 가장 높은거 하나씩 변수 하나씩 제거해보기 1. 총_유동인구_수, 2. 수요일 유동인구수...이건 좀 아닌거같음

library(MLmetrics)
real_data <- read.csv("test2015.csv") 
real_data <- real_data %>% mutate(분기_평균매출금액 = 분기_매출_금액 / 점포_수)

real <- real_data$분기_평균매출금액
pred1 <-predict(fit1, newdata = test_data, type = 'response')

RMSE(real, pred1)

mean(real_data$분기_평균매출금액)
mean(train_data$분기_평균매출금액)

#의사결정나무
library(rpart)
Ctrl <- rpart.control(minsplit = 20,
                      cp = 0.01,
                      maxdepth = 10)
set.seed(seed = 1234)
fit2 <- rpart(formula = 분기_평균매출금액~.,
              data = test_data,
              control = Ctrl)
summary(fit2)
printcp(fit2)
#cp가..뭐였더라..-> 가지치기 하는 기준정하는!
library(rpart.plot)
rpart.plot(fit2, 
           type = 2,
           extra = 101,
           fallen.leaves = FALSE)
plotcp(fit2)

pred2 <- predict(object = fit2, newdata = test_data, type = 'vector')
RMSE(real, pred2)

#랜덤포레스트 
library(randomForest)
set.seed(seed = 1234)
fit3 <- randomForest(formula = 분기_평균매출금액 ~.,
                     data = test_data,
                     ntree = 1000,
                     mtry = 3,
                     importance = TRUE,
                     do.trace = 50,
                     keep.forest = TRUE
)
print(fit3)
importance(fit3, type = 1) #변수중요도 확인
varImpPlot(fit3, main = 'Variable Importance', type = 1)

pred3 <-predict(fit3, newdata = test_data, type = 'response')

RMSE(real, pred3)



#총신대 뺴고 돌려보기 --------------------------------------------------------
test_data2 <- train_data %>% filter(상권명 %in% c("중앙대","숭실대"))
real2 <- real_data %>% filter(상권명 %in% c("중앙대","숭실대"))
real2 <- real2 %>% mutate(분기_평균매출금액 = 분기_매출_금액 / 점포_수)
real2 <- real_data$분기_평균매출금액

#다중선형회귀분석 
str(test_data)

full <- lm(분기_평균매출금액 ~., data = test_data2)
null <- lm(분기_평균매출금액~ 1, data = test_data2)
fit1<- step(null, 
            scope = list(lower = null, upper = full),
            direction = 'both')
summary(fit1)
library(car)
vif(fit1) 
test_data <- test_data %>% select(-수요일_유동인구_수)
test_data <- train_data

library(MLmetrics)
real_data <- read.csv("test2015.csv") 
real_data <- real_data %>% mutate(분기_평균매출금액 = 분기_매출_금액 / 점포_수)

real <- real_data$분기_평균매출금액
pred1 <-predict(fit1, newdata = test_data2, type = 'response')

RMSE(real2, pred1)

mean(real_data$분기_평균매출금액)
mean(test_data2$분기_평균매출금액)

#의사결정나무
library(rpart)
Ctrl <- rpart.control(minsplit = 20,
                      cp = 0.01,
                      maxdepth = 10)
set.seed(seed = 1234)
fit2 <- rpart(formula = 분기_평균매출금액~.,
              data = test_data2,
              control = Ctrl)
summary(fit2)
printcp(fit2)
#cp가..뭐였더라..-> 가지치기 하는 기준정하는!
library(rpart.plot)
rpart.plot(fit2, 
           type = 2,
           extra = 101,
           fallen.leaves = FALSE)
plotcp(fit2)

pred2 <- predict(object = fit2, newdata = test_data2, type = 'vector')
RMSE(real2, pred2)

#랜덤포레스트 
library(randomForest)
set.seed(seed = 1234)
fit3 <- randomForest(formula = 분기_평균매출금액 ~.,
                     data = test_data2,
                     ntree = 1000,
                     mtry = 3,
                     importance = TRUE,
                     do.trace = 50,
                     keep.forest = TRUE
)
print(fit3)
importance(fit3, type = 1) #변수중요도 확인
varImpPlot(fit3, main = 'Variable Importance', type = 1)

pred3 <-predict(fit3, newdata = test_data2, type = 'response')

RMSE(real2, pred3)

#결과 총신대를 빼고 돌리니까 성능이 더 안좋았음. 
#컬럼 축소해서 해보자
str(test_data)
test_data <- test_data %>% select(-총_유동인구_수)

#------------------두리님 모델
df<-read.csv("train_mod.csv")
str(df)
library(rpart)
ctrl<- rpart.control(minsplit = 10,
                     cp = 0.001,
                     maxdepth = 30)


fit1<-rpart(formula = (분기_매출_금액/점포_수)~.,
            data = df,
            control = ctrl)
summary(fit1)
test_mod <- read.csv("test_mod.csv")
real<- (df$분기_매출_금액/df$점포_수)
pred1<- predict(fit1, newdata = test_mod, type = 'vector')
source("myFuns.R")
regMeasure(real, pred1)

str(test_data)
str(df)

fit1<-rpart(formula = 분기_평균매출금액~.,
            data = test_data,
            control = ctrl)

pred1 <- predict(fit1, newdata = , type='vector')

str(df)

str(test_data)
colnames(test_data)
real_data2 <- read.csv("train2015.csv")
colnames(real_data2)
colnames(real_data)
library(tidyverse)
c_train_data <- train_data %>% filter(상권명 %in%c("중앙대","숭실대"))
d_train_data <- train_data %>% filter(상권명 %in%c("총신대"))

str(train_data)
#-----------------------
library(randomForest)
set.seed(seed = 1234)
fit4 <- randomForest(formula = 분기_평균매출금액 ~.,
                     data = train_data,
                     ntree = 1000,
                     mtry = 10,
                     importance = TRUE,
                     do.trace = 50,
                     keep.forest = TRUE
)
print(fit4)
importance(fit4, type = 1) #변수중요도 확인
varImpPlot(fit4, main = 'Variable Importance', type = 1)

