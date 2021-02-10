#anova검정으로 업종별로 매출액 다른지 확인해보기
str(merge_data)
ggplot(merge_data, aes(x = factor(서비스_업종_코드_명)), y=분기_매출_금액, fill = factor(서비스_업종_코드_명))+
  geom_boxplot()
merge_data <- read.csv("merge_data.csv")

tapply(merge_data$분기_매출_금액, merge_data$서비스_업종_코드_명, mean)
shapiro.test(merge_data$분기_매출_금액) #정규성 X -> 중심극한정리로 패스
bartlett.test(분기_매출_금액~as.factor(서비스_업종_코드_명), data = merge_data) #등분산성 x
oneway.test(분기_매출_금액~as.factor(서비스_업종_코드_명), data = merge_data, var.equal = F) #적어도 하나의 집단 다르다
a1<-aov(분기_매출_금액~as.factor(서비스_업종_코드_명), data = merge_data)
library(laercio)
LDuncan(a1, "group")
TukeyHSD(aov(분기_매출_금액~as.character(서비스_업종_코드_명),data=merge_data))
plot(TukeyHSD(aov(분기_매출_금액~as.character(서비스_업종_코드_명),data=merge_data)))



shapiro.test(merge_data$분기_매출_금액) #정규성 X -> 중심극한정리로 패스
bartlett.test(분기_매출_금액~as.factor(상권명), data = merge_data) #등분산성 x
oneway.test(분기_매출_금액~as.factor(상권명), data = merge_data, var.equal = F) #적어도 하나의 집단 다르다

a1<-aov(분기_매출_금액~as.factor(상권명), data = merge_data)
LDuncan(a1, "group")

TukeyHSD(aov(분기_매출_금액~as.character(상권명),data=merge_data))
plot(TukeyHSD(aov(분기_매출_금액~as.character(상권명),data=merge_data)))
