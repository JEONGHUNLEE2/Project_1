#대학별_매출에서_연령대_비율
sales <- read.csv("추정매출_train.csv")
save.image("대학별_매출에대한_연령대_비율.RData")
rm(list= ls())

#"사당로23길","사당로23나길","서달로10길","강남시장_동작"

sales <-  sales %>% filter(상권_코드_명 != "사당로23나길")

#점포수 10개 미만 상권 제거
c("강남시장_동작","서달로10길","사당로23길","사당로23나길")

str(sales)
colnames(sales)

data <- sales[,c(1:8,51:56)]
data <- sales

data$연령대_60_이상_매출_금액 <- as.integer(data$연령대_60_이상_매출_금액)
data <-data %>% select(-상권_구분_코드,-상권_구분_코드_명)
str(data)
str(merge_data)
merge_data <- merge_data %>% select(-X)
data <- data %>% select(-X)

data <- merge(merge_data, data,
      by.all = c("기준_년_코드","기준_분기_코드","상권_코드","상권_코드_명","서비스_업종_코드","서비스_업종_코드_명"),
      )
str(data)
View(data)
library(tidyverse)
colnames(data)
data <- data[,c(1:9,57,61:66)] %>% 
  gather("연령대","매출금액", 연령대_10_매출_금액:연령대_60_이상_매출_금액) %>% 
  group_by(기준_년_코드, 기준_분기_코드)
data <- as.data.frame(data)
str(data)
#---문제의 구간
data %>% group_by(년도_분기= paste(기준_년_코드, 기준_분기_코드), 연령대) %>% 
  summarise(sum = sum(매출금액)) %>% mutate(total_sum = sum(sum))

test_data %>% group_by(년도_분기= paste(기준_년_코드, 기준_분기_코드),서비스_업종_코드_명) %>%
  summarise(sum = sum(분기_매출_금액)) %>% mutate(total_sum = sum(sum))

merge_data%>% group_by(년도_분기= paste(기준_년_코드, 기준_분기_코드),서비스_업종_코드_명) %>%
  summarise(sum = sum(분기_매출_금액))
#----------

str(test_data)

data <- as.data.frame(data)

View(data)
data2<-data %>% group_by(분기별=paste(기준_년_코드, 기준_분기_코드), 상권명, 연령대) %>% 
  summarise(연령별_매출금액 = sum(매출금액))


data %>% summarise(sum = sum(매출금액))

data2 <- as.data.frame(data2)
data2$연령별_매출금액 <- as.integer(data2$연령별_매출금액)

str(data2)

data2 %>% group_by(상권명, 연령대) %>% 
  summarise(총_매출금액 = sum(연령별_매출금액)) %>% 
  mutate(상권별_매출금액  = sum(총_매출금액)) %>% 
  mutate(매출금액_비율 = round(총_매출금액 / 상권별_매출금액,3)*100) %>% 
  ggplot(aes(x=상권명, y = 총_매출금액, fill = 연령대))+
  geom_bar(stat="identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(매출금액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("상권별_매출금액_연령대_비율")+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  theme(plot.title = element_text(family = "serif", face = "bold",  size = 18, color = "darkgreen"))+
  theme(axis.title=element_text(size=15),title = element_text(size=13))
  

