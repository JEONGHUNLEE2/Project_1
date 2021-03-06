#train_data로 시각화 
total_merge_data <- merge_data
merge_data <- train_merge_data
save.image("EDA작업_최종.RData")
library(tidyverse)
rm(list=ls())
total_merge_data %>% filter(기준_년_코드 == 2020, 기준_분기_코드 == 3)
str(merge_data)
merge_data <- merge_data %>% mutate(분기_평균매출금액 = 분기_매출_금액 / 점포_수)
merge_data <- merge_data %>% select(-분기_매출_금액)
merge_data <- rename(merge_data, "분기_매출_금액" = "분기_평균매출금액")

#매출액 추이 
library(tidyverse)

str(merge_data)
merge_data %>%  
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드), 상권명) %>% 
  summarise(매출액 = sum(분기_매출_금액/점포_수)) %>% 
  ggplot(aes(x = 연도_분기별, y = 매출액, color = 상권명 ,group = 상권명))+
  geom_line(size = 2)+
  scale_y_continuous(label = scales::comma)+
  geom_point(size = 2)+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  ggtitle("대학상권별_매출액_변화추이")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkgreen"))
  
merge_data %>% group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드), 상권명) %>% 
  summarise(매출액 = sum(분기_매출_금액/점포_수),
               전체매출액 = sum(분기_매출_금액),
               점포_수 = sum(점포_수),
               평균매출액 = sum(분기_매출_금액)/sum(점포_수))
str(merge_data)
summary(merge_data)
View(merge_data)
View(merge_data %>% filter(기준_년_코드 == 2015))

#총신대만 자세히 
merge_data %>% filter(상권명 == "총신대") %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드), 상권명) %>% 
  summarise(매출액 = sum(분기_매출_금액)) %>% 
  ggplot(aes(x = 연도_분기별, y = 매출액, color = 상권명 ,group = 상권명))+
  geom_line(size = 2, color = "darkgreen")+
  scale_y_continuous(label = scales::comma)+
  geom_point(size = 3, color = "darkgreen")+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  ggtitle("대학상권별_매출액_변화추이")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkgreen"))

merge_data %>% filter(상권명 %in% c("중앙대","숭실대")) %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드), 상권명) %>% 
  summarise(매출액 = sum(분기_매출_금액)) %>% 
  ggplot(aes(x = 연도_분기별, y = 매출액, color = 상권명 ,group = 상권명))+
  geom_line(size = 2)+
  scale_y_continuous(label = scales::comma)+
  geom_point(size = 3)+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  ggtitle("대학상권별_매출액_변화추이")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkgreen"))

str(merge_data)
#매출_연령대
str(sales)
str(data)
data2<-data[,c(1:6,55,57:62)] %>% 
  gather("연령대","매출금액", 연령대_10_매출_금액:연령대_60_이상_매출_금액) %>% 
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드))

str(data2)
data2 <- as.data.frame(data2)
data2$기준_년_코드 <- as.factor(data2$기준_년_코드)
data2$기준_분기_코드 <- as.factor(data2$기준_분기_코드)

data_c <- data2 %>% filter(상권명 == "총신대") %>% 
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),연령대) %>% 
  summarise(연령별_매출금액 = sum(매출금액)) %>% mutate(전체_매출금액 = sum(연령별_매출금액))
str(data_j)
data_j <- as.data.frame(data_j)
data_s <- as.data.frame(data_s)
data_c <- as.data.frame(data_c)

data_c %>% group_by(분기별,연령대) %>% 
  summarise(매출금액 = sum(연령별_매출금액)) %>% 
  mutate(전체_매출금액 = sum(매출금액)) %>% 
  mutate(매출금액_비율 = round(매출금액 /전체_매출금액,3)*100) %>% 
  ggplot(aes(x=분기별, y = 매출금액, fill = 연령대))+
  geom_bar(stat="identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(매출금액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("총신대_매출금액_연령대_비율")+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  theme(plot.title = element_text(family = "serif", face = "bold",  size = 18, color = "darkgreen"))+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

#유동인구_연령별-------------------------------------------------------------------------
str(merge_data)
colnames(merge_data)
data_pop <- merge_data[,c(1:6,14:19,57)]
str(data_pop)

merge_data %>% filter(상권명 =="숭실대") %>% 
  gather("연령대","유동인구", 연령대_10_유동인구_수:연령대_60_이상_유동인구_수) %>% 
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),연령대,상권_코드_명) %>% 
  summarise(상권별_유동인구 = mean(유동인구)) %>% 
  group_by(분기별,연령대) %>% 
  summarise(유동인구 = sum(상권별_유동인구)) %>% 
  mutate(전체_유동인구 = sum(유동인구)) %>% 
  mutate(유동인구_비율 = round(유동인구 /전체_유동인구,3)*100) %>% 
  ggplot(aes(x=분기별, y = 유동인구, fill = 연령대))+
  geom_bar(stat="identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(유동인구_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("숭실대_유동인구_연령대_비율")+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  theme(plot.title = element_text(family = "serif", face = "bold",  size = 18, color = "darkgreen"))+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

View(merge_data %>% filter(상권명 == "중앙대"))

#상주인구
data_repop <- merge_data[,c(1:6,36:42,57)]
str(merge_data)
colnames(merge_data)
View(data_repop %>% filter(상권명 =="중앙대"))
str(merge_data)

merge_data %>% filter(상권명 =="총신대") %>% 
  gather("연령대","상주인구", 연령대.10.상주인구수:연령대.60.이상.상주인구수) %>% 
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),연령대,상권_코드_명) %>% 
  summarise(상권별_상주인구 = mean(상주인구)) %>% 
  group_by(분기별,연령대) %>% 
  summarise(상주인구 = sum(상권별_상주인구)) %>% 
  mutate(전체_상주인구 = sum(상주인구)) %>% 
  mutate(상주인구_비율 = round(상주인구 /전체_상주인구,3)*100) %>% 
  ggplot(aes(x=분기별, y = 상주인구, fill = 연령대))+
  geom_bar(stat="identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(상주인구_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("총신대_상주인구_연령대_비율")+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  theme(plot.title = element_text(family = "serif", face = "bold",  size = 18, color = "darkgreen"))+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

#서비스 업종비율_매출액
str(merge_data)


merge_data %>% filter(상권명 == "중앙대") %>% 
  filter(기준_년_코드 ==2020) %>% 
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),서비스_업종_코드_명) %>% 
  summarise(매출금액 = sum(분기_매출_금액/점포_수)) %>% 
  mutate(전체_매출금액 = sum(매출금액))  %>% 
  mutate(매출금액_비율 = round(매출금액 /전체_매출금액,3)*100) %>% 
  ggplot(aes(x=분기별, y = 매출금액, fill = 서비스_업종_코드_명))+
  geom_bar(stat="identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(매출금액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Spectral")+
  ggtitle("중앙대_매출금액_업종_비율")+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  theme(plot.title = element_text(family = "serif", face = "bold",  size = 18, color = "darkgreen"))+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

#서비스 업종비율_점포수
str(merge_data)
merge_data %>% filter(상권명 == "총신대") %>% 
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),서비스_업종_코드_명)%>%
  summarise(점포_수= sum(점포_수)) %>% 
  mutate(전체_점포수 = sum(점포_수))  %>% 
  mutate(점포_비율 = round(점포_수 /전체_점포수,3)*100) %>% 
  ggplot(aes(x=분기별, y = 점포_수, fill = 서비스_업종_코드_명))+
  geom_bar(stat="identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(점포_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Spectral")+
  ggtitle("총신대_서비스업종_비율")+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  theme(plot.title = element_text(family = "serif", face = "bold",  size = 18, color = "darkgreen"))+
  theme(axis.title=element_text(size=15),title = element_text(size=13))



str(total_merge_data)
merge_data %>% filter(상권명 == "중앙대") %>% 
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),서비스_업종_코드_명)%>% 
  summarise(sum = sum(분기_매출_금액))  

data %>%  filter(상권명 =="중앙대") %>% group_by(년도_분기= paste(기준_년_코드, 기준_분기_코드),서비스_업종_코드_명) %>%
  summarise(sum = sum(분기_매출_금액)) %>% mutate(전체 = sum(sum))
#연령대...가 문제인가
#---문제의 구간
data %>% group_by(년도_분기= paste(기준_년_코드, 기준_분기_코드), 연령대) %>% 
  summarise(sum = sum(매출금액)) %>% mutate(total_sum = sum(sum))

test_data %>% group_by(년도_분기= paste(기준_년_코드, 기준_분기_코드),서비스_업종_코드_명) %>%
  summarise(sum = sum(분기_매출_금액)) %>% mutate(total_sum = sum(sum))

merge_data%>% group_by(년도_분기= paste(기준_년_코드, 기준_분기_코드),서비스_업종_코드_명) %>%
  summarise(sum = sum(분기_매출_금액))
#----------연령대로 나눈다음 총매출 y축이랑 서비스업종 구분으로 나눴을 때... 오차가 있음..


#total_merge_Data
total_merge_data <- total_merge_data %>% mutate(분기_평균매출금액 = round(분기_매출_금액 / 점포_수))
View(total_merge_data[,c(1:9,52,61)])
colnames(total_merge_data)
str(total_merge_data)
merge_data <- total_merge_data[,c(1:8,61,9:60)]
str(merge_data)
#-------------------------------------------------------------------------
#----------------------
merge_data %>% filter(상권명 =="중앙대") %>% 
  gather("연령대","유동인구", 연령대_10_유동인구_수:연령대_60_이상_유동인구_수) %>% 
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),연령대,상권_코드_명) %>% 
  summarise(상권별_유동인구 = mean(유동인구)) %>% 
  group_by(분기별,연령대) %>% 
  summarise(유동인구 = sum(상권별_유동인구)) %>% 
  mutate(전체_유동인구 = sum(유동인구)) %>% 
  mutate(유동인구_비율 = round(유동인구 /전체_유동인구,3)*100) %>% 
  group_by(연령대) %>% 
  summarise(mean = mean(유동인구_비율),
            sum = mean(전체_유동인구),
            mean2 = mean(유동인구))
#----------------------
merge_data %>% filter(상권명 =="총신대") %>% 
  gather("연령대","상주인구", 연령대.10.상주인구수:연령대.60.이상.상주인구수) %>% 
  group_by(분기별=paste(기준_년_코드, 기준_분기_코드),연령대,상권_코드_명) %>% 
  summarise(상권별_상주인구 = mean(상주인구)) %>% 
  group_by(분기별,연령대) %>% 
  summarise(상주인구 = sum(상권별_상주인구)) %>% 
  mutate(전체_상주인구 = sum(상주인구)) %>% 
  mutate(상주인구_비율 = round(상주인구 /전체_상주인구,3)*100) %>% 
  group_by(연령대) %>% 
  summarise(mean = mean(상주인구_비율),
            sum = mean(전체_상주인구),
            mean2 = mean(상주인구))


ggplot(aes(x=분기별 , y = 상주인구, fill = 연령대))+
  geom_bar(stat="identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(상주인구_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("총신대_상주인구_연령대_비율")+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  theme(plot.title = element_text(family = "serif", face = "bold",  size = 18, color = "darkgreen"))+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

