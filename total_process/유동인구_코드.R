#유동인구_관련 코드
save.image("유동인구_코드.RData")

#유동인구수_성별
str(merge_data)
merge_data %>% filter(상권명 == "중앙대") %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드),상권_코드_명) %>% 
  summarise(n = n(), 상권별_유동인구 = sum(총_유동인구_수)/n) %>%
  group_by(연도_분기별) %>% 
  summarise(유동인구_수 = sum(상권별_유동인구)) %>% 
  ggplot(aes(x = 연도_분기별, y = 유동인구_수, group = 1)) +
  geom_line(size = 1, color = "darkgreen")+
  scale_y_continuous(label = scales::comma)+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  ggtitle("중앙대_유동인구_수_변화추이")+
  theme(plot.title = element_text(family = "serif", face = "bold",  size = 20, color = "darkgreen"))+
  geom_line(data = male_data, mapping = aes(x = 연도_분기별, y = 유동인구_수, group = 1), size = 1,color = "blue")+
  geom_line(data = female_data, mapping = aes(x = 연도_분기별, y = 유동인구_수, group = 1), size = 1,color = "red")+
  coord_cartesian(ylim=c(0,2500000)) 

#남성
male_data<-merge_data %>% filter(상권명 == "중앙대") %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드),상권_코드_명) %>% 
  summarise(n = n(), 상권별_유동인구 = sum(남성_유동인구_수)/n) %>%
  group_by(연도_분기별) %>% 
  summarise(유동인구_수 = sum(상권별_유동인구))
#여성
female_data<-merge_data %>% filter(상권명 == "중앙대") %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드),상권_코드_명) %>% 
  summarise(n = n(), 상권별_유동인구 = sum(여성_유동인구_수)/n) %>%
  group_by(연도_분기별) %>% 
  summarise(유동인구_수 = sum(상권별_유동인구))


#유동인구_연령대
str(merge_data)
merge_data %>% filter(상권명 == "중앙대") %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드),상권_코드_명) %>% 
  summarise(n = n(), 상권별_유동인구 = sum(총_유동인구_수)/n) %>%
  group_by(연도_분기별) %>% 
  summarise(유동인구_수 = sum(상권별_유동인구)) %>% 
  ggplot(aes(x = 연도_분기별, y = 유동인구_수, group = 1)) +
  geom_line(size = 1, color = "darkgreen")+
  scale_y_continuous(label = scales::comma)+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  ggtitle("중앙대_유동인구_수_변화추이")+
  theme(plot.title = element_text(family = "serif", face = "bold",  size = 20, color = "darkgreen"))+
  geom_line(data = age_10, mapping = aes(x = 연도_분기별, y = 유동인구_수, group = 1), size = 1,color = "blue")+
  geom_line(data = age_20, mapping = aes(x = 연도_분기별, y = 유동인구_수, group = 1), size = 1,color = "red")+
  geom_line(data = age_30, mapping = aes(x = 연도_분기별, y = 유동인구_수, group = 1), size = 1,color = "green")+
  geom_line(data = age_40, mapping = aes(x = 연도_분기별, y = 유동인구_수, group = 1), size = 1,color = "darkgray")+
  geom_line(data = age_50, mapping = aes(x = 연도_분기별, y = 유동인구_수, group = 1), size = 1,color = "orange")+
  geom_line(data = age_50, mapping = aes(x = 연도_분기별, y = 유동인구_수, group = 1), size = 1,color = "orange")+
  geom_line(data = age_60, mapping = aes(x = 연도_분기별, y = 유동인구_수, group = 1), size = 1,color = "purple")

age_10 <- merge_data %>% filter(상권명 == "중앙대") %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드), 상권_코드_명) %>% 
  summarise(n = n(),연령대10대_유동인구 = sum(연령대_10_유동인구_수)/n) %>% 
  group_by(연도_분기별) %>% 
  summarise(유동인구_수 = sum(연령대10대_유동인구))

age_20 <- merge_data %>% filter(상권명 == "중앙대") %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드), 상권_코드_명) %>% 
  summarise(n = n(),연령대20대_유동인구 = sum(연령대_20_유동인구_수)/n) %>% 
  group_by(연도_분기별) %>% 
  summarise(유동인구_수 = sum(연령대20대_유동인구))

age_30 <- merge_data %>% filter(상권명 == "중앙대") %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드), 상권_코드_명) %>% 
  summarise(n = n(),연령대30대_유동인구 = sum(연령대_30_유동인구_수)/n) %>% 
  group_by(연도_분기별) %>% 
  summarise(유동인구_수 = sum(연령대30대_유동인구))

age_40 <- merge_data %>% filter(상권명 == "중앙대") %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드), 상권_코드_명) %>% 
  summarise(n = n(),연령대40대_유동인구 = sum(연령대_40_유동인구_수)/n) %>% 
  group_by(연도_분기별) %>% 
  summarise(유동인구_수 = sum(연령대40대_유동인구))

age_50 <- merge_data %>% filter(상권명 == "중앙대") %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드), 상권_코드_명) %>% 
  summarise(n = n(),연령대50대_유동인구 = sum(연령대_50_유동인구_수)/n) %>% 
  group_by(연도_분기별) %>% 
  summarise(유동인구_수 = sum(연령대50대_유동인구))

age_60 <- merge_data %>% filter(상권명 == "중앙대") %>% 
  group_by(연도_분기별 = paste(기준_년_코드, 기준_분기_코드), 상권_코드_명) %>% 
  summarise(n = n(),연령대60대_유동인구 = sum(연령대_60_이상_유동인구_수)/n) %>% 
  group_by(연도_분기별) %>% 
  summarise(유동인구_수 = sum(연령대60대_유동인구))

#시간대별 
str(merge_data)
data <- merge_data[,c(1:6,54,grep("시간대",names(merge_data)))] %>%
  gather("시간대","유동인구", 시간대_00_06_유동인구_수:시간대_21_24_유동인구_수) %>% 
  group_by(기준_년_코드, 기준_분기_코드)

data <- as.data.frame(data)

data %>% filter(상권명 =="중앙대") %>% 
  group_by(기준_년_코드, 기준_분기_코드, 상권_코드_명,시간대) %>% summarise(n = n(),유동인구_수 = sum(유동인구)/n) %>% 
  group_by(시간대) %>% summarise(유동인구_수 = sum(유동인구_수)) %>% 
  ggplot(aes(x = 시간대, y = 유동인구_수, fill = 시간대))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  scale_fill_brewer(palette="Pastel1")+
  theme(legend.position = "none")+
  coord_cartesian(ylim=c(0,3500000)) 
