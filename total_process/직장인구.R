#직장인구
rm(list = ls())

save.image("직장인구_코드.RData")

data <- as.data.frame(data)
data <-merge_data %>% filter(기준_년_코드 == 2020, 기준_분기_코드 == 2)  
str(data)

data <- merge_data[,c(1:6,54,grep("직장인구",names(merge_data)))] %>% 
  gather("연령대","직장인구", 연령대_10_직장인구_수:연령대_60_이상_직장인구_수) %>% 
  group_by(기준_년_코드, 기준_분기_코드)
str(data)

data2<-data %>% group_by(상권_코드_명, 상권명, 연령대) %>% 
  summarise(n_dist = n_distinct(서비스_업종_코드), 연령별_직장인구 = sum(직장인구)/n_dist)

data2 %>% group_by(상권명, 연령대) %>% 
  summarise(직장인구 = sum(연령별_직장인구)) %>% 
  mutate(전체직장인구_수 = sum(직장인구)) %>% 
  mutate(직장인구_비율 = round(직장인구 / 전체직장인구_수,3)*100) %>% 
  ggplot(aes(x=상권명, y = 직장인구, fill = 연령대))+
  geom_bar(stat="identity")+
  geom_text(stat = "sum",aes(label = paste0(직장인구_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("상권별_직장인구_비율")+
  theme(axis.title = element_text(face = "bold", size = 17, color = "darkgreen"))+
  theme(plot.title = element_text(family = "serif", face = "bold",  size = 18, color = "darkgreen"))+
  theme(axis.title=element_text(size=15),title = element_text(size=13))

data2
View(data2 %>%  filter(상권명 == "숭실대") %>% summarise(sum = sum(연령별_직장인구)))
