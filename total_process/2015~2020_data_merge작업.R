#merge_data 
rm(list = ls())
save.image("merge작업_20200121.RData")

#추정매출
sales_2015<-read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2015.csv")
sales_2016<-read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2016.csv")
sales_2017<-read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2017.csv")
sales_2018<-read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2018.csv")
sales_2019<-read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2019.csv")
sales_2020<-read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2020.csv")

sales <- rbind(sales_2015,sales_2016,sales_2017,sales_2018,sales_2019,sales_2020)

sales<-sales%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753),
              substr(서비스_업종_코드,1,3)=='CS1')

sales <- sales %>% select(-c(주중_매출_비율:연령대_60_이상_매출_비율,주중_매출_건수:점포수))
sales <- sales %>% select(-c(주중_매출_금액:연령대_60_이상_매출_금액))
str(sales)

                 
#유동인구
pop<-read.csv("서울시 우리마을가게 상권분석서비스(상권-추정유동인구).csv")
pop<-pop%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753),기준.년코드 >= 2015)
pop <- pop %>% select(-c(남성연령대_10_월요일시간대_1_유동인구_수:여성연령대_60_이상_일요일시간대_6_유동인구_수))
str(pop)
colnames(pop)
str(merge_data)

#상주인구
repop<-read.csv("서울시 우리마을가게 상권분석서비스(상권_상주인구).csv")
repop<-repop%>%
  filter(상권.코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753),기준_년_코드 >= 2015)
repop <- repop %>% select(-c(남성연령대.10.상주인구.수:비.아파트.가구.수))
str(repop)
repop <- repop[,c(1:5,15,6:14)]

#직장인구
wkpop <- read.csv("서울시 우리마을가게 상권분석서비스(상권-직장인구).csv")
wkpop<-wkpop%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753),
              기준_년월_코드 >= 2015)
str(wkpop)
wkpop <- wkpop %>% select(-c(남성연령대_10_직장_인구_수:여성연령대_60_이상_직장_인구_수))


#집객시설
facility<-read.csv("서울시 우리마을가게 상권분석서비스(상권-집객시설).csv")
facility<-facility%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753),
              기준_년_코드 >= 2015)
facility[is.na(facility)] <- 0
facility <- facility %>% 
  filter %>% 
  mutate(총_집객시설_수 = 집객시설_수+관공서_수+은행_수+종합병원_수+일반_병원_수+약국_수+유치원_수 +
                                               초등학교_수+중학교_수+고등학교_수+대학교_수+백화점_수+슈퍼마켓_수+극장_수+숙박_시설_수+
                                               공항_수+철도_역_수+버스_터미널_수+지하철_역_수+버스_정거장_수)
str(facility)
facility <- facility %>% 
  select(기준_년_코드,기준_분기_코드,상권_구분_코드,상권_구분_코드_명,상권_코드,상권_코드_명,총_집객시설_수 )


#점포수
store_2015 <-read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2015년.csv")
store_2016 <-read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2016년.csv")
store_2017 <-read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2017년.csv")
store_2018 <-read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2018년.csv")
store_2019 <-read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2019년.csv")
store_2020 <-read.csv("서울시 우리마을가게 상권분석서비스(상권-점포).csv")


store <- rbind(store_2015,store_2016,store_2017,store_2018,store_2019,store_2020)

store <- store%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000762,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,1001443,
                      1000754,1000756,1000747,1001450,1000750,1000751,1000753), 
              substr(store$서비스_업종_코드,1,3)=='CS1')

store <- store %>% select(-개업_율, -폐업_률)

str(store)

#점포_수 10개 이하인 상권 제거 
sales<-sales%>%
  filter(상권_코드 %in% c(1000779,1000778,1000765,1000764,1000763,1001448,
                      1000760,1000761,1000759,1000758,1001444,
                      1000754,1000756,1000747,1001450,1000753))
#merge_data 만들기 
#1. sales + pop
test_data <- merge(sales, pop,
                    by.x=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명'),
                    by.y=c('기준.년코드','기준_분기_코드','X.상권_구분_코드','X.상권_구분_코드_명','상권_코드','상권_코드_명'),
                    all.x=T)
str(test_data)
#2. sales + pop + repop
test_data <- merge(test_data, repop,
                  by.x=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명'),
                  by.y=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권.코드','상권.코드.명'),
                  all.x=T)

test_data[is.na(test_data)]

#3. sales + pop + repop + wkpop
#2015년도 등등 직장인구 없는 것들이 많아서 상관관계를 보기 위함이니까 na값은 과감히 삭제 -> 280개 삭제되어짐
test_data <- merge(test_data, wkpop,
                  by.x=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명'),
                  by.y=c('기준_년월_코드','기준_분기_코드','기준_분기_코드.1','상권_구분_코드_명','상권_코드','상권_코드_명'),
                  all.x = T)

test_data[is.na(test_data)]
test_data <- na.omit(test_data)

#4. sales + pop + repop + wkpop + facility 
#na값은 과감히 삭제 -> 68개 삭제되어짐
test_data <- merge(test_data, facility,
                    by.x=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명'),
                    by.y=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명'),
                    all.x = TRUE)

test_data[is.na(test_data)]
test_data <- na.omit(test_data)

#5. sales + pop + repop + wkpop + facility  + store
#store에서 na값은 1로 대체 -> 매출데이터는 있으니까!
test_data <- merge(test_data, store,                
                    by.x=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명','서비스_업종_코드',"서비스_업종_코드_명"),                 
                    by.y=c('기준_년_코드','기준_분기_코드','상권_구분_코드','상권_구분_코드_명','상권_코드','상권_코드_명','서비스_업종_코드',"서비스_업종_코드_명"),
                    all.x=T)

test_data[is.na(test_data)]
test_data[is.na(test_data)] <- 1

test_data$점포_수<-ifelse(test_data$점포_수==0,1,test_data$점포_수)
test_data %>% filter(점포_수 == 0)

#NA가 있는 행 찾기
View(test_data[!complete.cases(test_data), ])
View(facility %>% filter(상권_코드_명 %in% c("사당로16가길","서달로8가길")))

#na값 있는 행 이름 출력 
colSums(is.na(merge_data))
merge_data[is.na(merge_data$상권_코드_명)==T | is.na(merge_data$서비스_업종_코드_명)==T,]

#중복값 확인
merge_data%>%duplicated()%>%table()
merge_data <- test_data

#대학구분 컬럼 추가
merge_data<-merge_data%>% 
  mutate(
    상권명 = case_when(
      상권_코드_명 %in% c('흑석로9길', '흑석로13길',
                     '서달로8가길', '서달로15길',
                     '서달로14길','흑석시장') ~ "중앙대",
      상권_코드_명 %in% c('상도로61길','상도로62길',
                     '상도로47길','상도로37길','상도전통시장') ~ "숭실대", 
      TRUE ~ "총신대"
    )
  )
str(merge_data)

#covid데이터 정리
covid19 <- read.csv("owid-covid-data.csv")
covid19 <- covid19 %>% filter(iso_code == "KOR")
str(covid19)
library(lubridate)
#분기별 코로나 확진자로 묶기
covid19$month <- month(covid19$date)
covid19$quarter <- quarter(covid19$date)
covid19$year <- year(covid19$date)
covid <- covid19 %>% filter(year == 2020)
covid[is.na(covid)] <- 0
covid <- covid %>% group_by(year,quarter) %>% summarise(분기별_코로나_확진자_수 = sum(new_cases))

covid <- as.data.frame(covid)
covid <- covid %>% filter(quarter %in% c(1,2,3))

data <- merge_data

data <- merge(x = data, y = covid, 
                    by.x = c('기준_년_코드','기준_분기_코드'),
                    by.y = c('year','quarter'),
                    all.x = T)
str(data)
data[is.na(data)] <- 0
data %>%filter(기준_년_코드 == 2019, 기준_분기_코드 == 1)
merge_data <- data

#서울시 코로나 확진자 현황 컬럼 추가 
covid_seoul<-read.csv("서울시 코로나19 확진자 현황.csv")
str(covid_seoul )
library(zoo)
View(covid_seoul)
covid_seoul$quarterYear <-as.yearqtr(ymd(covid_seoul$확진일)) 
covid_seoul$quarterYear<- as.factor(covid_seoul$quarterYear)
covid_seoul<-covid_seoul%>% filter(quarterYear %in% c('2020 Q1','2020 Q2','2020 Q3'))
covid_seoul<-covid_seoul%>% 
  mutate(
    기준_분기_코드 = case_when(
      quarterYear =='2020 Q1' ~ "1",
      quarterYear =='2020 Q2' ~ "2",
      quarterYear =='2020 Q3' ~ "3"))

covid_seoul<-covid_seoul%>% 
  mutate(
    기준_년_코드 = case_when(
      quarterYear =='2020 Q1' ~ "2020",
      quarterYear =='2020 Q2' ~ "2020",
      quarterYear =='2020 Q3' ~ "2020"))
str(covid_seoul)
count(covid_seoul,기준_분기_코드)

covid_seoul<-covid_seoul%>% 
  mutate(
    서울_확진자_수 = case_when(
      quarterYear =='2020 Q1' ~ 478,
      quarterYear =='2020 Q2' ~ 844,
      quarterYear =='2020 Q3' ~ 4002))

covid_seoul<-unique(covid_seoul[,16:18])

merge_data<-merge(merge_data, covid_seoul,                
                  by.x=c('기준_년_코드','기준_분기_코드'),
                  by.y=c('기준_년_코드','기준_분기_코드'),all.x=T) 
str(merge_data)
data<-merge_data

data[is.na(data)] <- 0
data %>% filter(기준_년_코드 == 2017, 기준_분기_코드 == 2)
merge_data %>% filter(기준_년_코드 == 2015, 기준_분기_코드 == 2)
merge_data <- data

#방학 개월 수
merge_data<-merge_data%>% 
  mutate(
    방학_개월수 = case_when(
      기준_분기_코드==1 ~'2',
      기준_분기_코드==2 ~'0',
      기준_분기_코드==3 ~'2',
      기준_분기_코드==4 ~'0'))

#끝!!
write.csv(merge_data,"total_merge_data.csv",row.names = F)
data <- read.csv("total_merge_data.csv")
str(data)
rm(data)

#형태변환
str(merge_data)
library(tidyvers)
vars <- c('기준_년_코드','기준_분기_코드','상권_코드')
merge_data[vars] <- map_df(.x=merge_data[vars],.f = as.factor)

#컬럼명 변경하기
merge_data <- rename(merge_data, 
                     "시간대_00_06_유동인구_수" = "시간대_1_유동인구_수",
                     "시간대_06_11_유동인구_수" = "시간대_2_유동인구_수",
                     "시간대_11_14_유동인구_수" = "시간대_3_유동인구_수",
                     "시간대_14_17_유동인구_수" = "시간대_4_유동인구_수",
                     "시간대_17_21_유동인구_수" = "시간대_5_유동인구_수",
                     "시간대_21_24_유동인구_수" = "시간대_6_유동인구_수",
                     "분기_매출_금액" = "당월_매출_금액",
                     "분기_매출_건수" = "당월_매출_건수",
                     "연령대_10_상주인구_수" = "연령대.10.상주인구.수",
                     "연령대_20_상주인구_수" = "연령대.20.상주인구.수",
                     "연령대_30_상주인구_수" = "연령대.30.상주인구.수",
                     "연령대_40_상주인구_수" = "연령대.40.상주인구.수",
                     "연령대_50_상주인구_수" = "연령대.50.상주인구.수",
                     "연령대_60이상_상주인구_수" = "연령대.60.이상.상주인구.수")

#2019~2020년도 데이터 세팅..
test_merge_data <- merge_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 ==3)

data1<- merge_data %>%
  filter(기준_년_코드 == 2019)

data2<-merge_data %>%
  filter(기준_년_코드 == 2020, 기준_분기_코드 <= 2)

train_merge_data <- rbind(data1, data2)

write.csv(train_merge_data, "train_data.csv",row.names=F)
write.csv(test_merge_data, "test_data.csv",row.names=F)
