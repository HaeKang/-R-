library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

library(ggmap) 
library(raster) 
library(rgeos) 
library(maptools)


data1 <- read.csv("국립암센터_년도별_국가암등록통계_24개종_연령군별_암발생률__2015.csv")
data2 <- read.csv("보건복지부_시도별_암발생자_분포_2014년_.csv")

names(data2) <- c("시도","남자","여자")

str(data1)
str(data2)

head(data2)


#데이터 클린징
colSums(is.na(data1))
colSums(is.na(data2))

data2_clean <- data2[-c(1,19),]
str(data2_clean)


#data1 발생률 null 처리
data1_notnull <- data1
data1_notnull$발생률 <- ifelse(is.na(data1_notnull$발생률), 
                            0, data1_notnull$발생률)
data1_notnull$발생자수 <- ifelse(is.na(data1_notnull$발생자수), 
                            0, data1_notnull$발생자수)
colSums(is.na(data1_notnull))

table(data1_notnull$발생연도)


#data1 분석 시작 




#연도 별 암환자 수


data1_year <- data1_notnull %>%
  filter(연령군=="전체") %>%
  group_by(발생연도) %>% 
  select(발생연도, 발생자수)


data1_year <- data1_year %>% group_by(발생연도) %>% 
  summarise(발생자수 = sum(발생자수))

data1_year

ggplot(data = data1_year, aes(x = 발생연도, y=발생자수)) + geom_line()


       
#연령대별 암 종류율

data1_age <- data1_notnull %>% 
  mutate(연령대 = 
              ifelse(연령군 %in% c("00-04세", "05-09세","10-14세"), "child", 
              ifelse(연령군 %in% c("65-69세", "70-74세", "75-79세"), "middle",
              ifelse(연령군 %in% c("80-84세", "85세이상"), "old", "youth")))) %>%
  filter(암종 != "모든암")

data1_age <- data1_age %>% 
  group_by(연령대) %>% 
  summarise(총발생자수 = sum(발생자수))

ggplot(data = data1_age, aes(x =연령대, y=총발생자수/1000000)) + 
  geom_bar(stat = "identity", fill = "#FF9900")


str(data1_notnull)
     

  
#성별 별 암 종류 비유

data1_man <- data1_notnull %>%
  group_by(성, 암종) %>%
  summarise(발생자수 = sum(발생자수)) %>%
  filter(성 == "남자", 암종 != "모든암")
  
data1_man <- data1_man[order(-data1_man$발생자수), ]
data1_man_top <- data1_man[c(1,2,3,4,5), ]

ggplot(data = data1_man_top, aes(x = 암종, y=발생자수/1000000)) + 
  geom_bar(stat = "identity", width = 0.7, fill="steelblue")


data1_woman <- data1_notnull %>%
  group_by(성, 암종) %>%
  summarise(발생자수 = sum(발생자수)) %>%
  filter(성 == "여자", 암종 != "모든암")

data1_woman <- data1_woman[order(-data1_woman$발생자수), ]
data1_woman_top <- data1_woman[c(1,2,3,4,5), ]

ggplot(data = data1_woman_top, aes(x = 암종, y=발생자수/1000000)) + 
  geom_bar(stat = "identity", width = 0.7, fill="#CC3399")


#data2에 col 추가 (전체인원 수)
  
data2_clean <- data2_clean %>% mutate(전체인원수 = 남자+여자)
head(data2_clean,5)


# data2 pie 시각화 
number_pat <- c(41078,16714,11075,11124,5633,5960,4421,632
           ,44972,7285,7156,10017,9319
           ,10508,13705,14826,2620)
percent1 <- round(number_pat/sum(number_pat) * 100)
area1 <- c("서울","부산","대구","인천","광주","대전","울산",
           "세종","경기도","강원도","충청북도","충청남도",
           "전라북도","전라남도","경상북도","경상남도","제주도")
area1 <- paste(area1, percent1)
percent1 <- paste(area1, "%",sep="")
pie(number_pat, main="시도별 암 환자 수",labels = percent1)


#data2 map visual
test <- read.csv("test.csv")
test
korea <- shapefile("TL_SCCO_CTPRVN.shp")
korea <- spTransform(korea, CRS("+proj=longlat"))
korea_map <- fortify(korea)
merge_result <- merge(korea_map, test, by="id")

q <- ggplot() + geom_polygon(data=merge_result, aes(x = long, y = lat, group=group,
                                               fill = 전체인원수)) + 
  labs(fill="암 환자 수")
q + scale_fill_gradient(low='yellow', high='red')
