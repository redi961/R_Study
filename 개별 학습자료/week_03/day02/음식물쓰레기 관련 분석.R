#json 라이브러리 불러오기
install.packages("jsonlite")
library(jsonlite)

sKey <- "TD9CtocYik0qfKi%2Fppbc8NgpHbn8qy5tIWz0JrY4eOajiGLrFHmz3jlDND4K0mK8JIdIBl34hmgrjQc4BZ7apA%3D%3D"
readType <- "json"
year <- "2021"
month <- "08"

## https를 입력시 오류가 남 http로 사용할것

url <- paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDateList?",
             "ServiceKey=",sKey,
             "&type=",readType,
             "&page=1&rowNum=7",
             "&disYear=",year,
             "&disMonth=",month,sep="")
url

trash <- fromJSON(url)

df <- trash$data$list
df

View(df)
#######

getData <- function(y,m) {
  sKey <- "TD9CtocYik0qfKi%2Fppbc8NgpHbn8qy5tIWz0JrY4eOajiGLrFHmz3jlDND4K0mK8JIdIBl34hmgrjQc4BZ7apA%3D%3D"
  readType <- "json"
  year <- y
  month <- m
  
  url <- paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDateList?",
               "ServiceKey=",sKey,
               "&type=",readType,
               "&page=1&rowNum=7",
               "&disYear=",year,
               "&disMonth=",month,sep="")
  trs1 <- fromJSON(url)
  tr <- trs1$data$list
  tr
  
  return (tr)
}

df1 <- getData("2020", "08")
df2 <- getData("2021", "08")
df3 <- getData("2022", "08")

## case -- 1
df4 <- rbind(df1,df2,df3)
df4

## case -- 2
library(dplyr)
df4 <- bind_rows(df1,df2,df3)
df4

## 지정한 년도의 모든 달의 데이터를 수집하는 반복문
dft <- data.frame() ;

# 드래그 후 통째로 실행할것
for (i in 1:12) {
  if (i< 10) { m = paste("0", i , sep="")}
  else m = as.character(i) ;
  temp <- getData("2021", m)
  dft <- bind_rows(dft, temp)
}
dft

#df4$배출요일 <- as.factor(df4$배출요일)
#df4$배출요일 <- ifelse(df4$배출요일 == "1" , "월", 
#                   ifelse(df4$배출요일 == "2", "화", 
#                          ifelse(df4$배출요일 == "3", "수",
#                                 ifelse(df4$배출요일 == "4", "목",
#                                        ifelse(df4$배출요일 == "5", "금",
#                                               ifelse(df4$배출요일 == "6", "토", "일"))))))

# 유사한 명령어
# 데이터 열 생성

names(dft) <- c("배출연도", "배출월", "배출일", "배출요일", "배출량", "배출량비율", "배출횟수", "배출횟수비율")

dft <- dft%>%
  mutate(배출요일_한 = case_when(배출요일 == 1 ~ "일",
                          배출요일 == 2 ~ "월",
                          배출요일 == 3 ~ "화",
                          배출요일 == 4 ~ "수",
                          배출요일 == 5 ~ "목",
                          배출요일 == 6 ~ "금",
                          배출요일 == 7 ~ "토"))
######

library("ggplot2")

dft

dft$배출요일 <- as.factor(dft$배출요일)
dft$배출월 <- as.factor(dft$배출월)

dft$배출량 <- dft$배출량 / 1000000
dft$배출량 <- round(dft$배출량,0)

ggplot(mapping =aes(x=배출월, y=배출횟수, group = reorder(배출요일, 배출요일_한), fill = 배출요일_한), data=dft) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('2020 ~ 2022 년도의 요일별 쓰레기 배출량')+
  geom_hline(yintercept = mean(dft$배출횟수), linetype= "dotted", color="red", size=1) +
  ## 범주 정렬 지정한 순으로 정렬됨
  scale_fill_discrete(limits=c("일","월","화","수","목","금","토")) +
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

  