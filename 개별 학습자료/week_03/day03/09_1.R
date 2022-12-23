#한국환경공단_에어코리아_대기오염통계 현황
#시도별 실시간 평균정보 조회 상세기능명세
#최근 한달간 지역별 일평균 대기오염 정보
#기준초과인경우 
#https://www.airkorea.or.kr/web/contents/contentView/?pMENU_NO=132&cntnts_no=6

#json처리
install.packages("jsonlite") 
library(jsonlite)

#자료처리
library(dplyr)

#데이터 가져오기함수
getData <- function(item, igubun) {
  itemcode <- item
  gubun <- igubun
  scondit <- "MONTH" 
  rtype <- "json"
  skey = "TD9CtocYik0qfKi%2Fppbc8NgpHbn8qy5tIWz0JrY4eOajiGLrFHmz3jlDND4K0mK8JIdIBl34hmgrjQc4BZ7apA%3D%3D"
  
  url = paste("http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst?",
              "itemCode=",itemcode,
              "&dataGubun=",gubun,
              "&searchCondition=",scondit,
              "&pageNo=1&numOfRows=100",
              "&returnType=",rtype,
              "&serviceKey=",skey,sep="")
  
  weather <- fromJSON(url)
  wdata <- weather$response$body$items
  
  return (wdata)
  
}

  df1 <- getData("PM10", "DAILY")
  View(df1)  

  df2 <- getData("O3","DAILY")
  View(df2)
  
#1. pm10, O3 데이터 추출하여 합치기
##dplyr 필요
  
  df3 <- bind_rows(df1, df2)
  View(df3)
  df3
  
#2. 지역명 벡터
area <- c("seoul", "busan", "daegu","incheon","gwangju",
          "daejeon", "ulsan", "gyeonggi", "gangwon",
          "chungbuk", "chungnam", "jeonbuk", "jeonnam",
          "gyeongbuk", "gyeongnam", "jeju", "sejong")

length(area)
areaname <- c("서울","부산","대구","인천","광주","대전",
              "울산","경기","강원","충북","충남","전북",
              "전남","경북","경남","제주","세종")
length(areaname)

dft <- data.frame()

for (i in 1:length(area)) {
  print(paste(i, area[i], areaname[i]))
}

View(df3)
## case -- 1
#for (i in 1:length(area)) {
#  temp <- c()
#  temp$dataTime <- unlist(df3["dataTime"])
#  temp$itemCode <- unlist(df3["itemCode"])
#  temp$area <- areaname[i]
#  temp$item <- unlist(df3[area[i]])
#  dft <- bind_rows(dft, temp)
#}

## case -- 2
dft <- data.frame()
for (i in 1:length(area)) {
  t <- df3[c("dataTime", "itemCode", area[i])]
  t$area <- areaname[i]
  names(t) <- c("dataTime", "itemCode", "item", "area")
  dft <- bind_rows (dft, t)
}

View(t)


#3. 통합데이터프레임 만들기
 


#4.주의보
#https://www.airkorea.or.kr/web/dustForecast?pMENU_NO=113
dft$기준 <- ifelse(dft$itemCode == "PM10" ,
                 ifelse(dft$item <= 30 ,"좋음",ifelse(dft$item <= 80, "보통", ifelse(dft$item <= 150, "나쁨", "매우나쁨"))),
                 ifelse(dft$item <= 0.03, "좋음", ifelse(dft$item <= 0.09, "보통", ifelse(dft$item <= 0.15, "나쁨", "매우나쁨"))))

View(dft) 

dft$item <- as.numeric(dft$item)

#5.일자별 주의보정보
## View로 확인하였을때 왼쪽정렬인경우 문자열 / 우측정렬이면 숫자형으로 구분이 가능함
names(dft)
 
names(dfn2) <- c("일자", "기준","기준수")

# summarise(n = n()) <- group_by() 와 함께 chaining 해서 사용
# 그룹별, 집단별 합계
dfPM10 <- dft %>%
  filter(itemCode =="PM10" & 기준 == "좋음") %>%
  group_by(area) %>%
  summarise(n=n())

View(dfPM10)

dfn <- table(dft$dataTime, dft$기준)
dfn1 <- as.data.frame(dfn)
dfn2 <- as.data.frame.matrix(dfn)

class(dfn)
dfn1 # 빈도수를 자동으로 정렬하여 표기됨
class(dfn2) # 빈도수 테이블을 그대로 표기함 ## 데이터 형식을 그대로 사용하고싶을때 사용

library(ggplot2)
ggplot(dft, aes(x=dataTime, y =item, group=기준, color=기준)) +
  geom_line() +
  geom_point() +
  labs(x="", y="")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#5.지역별 PM10이 좋음인 날 수 
names(dft)
 
ggplot(dfPM10, aes(x=area, y =n, fill=area)) +
  geom_bar(stat="identity") +
  labs(x="", y="")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
