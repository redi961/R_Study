# 기상개황 로드
wdf <- read.csv("./day02/02_기상개황.csv", header = T, fileEncoding = "euc-kr")
View(wdf)

#테이블 확인 및 필요자료 추출
names(wdf)
wdf <- wdf[c("월별.1.","평균기온....","평균상대습도....")]

#행 이름 재정의
names(wdf) <- c("월","평균기온","평균상대습도","불쾌지수치","불쾌지수단계")
wdf

##계산진행 및 테이블 삽입입
## DI = 0.81 * Ta + 0.01 * RH(0.99 * Ta - 14.3) + 46.3
# DI: 불쾌지수
# Ta: 건구온도(평균기온)
# RH: 상대습도(평균상대습도)

wdf$불쾌지수치 <- 0.81 * wdf$평균기온 + 0.01 * wdf$평균상대습도*(0.99 * wdf$평균기온 - 14.3) + 46.3
wdf$불쾌지수단계 <- ifelse(wdf$불쾌지수치 < 68 , "낮음", 
                     ifelse(wdf$불쾌지수치 < 75 , "보통", 
                      ifelse(wdf$불쾌지수치 < 80 , "높음", "매우높음")))
wdf
##연간자료 제외
wdf <- wdf [2:13, ]
wdf


## 시각화를 위한 빈도 분석
bindo <- table(wdf$불쾌지수단계)
bindo

## 파악한 빈도테이블을 데이터 프레임으로 전환
dbindo <- as.data.frame(bindo)
names(dbindo) <- c("불쾌지수단계", "인원수")
dbindo
class(dbindo)

## 범주 데이터 전환
dbindo$인원수 <- as.factor(dbindo$인원수)

## 시각화
ggplot(mapping =aes(x=불쾌지수단계, y=인원수, fill = 불쾌지수단계), data=dbindo) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('불쾌지수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))


####################
dfdit <- table(wdf$불쾌지수단계)
barplot(dfdit)

dfdit2 <- as.data.frame(dfdit)
class(dfdit2)
names(dfdit2) <- c("불쾌지수단계", "인원수")
dfdit2

## 시각화
ggplot(mapping =aes(x=불쾌지수단계, y=인원수, fill = 불쾌지수단계), data=dfdit2) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('불쾌지수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
