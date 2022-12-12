# 파일리딩
## Header는 가장윗줄을 열의 이름으로 취급할것인지 여부를 확인하는 것이다.
df <- read.csv("../Part-I/day03/03_치매환자현황.csv", header = T, fileEncoding = "euc-kr")
View(df)

#필요 라이브러리 로드 (필요시 설치)
#install.packages("ggplot2")
install.packages("qplot")
library(ggplot2)

# 날짜처리 패키지
install.packages("lubridate")
library(lubridate)

#### 파일 자료형 확인간 class / mode / typeof 형 적극적으로 활용할것 ####

###필요자료추가 과정###
names(df)
df$나이 <- 2022 - df$출생년도

#연령대 쉽게 구하기
df$연령대 <- df$나이 %/% 10 * 10
#끝에 n대 삽입
df$연령대 <- paste(df$연령대,"대", sep="")
#나이대 100대 -> 90대 재설정
subset(df, df$연령대 == "100대")
df$연령대 <- ifelse(df$연령대 == "100대", "90대", df$연령대)


## 날짜 비교 (difftime 이용 (mins, secs도 활용가능))
df$진단일수 <- abs(difftime(df$데이터기준일자, df$진단일자, 'days'))
## 날짜값 데이터 변환
df$진단일수 <- as.integer(df$진단일수)
## abs = 절대값
mean(df$진단일수)
df$진단일수


## 날짜 비교 2 (as.date이용)
d1 <- as.Date(df$진단일자)
d2 <- as.Date(df$데이터기준일자)
d2-d1

#평균 진단일 확인 (반올림)
mean(df$진단일수)
round(mean(df$진단일수), 2)

## 거주지역에 따른 빈도 추출 및 테이블 이름 재설정
bind1 <- table(df$거주지역, df$성별)
bind1 <- as.data.frame(bind1)
names(bind1) <- c("거주지역","성별","환자수")
bind1

barplot(bind)



## 거주지역에 따른 환자수 시각화
ggplot(mapping =aes(x=거주지역, y=환자수, fill = 거주지역), data=bind1) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('지역에 따른 환자수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

## qplot의 경우
qplot(거주지역, 환자수, data=bind1, fill=거주지역) +
  geom_bar(stat="identity") +
  ggtitle('지역에 따른 환자수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

#########################

# 연령대에 따른 빈도 추출 및 테이블 이름 재설정
bind2 <- table(df$연령대)
bind2 <- as.data.frame(bind2)
names(bind2) <- c("연령대","환자수")
bind2

## 연령대에 따른 환자수 시각화
ggplot(mapping =aes(x=연령대, y=환자수, fill = 연령대), data=bind2) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('연령대에 따른 환자수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

