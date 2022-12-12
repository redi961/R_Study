## 엑셀파일 read 라이브러리 설치 및 라이브러리 로드
install.packages("readxl")
library(readxl)
getwd()

ed <- read_excel("./day02/02_역주행사고.xlsx")
ed1 <- subset(ed, 구분 == "전체")
ed2 <- subset(ed, 구분 == "역주행")

ed1
ed2

ed1$치명률 <- (ed1$사망 / ed1$사고) * 100
ed2$치명률 <- (ed2$사망 / ed2$사고) * 100
round(apply(ed3[3:5], 2, mean), 2)

## 일반 교통사고
ed3 <- ed1
ed3$구분 <- "일반"

## 전체 사고 - 역주행사고
ed3[c("사고","사망")] <- ed3[c("사고","사망")] - ed2[c("사고","사망")]
ed3

## 기초통계
summary(ed2)
summary(ed3)

역주행사고율 <- mean(ed2$치명률)
일반사고율 <- mean(ed3$치명률)

cat ("최근 3년간 역주행 교통사고의 치명률이 ", 
     round(역주행사고율, 1), 
     "%로 일반 교통사고",
     round(일반사고율, 1),
     "% 보다 ",
     round(round(역주행사고율, 1) / round(일반사고율, 1),1),
     "배 높은 것으로 나타났다."
     )

## 시각화
install.packages("ggplot2")
library(ggplot2)

ggplot(mapping =aes(x=년도, y=사고, fill=구분), data=ed) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('년도별 사고건수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
