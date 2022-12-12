install.packages("ggplot2")
library(ggplot2)

df <- read.csv('./day02/부산광역시_지방세 체납현황_20191231.csv', header = T, fileEncoding = "euc-kr")
View(df)
names(df)

df <- df[c("과세년도","세목명" ,"체납액구간","누적체납건수","누적체납금액")]
df

#세목명확인
cols = unique(df$세목명) ##Unique --> 중복 제거
cols

#과세년도 범주형으로 전환
## 범주형 데이터이면서 수치형 데이터로 잡혀있는경우 사용함 (자료 분류에 유용)
## 사용하지 않을시 2017 2017.5 2018 2018.5 형태로 표기되어 분류에 어려움
df$과세년도 <- as.factor(df$과세년도)

makedf <- function(item) {
  temp <- subset(df, df$세목명 == item)
  ggplot(mapping =aes(x=과세년도, y=누적체납건수, fill = 체납액구간), data=temp) +
    geom_bar(stat="identity", position=position_dodge()) +
    ggtitle('채납분석')+
    theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
}

makedf("지방소득세")

