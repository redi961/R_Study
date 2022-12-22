#라이브러리 불러오기
install.packages("dplyr")
library(ggplot2)
library(dplyr)

#파일 불러오기 및 뷰 확인
df <- read.csv("../week_03/day02/06_지상관측.csv",header = T, fileEncoding = "euc-kr") 
View(df)

# 열명 변경 및 필요 열 생성
names(df) <- c("지점","지점명","일시","기온","풍속","상대습도")
df$풍속2 <- (3.6 * df$풍속)
df$풍속2 <- round(df$풍속2,1)
df$체감온도 <- (13.12 + 0.6215*df$기온 - 11.37*df$풍속2*0.16 + 0.3965 * df$풍속2 * 0.16 * df$기온)
df$체감온도 <- round(df$체감온도,1)
df$겨울체감 <- ifelse(df$체감온도 <= 10 & df$풍속 >= 1.3 ,"True","False")

df2 <- df %>%
  filter(지점명 == "부산" & 겨울체감 == "True")
df2

View(df2)

ggplot(mapping =aes(x=일시, y=체감온도,fill = 체감온도), data=df2) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('부산지역 겨울철 체감온도')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),axis.text.x=element_text(angle=90, hjust=1))

############

df$열지수 <- 0.5 * {df$기온 + 61.0 + ((df$기온- 68.0) * 1.2) + (df$상대습도*0.094)}
df$열지수 <- round(df$열지수,1)

#df3 <- df%>% filter(지점명 == "서울" | 지점명 == "부산" | 지점명 == "제주")
df3 <- df %>% filter(지점명 %in% c("서울", "부산", "제주"))
View(df3)

ggplot(mapping =aes(x=일시, y=기온,fill = 지점명), data=df3) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_hline(yintercept = mean(df$기온), linetype= "dotted", color="red", size=1) + # 평균치 수직선
  ggtitle('기온현황')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),axis.text.x=element_text(angle=90, hjust=1))

ggplot(mapping =aes(x=일시, y=열지수,group = 지점명, color = 지점명), data=df3) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 5, linetype= "dotted", color="red", size=1) + # 평균치 수직선
  ggtitle('열지수 현황')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),axis.text.x=element_text(angle=90, hjust=1))

############

df4 <- df3%>%
  filter (열지수 <= 5)
View(df4)



####

## 그래프 관련 참고자료 https://blog.naver.com/PostView.naver?blogId=pmw9440&logNo=221986397977&parentCategoryNo=&categoryNo=&viewDate=&isShowPopularPosts=false&from=postView
## 그래프 관련 참고자료2 http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization