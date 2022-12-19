#인구 동향
library(dplyr)

#데이터 불러오기(인구동향)
df <- read.csv("../week02/day01/04_인구동향.csv",header = T, fileEncoding = "euc-kr", stringsAsFactors = F) 

# 열명 확인
# "행정구역별","시점","출생아수","사망자수","혼인건수","이혼건수"
View(df)
names(df) <- c("행정구역별","시점","출생아수","사망자수","혼인건수","이혼건수")
head(df)
## 평균치를 냈을때 의미를 가지는가에 따라 범주형 / 수치형을 구분할수 있음


#EDA
#데이터 구조 확인
str(df)

#범주형 변경 : 시점 int -> category
df$시점 <- as.factor(df$시점)
mode(df$시점)
class(df$시점)

#결측치 확인
summary(df)
options(max.print = 1000000)
is.na(df)

## 자체적으로 TRUE / FALSE 값을 가지기 떄문에 추가적으로 비교문을 입력할 필요는 없음
df2 <- df %>% 
  filter(is.na(혼인건수)) 

unique(df2$행정구역)
unique(df2$시점)

#결측치 행 제거
df3 <- na.omit(df)
summary(df3)
summary(df)
#################CASE-1######################

#결측치 값 대체
df4 <- df
df4$출생아수 <- ifelse(is.na(df4$출생아수),"0",df4$출생아수)
df4$사망자수 <- ifelse(is.na(df4$사망자수),"0",df4$사망자수)
df4$혼인거수 <- ifelse(is.na(df4$혼인건수),"0",df4$혼인건수)
df4$이혼건수 <- ifelse(is.na(df4$이혼건수),"0",df4$이혼건수)
summary(df4)

#################CASE-2######################

## 단축 -> "출생아수" "사망자수" "혼인건수" "이혼건수"에 대한 na값 0으로 변경 : 반복문
col <- names(df)[3:6] ## 출생아수 : 이혼건수 컬럼명을 벡터로써 추출함
df[,"출생아수"]
is.vector(col)
col

for(c in col) {
  temp <- df[,c] # 추출할 열 이름을 통하여 반복문 진행
  temp <- ifelse(is.na(temp),0,temp)
  df[,c] <- temp
}
summary(df)

#################CASE-3######################

df4 <- df4 %>% replace(is.na(df4), 0)
df4

#############################################
 
# 자연증가수 
df$자연증가수 <- df$출생아수 - df$사망자수
df
View(df) 

# df[order(df$행정구역별 == "전국", -df$자연증가수),]

##Case 1 ::
df[df$행정구역 == "전국"&df$자연증가수<0, ]['시점']

##Case 2 ::
df[which(df$행정구역 == "전국" & df$자연증가수 <0), ]['시점']

##Case 3 ::
df %>% 
  filter(df$행정구역별 == "전국" & df$자연증가수 <0) %>%
  select('시점')

# 기술통계분석 - 범주형자료 - 빈도분석
## Table은 범주형 자료에 사용
table (df$행정구역)
table (df$시점)

# 기술통계분석 - 범주형자료 - 빈도분석 - barplot 그래프
summary(df$출생아수)

# 기술통계분석 - 연속형자료 - 산점도 그래프
plot(df$출생아수, df$혼인건수)

# 자료 나누기
dft <- df %>% 
  filter(df$행정구역별 == "전국")
dfa <- df %>%
  filter(df$행정구역 != "전국")


# 사망자수와 출생아수의 비교 그래프 시각화
library(ggplot2)

plot(df$사망자수, type="o", col="red", xlab='', ylab= '')
par(new=T)
plot(df$출생아수, type="o", col="blue", xlab='', ylab= '',axes=F) 

ggplot(mapping =aes(x=시점, y=출생아수, fill = 출생아수), data=df) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('출생아수와 사망자수 시각화')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),axis.text.x=element_text(angle=90, hjust=1))


library(ggplot2)
  


