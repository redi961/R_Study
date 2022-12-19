#암종류별 성별 분석
install.packages("dplyr")
library(ggplot2)
library(dplyr)
getwd()
setwd("C:/Rwork/")

#데이터 불러오기(암발생자수)
df <- read.csv("./week01/day03/03_암발생자수_.csv",header = T, fileEncoding = "euc-kr") 

# 열명 변경
# "암종별", "성별", "연령별", "발생자수", "조발생률"
View(df)
names(df) <- c("암종별","성별","연령별","발생자수","조발생률")
names(df)

# 데이터셋 조회
# 1) 특정 변수 조회
## r에서 사용간에는 벡터형식으로 처리하는것을 권장함
t1 <- df$암종별
class(t1)
mode(t1)
is.vector(t1)
t1


# 2) 특정 열명을 사용하여 조회
t2 <- df['암종별']
class(t2)
mode(t2)
is.vector(t2)


# 3) 특정 행 조회 :1행 조회
df1[c(2,4),]


# 4)특정행 제거 : 1행제거
# r에서 -인덱스는 제거의 개념이다
# 1행의 인덱스를 모두 삭제
df
df <- df[-1,]
head(df)

# 5) 특정행 열 조회
df[1:3,c('암종별','발생자수')]

# 열 데이터 타입 확인
str(df)


# 값 변경 : - => 0
df22$발생자수 <- ifelse(df22$발생자수 == "-", 0, df22$발생자수)
df22

df$발생자수 <- ifelse(df$발생자수 == "-", 0, df$발생자수)
 
# 열 데이터타입 변경
df22$발생자수 <- as.numeric(df22$발생자수)
df$발생자수 <- as.numeric(df$발생자수)
df$조발생률 <- as.numeric(df$조발생률)

is.numeric(df22$발생자수)

### 값 추출
unique(df$암종별)


df21 <- df2 %>%
  filter(성별 == "계")

df22 <- df2 %>%
  filter(성별 != "계")
df22

# 특정열 가져오기
df21 <- df21[c('암종별', '발생자수')]
df21

df22 <- df22[c('암종별', '성별', '발생자수')]
df22

str(df21)

View(df21)

table(df22$발생자수,df22$성별)

# 모든암 제거하고 연령별이 계인 데이터 
#SELECT FROM 유사명령어 (dplyr)
df2 <- df %>% 
  filter(암종별 != "모든 암(C00-C96)") %>%
  filter(연령별 == "계")

#그래프
ggplot(mapping =aes(x=암종별, y=발생자수, fill = 암종별), data=df21) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('암종류에 따른 성별분석')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),axis.text.x=element_text(angle=90, hjust=1))

ggplot(mapping =aes(x=암종별, y=발생자수, fill = 성별), data=df22) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('연령대에 따른 성별분석')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),axis.text.x=element_text(angle=90, hjust=1))


##
plot(df21$발생자수, type="o", col="red", xlab='', ylab= '')
par(net=T)
plot(df22$발생자수, type="o", col="red", xlab='', ylab= '')
