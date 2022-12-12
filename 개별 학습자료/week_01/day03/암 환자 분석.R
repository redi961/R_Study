#패키지 설치 라이브러리 로드
install.packages("dplyr")
library(dplyr)
library(ggplot2)

#파일로드
df2 <- read.csv('../Part-I/day03/03_암발생자수_.csv', header = T, fileEncoding = "euc-kr") 
df2

#열명 확인 및 변경
names(df2)
names(df2) <- c("암종별","성별","연령별","2019년","2019년_1")
View(df2)

#모든암
df21 <- subset(df,df$암종별 == "모든 암(C00-C96)")

# 필터사용간 내부에서 상단에 ! 가 있는경우 false문으로 연산함
## 파이프 연산자(%>%): 데이터 전달하기 
## 왼쪽의 경과를 오른쪽의 함수의 첫번째 입력값으로
### x %>% f(y)  #(1) f(x, y)와 같습니다.

df2 %>%
  filter(!(연령별 %in% c("계", "연령미상")))

## 필터를 통하여 연령미상과 계 연령을 제외함함
df22 <- df %>%
        filter(df$암종별 == "모든 암(C00-C96)" &
                 !(연령별 %in% c("계", "연령미상")) )
#중복제거 & 계 값 연산
unique(df22)
names(df22) <- c("암종별","성별","연령별","2019년","2019년_1","연령대")
df22$`2019년` <- as.numeric(df22$`2019년`)

df22g <- df22 %>%
  group_by(연령대, 성별) %>% 
  summarise(계 = sum(`2019년`))

df22g

df22$연령대 <- ifelse(df22$연령별 %in% c("0-4세","5-9세","10-14세",
                                          "15-19세","20-24세","25-29세",
                                            "30-34세","35-39세"), "30대이하",
                   ifelse(df22$연령별 %in% c("40-44세","45-49세", 
                                             "50-54세","55-59세"), "40~50대",
                          ifelse(df22$연령별 %in% c("60-64세","65-69세","70-74세","75-79세"), "60~70대", "80대이상")))

######시각화

qplot(연령대, data=df22, fill = 성별) +
  ggtitle('연령별 암 환자수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

ggplot(mapping =aes(x=연령대, y=계, fill = 성별), data=df22) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('연령대에 따른 성별분석')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

