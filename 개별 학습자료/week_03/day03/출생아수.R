## 파일불러오기
df <- read.csv("../week_03/day03/07_출생아수.csv", 
               header = T, 
               stringsAsFactors = F, 
               fileEncoding = 'euc-kr')
df
str(df)

## 라이브러리 불러오기 
install.packages("forecast")
library(forecast)
library(dplyr)

df
## 열이름 변경 및 학습데이터 기간 선별후 추출
names(df) <- c("시점","출생아수")
train <- df %>% filter(시점 < '2010' & 시점 >= '2000')
test <- df %>% filter(시점 >= '2011' & 시점 < '2016')

train
test

temp <- train$출생아수
temp

## start c(연,월) 월이없는경우 연 만 / frequency는 연을 월단위로 쪼갤시 12 / 연단위 1
temp_ts <- ts(temp, frequency = 1, start = c(2000))
temp_ts

View(temp_ts)
## 뒤에서 사용할 아리마 모델을 추천해줌 결과 확인후 order에 대입할것
arima <- auto.arima(temp_ts)
arima

model <- arima(temp_ts, order=c(0,1,0))
model

tsdiag(model)

#box-Ljungn잔차항 모형 진단
#p-value >= 0.05 통계적으로 적절 
Box.test(model$residuals, lag=1, type="Ljung")

#예측
## 4개의 예상자료
fore <- forecast(model, h=5)
fore

plot(fore) 

test$출생아수
class(fore$mean)
pred = as.vector(fore$mean)

result <- data.frame(test = test$출생아수, pred=pred)
result

plot(result$test, type="o", col="red")
par(new=T)
plot(result$pred, type="o", col="blue")

