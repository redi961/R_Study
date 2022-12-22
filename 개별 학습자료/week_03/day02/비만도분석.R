## 자료 불러오기
df <- read.csv("../week_03/day02/06_국민건강보험공단500.csv",header = T, fileEncoding = "euc-kr") 
View(df)

# 필요 자료 추출 및 이름 재설정
names(df)
dfb <- df[c("신장.5Cm단위.", "체중.5Kg.단위.")]
names(dfb) <- c("신장", "체중")

# 결측치 확인
sum(is.na(dfb))

# BMI수치 측정 및 비만도 자료화
dfb$BMI <- dfb$체중 / (dfb$신장/100) ** 2

dfb$BMI <- round(dfb$BMI,0)

dfb$비만도 <- ifelse(dfb$BMI < 20 , "저체중", 
                     ifelse(dfb$BMI >= 20 & dfb$BMI <=24, "정상",
                            ifelse(dfb$BMI >= 25 & dfb$BMI <= 29 , "과체중", "비만" )))
View(dfb)
###### 학습자료화 ######
## 측정 데이터를 범주화 (팩터화)
dfb$비만도 <- as.factor(dfb$비만도)

## 학습자료, 확인자료 분류
1:nrow(dfb) # 전체데이터 갯수 확인

x <- sample(1:nrow(dfb), 0.7 * nrow(dfb))
x

train <- dfb[x, ] #x에 해당하는 데이터
test <- dfb[-x, ] #x에 해당하지 않는 데이터
nrow(train)
nrow(test)

############ 라이브러리 불러오기
install.packages("party")
library(party)
names(dfb)

########## 모델결정 (분류모델) -> 비만도 예측 #########

model1 <- ctree(비만도 ~ 체중 + 신장, data=train)

#시각화
plot(model1)

# 학습
pred1 <- predict(model1, test)

#혼돈행렬 확인
t1 <- table(test$비만도, pred1)
t1

## 정확도 계산
acc1 <- (t1[1,1] + t1[2,2] + t1[3,3] + t1[4,4]) / sum(t1)
cat("비만도 예측 정확도 :: ", acc1)

########## 지도학습을 통하여 BMI 예측 #############

# 학습
model2 <- lm(formula = BMI ~ 체중 + 신장, data=train)

# 예측
pred2 <- predict(model2, test)

# 오차확인
RMSE2 <-  sqrt(mean((test$BMI - pred2)^2))

# 오차범위확인
cat("BMI 예측 오차범위 :: ", RMSE2)



