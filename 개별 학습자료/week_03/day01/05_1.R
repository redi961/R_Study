
#R 내장 데이터 가져오기
data(iris)
View(iris)

#iris 데이터 확인
str(iris)

#iris : 꽃받침, 꽃잎 데이터 추출
iris[,-5]
iris1 <- iris[,-5]
iris1

#기술통계량
summary(iris1)
 
#상관계수 (a가 커질수록 b가 커지는경우 양의 상관관계 / a가 커질수록 b가 작아질시 음의 상관관계)
# (0.7이상일시 관계성이 크다고 판단할 수 있을것)
cor(iris1, method="pearson")

#색의 농도로 상관계수 
install.packages("corrgram")
library(corrgram)

corrgram(iris1, upper.panel = panel.conf)

#상관계수 챠트
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(iris1)

#학습데이터와 테스트데이터 분리
1:nrow(iris1) # 1부터 150까지 (전체)
View(iris)
#랜덤하게 총 150개의 데이터중 70%의 데이터를 뽑아서 넣음
x <- sample(1:nrow(iris1), 0.7 * nrow(iris1))
x
 
train <- iris[x, ] #x에 해당하는 데이터
test <- iris[-x, ] #x에 해당하지 않는 데이터
nrow(train)
nrow(test)

#회귀모델 : 꽃받침 길이 예측 
names(iris1)

#학습
## (지도학습 = feature를 통하여 target을 예측하는것)
## (formula = Sepal.Length / target) // ( ~ Sepal.Width + Petal.Length + Petal.Width / feature)
View(test)
model1 <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=train)
model2 <- lm(formula = Sepal.Length ~ Petal.Length + Petal.Width, data=train)
model3 <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data=train)
model4 <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Width, data=train)
model5 <- lm(formula = Sepal.Length ~ Sepal.Width, data=train)
model6 <- lm(formula = Sepal.Length ~ Petal.Width, data=train)

#예측
pred1 <- predict(model1, test)
pred2 <- predict(model2, test)
pred3 <- predict(model3, test)
pred4 <- predict(model4, test)
pred5 <- predict(model5, test)
pred6 <- predict(model6, test)

#평가
#RMSE : sqrt((실제 - 예측)^2의 평균)
## Feature가 많을수록 오차값의 범위는 줄어든다
RMSE1 <-  sqrt(mean((test$Sepal.Length - pred1)^2))
RMSE2 <-  sqrt(mean((test$Sepal.Length - pred2)^2))
RMSE3 <-  sqrt(mean((test$Sepal.Length - pred3)^2))
RMSE4 <-  sqrt(mean((test$Sepal.Length - pred4)^2))
RMSE5 <-  sqrt(mean((test$Sepal.Length - pred5)^2))
RMSE6 <-  sqrt(mean((test$Sepal.Length - pred6)^2))


cat("예측 1 : ", RMSE1) # 피쳐 3개
cat("예측 2 : ", RMSE2)
cat("예측 3 : ", RMSE3)
cat("예측 4 : ", RMSE4)
cat("예측 5 : ", RMSE5)
cat("예측 6 : ", RMSE6)

########### 

#분류모델 
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width))  + 
  geom_point(aes(colour = Species))

ggplot(iris, aes(Petal.Length , Petal.Width))  + 
  geom_point(aes(colour = Species))

#학습데이터와 테스트데이터 나누기
x <- sample(1:nrow(iris), 0.7 * nrow(iris))
x

train <- iris[x, ] #x에 해당하는 데이터
test <- iris[-x, ] #x에 해당하지 않는 데이터
nrow(train)
nrow(test)

#모델 학습
#트리 모델 
install.packages("party")
library(party)
names(iris)

View(train)

model1 <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,data = train)
model2 <- ctree(Species ~ Sepal.Length + Petal.Length + Petal.Width, data=train)
model3 <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length, data=train)
model4 <- ctree(Species ~ Sepal.Length + Sepal.Width, data=train)

plot(model1)
plot(model2)
plot(model3)
plot(model4)

#예측 
pred1 <- predict(model1, test)
pred2 <- predict(model2, test)
pred3 <- predict(model3, test)
pred4 <- predict(model4, test)

#혼돈행렬
t1 <- table(test$Species, pred1)
t2 <- table(test$Species, pred2)
t3 <- table(test$Species, pred3)
t4 <- table(test$Species, pred4)

acc1 <- (t1[1,1] + t1[2,2] + t1[3,3]) / sum(t1)
acc2 <- (t2[1,1] + t2[2,2] + t2[3,3]) / sum(t2)
acc3 <- (t3[1,1] + t3[2,2] + t3[3,3]) / sum(t3)
acc4 <- (t4[1,1] + t4[2,2] + t4[3,3]) / sum(t4)

acc1
acc2
acc3
acc4

class(train$Species)

#iris data 저장
 
