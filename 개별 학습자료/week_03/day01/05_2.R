#타이타닉 데이터
#https://www.kaggle.com/c/titanic/overview

#데이터불러오기
df <- read.csv("../week_03/05_titanic.csv", 
                  header = T, 
                  stringsAsFactors = F, 
                  fileEncoding = 'euc-kr') 
head(df)
View(df)

#데이터구조
# PassengerID	승객을 구별하는 고유 ID number	Int
# Survived	승객의 생존 여부를 나타내며 생존은 1, 사망은 0 입니다.	Factor
# Pclass	선실의 등급으로서 1등급(1)부터 3등급(3)까지 3개 범주입니다.	Ord.Factor
# Name	승객의 이름	Factor
# Sex	승객의 성별	Factor
# Age	승객의 나이	Numeric
# SibSp	각 승객과 동반하는 형제 또는 배우자의 수를 설명하는 변수이며 0부터 8까지 존재합니다.	Integer
# Parch	각 승객과 동반하는 부모님 또는 자녀의 수를 설명하는 변수이며 0부터 9까지 존재합니다.	Integer
# Ticket	승객이 탑승한 티켓에 대한 문자열 변수	Factor
# Fare	승객이 지금까지 여행하면서 지불한 금액에 대한 변수	Numeric
# Cabin	각 승객의 선실을 구분하는 변수이며 범주와 결측치가 너무 많습니다.	Factor
# Embarked	승선항, 출항지를 나타내며 C, Q, S 3개 범주이다.	Factor




# 결측치 확인
sum(is.na(df$Age))

# 결측치 처리
# 평균값으로 대체

#df$age의 na값을 제외한 평균값을 구한뒤 결측치에 넣음
###############
## case - 1
df$Age[is.na(df$Age)] <- round(mean(df$Age, na.rm = T),0)
## case - 2
df$Age <- ifelse(is.na(df$Age), round(mean(df$Age, na.rm = T),0), df$Age)
################
sum(is.na(df))
is.na(df) 


# 성별에 따른 생존여부
library(ggplot2)

class(df$Survived)
mode(df$Survived)
 

# 시각화를 위한 범주화
df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)

## 호실 등급에 따른 생존분류

ggplot(df, aes(x=Survived, fill =Pclass)) +
  geom_bar()

ggplot(df, aes(x=Survived, fill =Pclass)) +
  geom_bar(position = "dodge")

# 성별에 따른 생존 빈도확인
table(df$Survived, df$Sex)

ggplot(df, aes(x=Survived, fill =Sex)) +
  geom_bar()

#Pclass	선실의 등급에 따른 생존여부

#분류모델
names(df)
ta <- df[,c("Pclass","Sex","Age","Survived")]
head(ta)

# 성별데이터를 수치로 치환함
ta$Sex <- ifelse(ta$Sex == "male", 1, 2) 

#학습데이터와  테스트 데이터 나누기
x <- sample(1:nrow(ta), 0.7 * nrow(ta))
x

train <- ta[x, ] #x에 해당하는 데이터
test <- ta[-x, ] #x에 해당하지 않는 데이터
nrow(train)
nrow(test)

# 학습
install.packages("party")
library(party)
names(ta)

View(train)

model1 <- ctree(Survived ~ Pclass + Sex + Age ,data = train)
plot(model1)

# 예측
pred1 <- predict(model1, test)

#혼돈행렬
t1 <- table(test$Survived, pred1)
t1

#accuracy
acc1 <- (t1[1,1] + t1[2,2]) / sum(t1)
acc1
