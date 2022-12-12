# 해결문제 
# BMI는 몸무게와 키를 이용하여 체지방율을 측정하는 지수이다. 
# 자신의 몸무게와 키를 각각 변수 weight와 height에 저장하고 BMI지수를 계산해 본다. 
# 단, 키는 cm로 입력 받아서 처리한다.
# 
# BMI = 체중(kg) / (키(m) x키(m))


 
# 키와 몸무게 scala입력
## 한개의 변수에 한개의 값이 있는경우 -> scala상태 // 여러값이 있는경우 -> vector
height <- scan() # scala
weight <- scan() # scala
BMI <- weight / (height/100) ** 2

# 키와 몸무게 scala입력 (Snac 1개 사용)
print("키와 몸무게 입력")
data <- scan() ## Vector
height <- data[1]
weight <- data[2]
BMI <- weight / (height/100) ** 2

# 몸무게 수치 변환
data <- readline() # 문자열 형태(char)로 한줄 변수 기록록
data
mode(data)


# BMI 계산
 

# 키와 몸무게 vector입력


#문자열 입력
 

# stringr 패키지 설치
install.packages(("stringr"))
library(stringr)

# 문자열 분리
data <- strsplit(data, split = ' ') #data 문장의 빈칸을 기준으로 문장을 나눔 / 결과값은 List로 저장됨

# 자료형 확인
mode(data) # 자료형 확인

# 벡터로 형변환
data <- unlist(data) # List 해제 후 벡터로 전환
data

# 벡터 확인
is.vector(data)

# 숫자벡터로 변경
data <- as.numeric(data) # 문자를 숫자로 전환함
data
height <- data[1]
weight <- data[2]
BMI <- weight / (height / 100) ** 2

# 데이터프레임 입력
df <- data.frame()
mode(df)
class(df) # 데이터 프레임 확인

df <- edit(df) # 데이터 프레임 테이블 확인가능
df

# 데이터프레임 열명 변경
names(df) <- c("키", "몸무게") # 프레임 열 이름 설정
df$BMI <- df$몸무게 / (df$키/100) ** 2
df

# 해결문제 
# 국민건강보험공단 자료를 이용하여 BMI와 비만도를 구하시오.
# 비만도 
# 저체중	20 미만
# 정상	20 - 24
# 과체중	25 - 29
# 비만	30 이상


########################## 입력 및 확인 과정 
# read의 경로는 상대경로로 지정함
#setwd() ## 디렉토리 위치를 조정해야하는 경우
getwd()
BTable <- read.csv('01_국민건강보험공단500.csv', header = T, fileEncoding = "euc-kr") 
BTable
mode(BTable)
class(BTable)
########################## 
head(BTable) # 상위 5개항목만 출력
BTable$BMI <- BTable$체중 / (BTable$신장/100) ** 2
BTable$BMI = round(BTable$BMI, 2) ## 소수점 2개까지만 출력력
BTable
is.numeric(BTable$BMI)
######################### ifelse (조건 , True의경우, False의 경우) 조건문 설정
BTable$비만도 <- ifelse(BTable$BMI < 20 , "저체중", 
                     ifelse(BTable$BMI >= 20 & BTable$BMI <=24, "정상",
                            ifelse(BTable$BMI >= 25 & BTable$BMI <= 29 , "과체중", "비만" )))

BTable

# 빈도 테이블
table(BTable$성별)
bindo <- table(BTable$성별, BTable$비만도) # 테이블 상태만으로도 저장이 가능한것 확인됨
bindo

# 빈도 테이블 저장
## quote = F ====> 저장 과정간에 "따옴표" 삭제

write.csv(table(BTable$성별, BTable$비만도),'빈도파악테이블.csv', fileEncoding = 'euc-kr', quote = F)
write.csv(BTable, '비만도갱신테이블.csv', row.names=FALSE, fileEncoding = "euc-kr", quote = F)


# 해결문제
# 국민건강보험 관리공단의 건강검진 자료를 이용하여 대사증후군을 판별하시오.
# 높은 혈압(130/85mmHg 이상)
# 높은 혈당(공복 혈당 100mg/dL 이상)
# 높은 중성지방(트리글리세라이드 150mg/dL 이상)
# 낮은 HDL 콜레스테롤 수치(남성은 40mg/dL 미만, 여성은 50mg/dL 미만)
# 복부 비만(남성 90cm 이상, 여성 85cm 이상)
# 판별
# 0 : 정상, 1~2 : 주의군, 3~5 : 위험군

a = 0

dfblood <- read.csv('국민건강보험공단_건강검진정보_20211229.CSV', header = T, fileEncoding = "euc-kr")
head(dfblood)

# 열명 확인
names(dfblood)

#필요한 열 추출
dfb <- dfblood[c("성별코드", "허리둘레", "수축기.혈압" , "이완기.혈압",
                 "식전혈당.공복혈당.", "트리글리세라이드", "HDL.콜레스테롤")]

#NA값 제거
dfb <- na.omit(dfb)

#열명변경
names(dfb) <- c("성별코드", "허리둘레", "수축기혈압", "이완기혈압",
                 "공복혈당", "트리글리세라이드", "HDL콜레스테롤")
dfb
head(dfb)

# 높은 혈압 (130/85mmHg 이상)
dfb$높은혈압 <- ((dfb$수축기혈압 >= 130) | (dfb$이완기혈압 >= 85))

# 높은 혈당 (공복 혈당 100mg/dL 이상)
dfb$높은혈당 <- ((dfb$공복혈당 >= 100))

# 높은 중성지방(트리글리세라이드 150mg/dL 이상)
dfb$높은중성지방 <- (dfb$트리글리세라이드 >= 150)

# 낮은 HDL 콜레스테롤 수치(남성은 40mg/dL 미만, 여성은 50mg/dL 미만)
dfb$낮은콜레스테롤 <- ((dfb$성별코드 == 1 & dfb$HDL콜레스테롤 < 40) | 
                  (dfb$성별코드 == 2 & dfb$HDL콜레스테롤 < 50))

# 복부 비만(남성 90cm 이상, 여성 85cm 이상)
dfb$복부비만 <- ((dfb$성별코드 == 1 & dfb$허리둘레 >= 90) | 
               (dfb$성별코드 == 2 & dfb$허리둘레 >= 85))

# 대사증후군 판단단
dfb$대사증후군 <- dfb$높은혈압 + dfb$높은혈당 + dfb$높은중성지방 + dfb$낮은콜레스테롤 + dfb$복부비만
dfb$판별 <- ifelse(dfb$대사증후군 == 0 , "정상", 
                 ifelse(dfb$대사증후군 <= 2, "주의군", "위험군"))
dfb$성별 <- ifelse(dfb$성별코드 == 1, "남", "여")
table(dfb$판별, dfb$성별)

View(dfb) ## 해당 테이블 View를 통하여 표시
write.csv(table(dfb$판별, dfb$성별), "성별에따른_대사증후군.csv" ,fileEncoding = "euc-kr", quote = F)
write.csv(dfb,"대사증후군판별.csv", fileEncoding = 'euc-kr', quote = F)
