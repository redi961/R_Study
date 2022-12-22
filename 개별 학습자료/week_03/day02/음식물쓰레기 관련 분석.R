#json 라이브러리 불러오기
install.packages("jsonlite")
library(jsonlite)

sKey <- "TD9CtocYik0qfKi%2Fppbc8NgpHbn8qy5tIWz0JrY4eOajiGLrFHmz3jlDND4K0mK8JIdIBl34hmgrjQc4BZ7apA%3D%3D"
readType <- "json"
year <- "2021"
month <- "08"

## https를 입력시 오류가 남 http로 사용할것

url <- paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDateList?",
             "ServiceKey=",sKey,
             "&type=",readType,
             "&page=1&rowNum=7",
             "&disYear=",year,
             "&disMonth=",month,sep="")
url

trash <- fromJSON(url)

df <- trash$data$list
df

View(df)
