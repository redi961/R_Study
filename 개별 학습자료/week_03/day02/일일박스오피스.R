#json 라이브러리 불러오기
install.packages("jsonlite")
library(jsonlite)

#영화진흥위원회 -> 일일 박스오피스 json 자료 주소값 설정
# paste를 통하여 붙여적음 / sep 빼먹을시 api / dt값 사이에 공백이 한칸 생기므로 잊지말것
apikey <- "f5eef3421c602c6cb7ea224104795888"
dt <- "20221220"
url <- paste("http://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?",
             "key=", apikey,
              "&targetDt=",dt, sep="")

url

mv <- fromJSON(url)
mv

View(mv)

#박스오피스 목록 추출
BoxofficeList <- mv$boxOfficeResult$dailyBoxOfficeList
View(BoxofficeList)



names(BoxofficeList)
str(BoxofficeList)

col <- c("rnum", "rank", "rankInten",
         "salesAmt","salesShare","salesInten",
         "audiCnt","audiInten","audiChange",
         "audiAcc","scrnCnt","showCnt")

for (c in col) {
  BoxofficeList[c] <- as.numeric(unlist(BoxofficeList[c]))
}

View(col)


## 라이브러리 
library(dplyr)

# 매출평균보다 매출이 높은 영화 
View(BoxofficeList)

target <- BoxofficeList %>% 
  filter(salesAmt >= mean(salesAmt)) %>%
  select(movieNm)

target

View(target)

# Return 괄호 까먹지 말것
daily <- function(dt) {
  apikey <- "f5eef3421c602c6cb7ea224104795888"
  url <- paste("http://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?",
               "key=", apikey,
               "&targetDt=",dt, sep="")
  
  mv <- fromJSON(url)
  
  BoxofficeList <- mv$boxOfficeResult$dailyBoxOfficeList
  return (BoxofficeList)
}


daily(20121220)
