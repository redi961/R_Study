# 기상개황 자료를 이용하여 월별 불쾌지수를 계산하고 불쾌지수가 높음이상인 월을 구하시오.
# 
# https://kosis.kr/statHtml/statHtml.do?orgId=735&tblId=DT_A1040&vw_cd=MT_ZTITLE&list_id=215_215A_735_73503_A&seqNo=&lang_mode=ko&language=kor&obj_var_id=&itm_id=&conn_path=MT_ZTITLE
# 
# 불쾌지수 공식
# 
# DI = 0.81 * Ta + 0.01 * RH * (0.99 * Ta - 14.3) + 46.3
# DI: 불쾌지수
# Ta: 건구온도(평균기온)
# RH: 상대습도(평균상대습도)
# 불쾌지수 단계
# 
# 매우높음: 80이상
# 높음: 75이상 80미만
# 보통: 68이상 75미만
# 낮음: 68미만

#데이터 불러오기(기상데이터)
df <- read.csv("../week02/day01/기상개황_20221216.csv",header = T, fileEncoding = "euc-kr", stringsAsFactors = F) 
View(df)

# 필요데이터 추출 및 1행 삭제
#df <- df[c("월별.1.","평균기온....","평균상대습도....")]
df <- df[-c(1:2),]
df

# 열명 재설정
names(df) <- c("월","평균기온","평균상대습도")
df$평균상대습도 <- as.numeric(df$평균상대습도)
df$불쾌지수 <- 0.81 * df$평균기온 + 0.01 * df$평균상대습도 * (0.99 * df$평균기온 - 14.3) + 46.3

