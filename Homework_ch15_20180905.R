#

# 자주 실수하는 것!

exam = read.csv("csv_exam.csv")
exam[]
# 행과 열을 다룰 때는 무조건 대괄호 [] [행,열]

# 1행만 추출
exam[1,]
exam[exam$class == 1,]

exam[exam$math >= 80,]

twitter = read.csv("twitter.csv")
twitter

exam[exam$class == 1 & exam$math >= 50]
exam[exam$english < 90 | science <50]

exam[,1]


exam[,"class"]
exam[,"math"]

# 열 쪽은 일일이 세기 뭐하니깐, 이름으로 찾는게 좋으다.

exam[exam$math >= 50, "english"]
exam

exam[exam$math >= 50, c("english", "science")]

# 지금까지는 그냥 내장해서 뽑아본거죵

# dplyr 로 해보쟝

install.packages("dplyr")
library(dplyr)

# 아래 두개 코드 방법 비교!
exam$tot = (exam$math + exam$english +exam$science)/3
aggregate(data = exam[exam$math >= 50 & exam$english >= 80,], tot~class, mean) # 요게 group_by

exam %>% 
  filter(math >= 50 & english >= 80) %>% 
  mutate(tot = (math + english + science)/3) %>% 
  group_by(class) %>% 
  summarise(mean = mean(tot))

# dplyr 하는게 더 직관적! 글구 내장꺼는 dataset안에 변수 쓸 때마다 그 dataset이름하고 $변수 해줘야되서 더 복잡함!


# 혼자서 해보기

install.packages("ggplot2")
library(ggplot2)
mpg = as.data.frame(ggplot2::mpg)
mpg

mpg$total = (mpg$cty + mpg$hwy) / 2
mpg

mpg[mpg$class == c("compact, suv"),]

# compact 추출
df_comp = mpg[mpg$class == "compact",]
df_suv = mpg[mpg$class == "suv",]

mean(df_comp$total)
mean(df_suv$total)

library(dplyr)


mpg %>%
  mutate(aver = (cty + hwy) / 2) %>% 
  filter(class == c("compact", "suv")) %>%
  group_by(class) %>% 
  summarise(mean = mean(aver))

# 오홋! 옆자리 친구가 알려줌 ㅎ.ㅎ




# 변수 타입

# 함수마다 적용 가능한 변수 타입이 다를 수 있다.
# 분석 전에 변수 타입 무엇인지 확인 필요
# 함수 실행시 오류가 발생하거나 예상과 다른 결과가 출력되면 변수 타입 확인할 것!!

# Satoshi Nakamoto : 혜성과 같은 사람. 
# 갑자기 논문 1편(2009, 9장짜리)(예고도 없이) 발표했는데 10년만에 터짐. 세상을 뒤짚어높음.
# 근데 실체가 없음. 기자들도 못 찾음.
# 2000만원까지 만들어놨다가 지금은 800만원.
# 이 사람이 겁나 많이(200만) 갖고 있는데 한번도 안 씀. 전혀 정체를 들어내지 않음. 
# 1.의심가는 사람있는데... 죽어서? 2.비밀번호를 찾고있다?
# block chain이란?
# 사람사이에 돈거래를 하고 싶은데, 그냥 하기 믿기 어려우니깐, 은행이 하는데. 은행이 수수료 등 막 떼.
# 은행 수수료 아까우니깐 컴퓨터가 하게끔 하는게 block chain
# 누구나 확인가능하니깐 해킹하려면 모든 사람 해킹해야해서 할 수가 없당.
# 홈페이지에 있으니깐 참고해봐영.
# 블록체인은 지금 완전 블루오션이야요. 지금이 시작임.
# 무궁무진해영.
# 장담하건데, 지금 인터넷 컴퓨터 상상할 수 없듯이, 없으면 말이 안 되듯이,
# 블록체인도 그렇게 될 것이다!!!!
# 관심을 가져봐용. 블록체인의 철학!!! 뺏긴 권력을 되찾자!!!
# Bitcoin: A Peer-to-Peer Electronic Cash System
# Satoshi Nakamoto 라는게 아마 가명인듯. 일본이 그래서 일본사람인줄알고 겁나 지원하줌.
# 작년에 한국에서 그걸(?) 가져올라고 했는데... 말아먹음.

# SNU Coin ㅋㅋㅋㅋ 선생님이 만든거ㅋㅋㅋ 그걸로 상 받으셨다능.




# ch15.
# 변수 타입

# 연속 변수 vs 범주 변수
# 연속 변수 : Numeric 타입! 값에 의미가 있다. 연산이 가능하다. ex.키, 몸무게
# 범주 변수 : Factor 타입! 검증. 문자로 대체 가능. 숫자의 크기와 전혀 상관이 없다. ex.남녀(1,2), 지역(1,2,3,4)

var1 = c(1, 2, 3, 1, 2)
var2 = factor(c(1, 2, 3, 1, 2))

var1
var2

var1+2  # 계산이 된당.
# var2+2
# Warning message: 
# In Ops.factor(var2, 2) : 요인(factors)에 대하여 의미있는 ‘+’가 아닙니다.

class(var1)
class(var2)

levels(var1) # numeric은 레벨이 없음!!
levels(var2)

var3 = c("a", "b", "b", "c")  # 문자변수 생성
var4 = factor(c("a", "b", "b", "c"))  # 문자도 factor변수로 생성가능!!

var3
var4

class(var3)
class(var4)

mean(var1)
mean(var2)  # 당연히 안됨!
mean(var3)  # 당연히 안됨!
mean(var4)  # 당연히 안됨!

# 연산 함수는 numeric만 된다~

var2 = as.numeric(var2)  # factor값을 numeric값으로 바꿔라
mean(var2)
class(var2)
levels(var2)  # 범주 확인


# 혼자해보기
class(mpg$drv)
mpg$drv = as.factor(mpg$drv)
levels(mpg$drv)

## 15-3 데이터 구조

# 스칼라 : 하나
# 벡터 : 여러개. 근데 형태는 한 종류밖에 안 됨.
# 데이터프레임은 이것저것 다 들어가눈딩

a = 1
b = "hello"

# 데이터프레임은 다 넣을 수 있당.
x1 = data.frame(var1 = c(1, 2, 3),
                var2 = c("a", "b", "c"))
class(x1)

# 매트릭스 만들기
x2 = matrix(c(1:12), ncol = 2)
x2
class(x2)
# 매트릭스 가장 큰 차이점!
# 데이터프레임은 여러가지 형태를 구성할 수 있는데,
# 매트릭스는 한가지만 가능하다!(2차원이긴 하지만)
# 그래서 매트릭스 별로 안 쓰고, 데이터프레임을 써.

# array 만들기 1~20으로 2행*5열*2차원
# 2차원 이상!!
# 근데 얘도 한 가지 형태로밖에 안 됨!!!
x3 = array(1:20, dim = c(2, 5, 2))
x3
class(x3)

# so, 제일 만만한게 데이터프레임이다~!
# ㅋㅋㅋ 데이터프레임이 진리다.

# 리스트!!
# 모든 데이터구조를 죄다 포함한다.(벡터, 어레이, 리스트, 데이터프레임 등등등)

x4 = list(f1 = a,  # 벡터
          f2 = x1,  # 데이터 프레임
          f3 = x2,  # 매트릭스
          f4 = x3)  #어레
x4
class(x4)

# 실사용을 봐보장.

mpg = ggplot2::mpg
x = boxplot(mpg$cty)
x # 오호. 리스트로 보여주는구만.
# 함수의 결과값이 리스트로 나오는게 많음!
# 함수의 결과에는 이것저것 들어가는게 많으니깐영.
# 요 값들이 궁금하면? help!
?boxplot

x$stats[,1]  # 요약 통계량 추출. 첫번째 열의 값
x$stats[,1][3]  # 첫번째 열에서 세번째(중앙값)
x$saats[,2][2]  # 1분위수


## 혼자 해보기
BOD
class(BOD)
i = boxplot(BOD)
i
i$stats[,1]
i$stats[,1][4] # 3 분위! 5개 중에서는 4번째죵
i$stats[,1][3] # 요게 중앙값일꺼규용~~

# 그래서 오늘은 실수하는거 봤어용~
# 근데 그중에서도 실수 많이 하는 것은 주로, 오타!!!!!!!! 랑 설치 안 한거!!!!!!!!!!

# 내일은 공짜로 R공부하는 사이트, 자료 받는 곳 등등 알려준데용. ㅋㅋ 처음을 같이 해 봅시당. 가입도! 그래서 앞으로 계속 공부할 수 있도록.!!