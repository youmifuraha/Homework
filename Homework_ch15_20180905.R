# Homework_ch15 것!

# 15-1 R내장 함수로 데이터 추출

# 데이터 준비
exam = read.csv("csv_exam.csv")
exam[]  # = exam 같음

## 행으로 추출
# 1행만 추출
exam[1,]

# 2행 추출
exam[2,]

# 1반인 칭구들만
exam[exam$class == 1,]  # 행에 해당하는 조건이니깐, 쉼표 앞에만 조건 부여!

# 수학점수 80 이상
exam[exam$math >= 80,]  # 콤마빼먹지 말긔!!

# 1반이면서 수학점수 50 이상
exam[exam$class == 1 & exam$math >= 50,]
# 콤마빼먹지 말긔!!!!!!

# 영어점수 90 미만 이거나 과학점수 50점 미만
exam[exam$english < 90 | exam$science < 50,]

## 열 변수 추출

# 첫번째 열만
exam[,1]  # id 였지!

# 두번째 열만
exam[,2]  # class 였지!

# 세번쨰 열만
exam[,3]  # math 점수

### 근데 변수명이 같이 안 출력되서 불편하구만...

# 변수명으로 변수 추출하기

# class 변수
exam[,"class"]
 
# math 변수
exam[,"math"]

### 참고. 행 값 추출할 때는 '원본데이터$변수명'식으로 추출해야되는디, 열 값(변수) 추출할 때는 걍 "변수명"이구만.

# class, math, english 추출
exam[,c("class", "math", "english")]
# c로 묶어서 출력!!

# 열 쪽은 일일이 세기 뭐하니깐, 이름으로 찾는게 좋으다!!

## 행, 열 동시 추출

# 1행 3열?
exam[1,3]

# 5행의 영어점수?
exam[5,"english"]

# 수학점수 50 이상 친구들의 수학 점수랑 영어 점수 -> 아니네!
exam[exam$math >= 50, "english"]
### 어허.... 이게 아니네. 
### 수학 점수 50 이상 친구들의 영어점수네!
### 수학 점수는 안 나옴.
### 걍 수학 점수는 조건으로 만됨. 수학점수는 조건만족시키는 행이되는거고, 값은 열값인 english 점수가 나옴.

# 수학점수 50 넘는 친구들의 영어, 과학 점수
exam[exam$math >= 50, c("english", "science")]

exam

exam[exam$math >= 50, c("english", "science")]
# 자리 바꾸면?
exam[exam$math >= 50, c("science", "english")]
# 바꿔서 출력

# 수학 추가하면?
exam[exam$math >= 50, c("math", "science", "english")]
# 수학 점수도 포함해서 출력~!


## dplyr랑 비교해보장

library(dplyr)

# 수학 점수 50이상, 영어 점수 80이상, 각 반의 전 과목 총평균?

# 내장함수 방법
exam$aver = (exam$math + exam$english +exam$science)/3
aggregate(data = exam[exam$math >= 50 & exam$english >= 80,], aver~class, mean)
# aggregate 함수는 dplyr 에서 group_by 같은 칭구. data는 주어진 주건에 만족하는 애들만 씀. 그리고, aver~class: 반에따른 평균!, mean = 평균값 구해줘 함수.

# dplyr 방법
exam %>% 
  filter(math >= 50 & english >= 80) %>% 
  mutate(aver2 = (math + english + science)/3) %>% 
  group_by(class) %>% 
  summarise(mean = mean(aver2))

# dplyr 하는게 더 직관적! 글구 내장꺼는 dataset안에 변수 쓸 때마다 그 dataset이름하고 $변수 해줘야되서 더 복잡함!


# 혼자서 해보기

library(ggplot2)
mpg

mpg$total = (mpg$cty + mpg$hwy) / 2
mpg

# 통합연비변수 생성
mpg$total = (mpg$cty + mpg$hwy) / 2

# compact 추출
df_comp = mpg[mpg$class == "compact",]

# suv 추출
df_suv = mpg[mpg$class == "suv",]

# 각 평균값
mean(df_comp$total)
mean(df_suv$total)


## dplyr 로 구해보기

library(dplyr)

mpg %>%
  mutate(aver = (cty + hwy) / 2) %>% 
  filter(class == c("compact", "suv")) %>%
  group_by(class) %>% 
  summarise(mean = mean(aver))

# 간단&직관적


## 변수 타입

# 연속 변수 vs 범주 변수

var1 = c(1, 2, 3, 1, 2)  # 연속
var2 = factor(c(1, 2, 3, 1, 2))  # 범주

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

# array 만들기 1~20으로 2행*5열*2차원
# 2차원 이상!!
# 근데 얘도 한 가지 형태로밖에 안 됨!!!
x3 = array(1:20, dim = c(2, 5, 2))
x3
class(x3)

# so, 제일 만만한게 데이터프레임!!

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
