# review
# P값은 0.5보다 작을 때 의미있다!★★★

# t-test

# 귀무가설 vs 대립가설
# 귀무가설 : null hypertesis 
# ex. 타자속도 변하지 않을 것이다.
# 대립가설 : altenative(anti) hypetesis
# ex. 타자속도 변할 것이다.

# 귀무가설을 기각한다! 요게 좋은겨! 긍까 대립가설을 살린다!

mpg = as.data.frame(ggplot2::mpg)
library(dplyr)

View(mpg)

# compact와 suv의 도시 연비를 비교해보고 싶오.

# 평균을~ 근데 얘가 알아서 다 해준당.

mpg_diff = mpg %>% 
  select(class, cty) %>% 
  filter(class %in% c("compact", "suv"))

head(mpg_diff)

table(mpg_diff$class)

t.test(data = mpg_diff, cty ~ class, var.equal = T)

# ~ : 물결무늬
# var.equal : 퍼져있는 정보가 동일하다고 가정하는거. T. True. 근데 대부분이 True. 대부분 TRUE. 

# 결과 : p-value < 2.2e-16
# 항상 p값이 결론임. 딱 p만 보면됨!!!

# 딱 결론!!!!!
# 우선 p가 0.5보다 작지!! 그럼 의미가 있다!!!!!!! 끗@@
# 통계적으로 유의하다! 상관관계는? 두번째 시간에~

library(ggplot2)
midwest
View(midwest)

midwest_s = midwest %>% 
  select(state, poptotal) %>% 
  filter(state %in% c("IL", "MI"))

head(midwest_s)

table(midwest_s)

t.test(data = midwest_s, poptotal ~ state, var.equal = T)
# 문법쓸때, state에 따른 poptotal이 알고 싶은거니깐, 순서가 확실히 있음! poptotal ~ state!!

# p값이 0.9991로 0.5보다 크므로, 통계적으로 유의하지 않다.
# 99.91% 거의 전혀 상관이 없다~!
# 실제로 평균값도. IL는 112064.7, MI는 111991.5입니당. 거의 차이 엄쪙.

## 더 많이 쓰는 T검정~~

# 혼자 해보기~~

before = c(52, 60, 63, 43, 46, 56, 62, 50)
after = c(58, 62, 62, 48, 50, 55, 68, 57)

typing = data.frame(before, after)
typing

?t.test
# 노노 t.test(data = typing, after ~ before, paired = T, var.equal = T)

t.test(before,after, paired = TRUE)
# paired: 짝꿍으로 같은 사람이 딱 일치되게 한 거다. 시험 전. 후. 
# 

# p값이 0.0166 로 0.5보다 작다!!! 유의하다!!! 기무가설을 기각한닷!(아까 기무가, 두개가 같을 것이다. 전이든 후든.)

a = c(52, 60, 63, 43, 46, 56, 62, 50)
b = c(58, 62, 62, 48, 50, 55, 68)
t.test(a,b, paired = TRUE)
# 길이 좀 달라도 된댔는데, 어차피 평균이고, 중간에 전학갈수도 있으니깐. 흠. 근데 안되네여. Error in complete.cases(x, y) : 모든 인자들이 같은 길이를 가지고 있지 않습니다


## 다음주 화요일에 보충있어여~~


# 상관관계
# 상관분석
# 부 변수가 관계있는지!

# 0~1 사이의 값을 지니고 1에 가까울수록 관련성이 크다는 의미
# 상관계수가 양수면 정비례, 음수면 반비례 관계

economics = as.data.frame(ggplot2::economics)

View(economics)

# 혹시나 데이터셋의 변수 모르겠다?!?!?!?!!? 헬프를 봐여!!!!
?economics

# 실업과 개인소비
cor.test(economics$unemploy, economics$pce)

# p값이 2.2e-16 로 0.5보다 훨작다! 완전 유의하군!!

# 지금한건 두 변수간에 상관관계.
# 요번엔 여러변수를 한번에 비교분석해보자!!! 어떤 관계가 있는지. 관계가 큰지 작은지도!!!

mtcars
head(mtcars)

# 우선 상관행렬을 만들어야해여.
car_cor = cor(mtcars)
car_cor

# 어우 소수자리수가 넘 많다얘

round(car_cor, 2)

# 아하. 상관계수인가봄.
# mpg(mile per galon) 이랑 mpg는 1~~

# 오잉 파악하기 어려웡. -> 그림으로 그려보쟝~~

install.packages("corrplot")
library(corrplot)

corrplot(car_cor)
# 올~~~

# 요것도 눈에 잘 안 들어오낫?
# 또다른 방법!!!

corrplot(car_cor, method = "number")

# 좀 더 더 보기 편하게~
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(car_cor,
         method = "color",
         col = col(200),
         type = "lower",
         order = "hclust",
         addCoef.col = "black",
         t1.col = "black",
         t1.srt = 45,
         diag = F)

# Wool 데이터를 이용하여 두 변수의 관계성을 분석해 보세요
# • len 와 cycles 의 상관관계를 분석하여 2 줄 이내로 해석하세요.
# • 모든 변수의 상관 관계를 나타낸 관계 행렬을 만드세요.(소수 두자리까지만)
# • 관계 행렬을 사각형 히트맵으로 만드세요

library(carData)

Wool
View(Wool)
?Wool

cor.test(Wool$len, Wool$cycles)
# P값이 0.0005244으로 0.5보다 작으므로 매우 유의! 상관관계는 0.6225827

Wool_cor = cor(Wool)
Wool_cor

round(Wool_cor, 2)

?corrplot

corrplot(Wool_cor, order = "hclust")
corrplot(Wool_cor, order = "hclust", hclust.method = "ward.D2", addrect = 4)

# 사각형 히트맵으로!!
?corrplot 에서 method 부분 봐보장. method에서 color대신 square를 넣으면 되겠군!!
  
  corrplot(Wool_cor,
           method = "color",
           col = col(200),
           type = "lower",
           order = "hclust",
           addCoef.col = "black",
           t1.col = "black",
           t1.srt = 45,
           diag = F)


corrplot(Wool_cor,
         method = "square",
         col = col(200),
         type = "lower",
         order = "hclust",
         addCoef.col = "black",
         t1.col = "black",
         t1.srt = 45,
         diag = F)



corrplot(car_cor,
         method = "square")
