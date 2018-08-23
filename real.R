test2 

<<<<<<< HEAD
# Ch09 Homework

# 레알 데이터 분석 시작!
# 한국인의 삶을 파악해요

library(foreign)  # SPSS 읽는 친구
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare = read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)

# 언제나 복사본을 만들어야죠!
welfare = raw_welfare

head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

# 흠.. 알 수 없는 코드로 되어있음.
# 알아보기 쉽게 변수명 수정!

welfare = rename(welfare,
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7)

# 본격 데이터 분석!
# 1단계 : 전처리(결측치, 이상치)

# 성별에 따른 분석

class(welfare$sex)  # 수치로 되어있구만!
table(welfare$sex)  # 1과 2로만 잘 되어 있군!

# 혹시나 이상치 있을 때!
welfare$sex = ifelse(welfare$sex == 9, NA, welfare$sex)

# 결측치 확인
table(is.na(welfare$sex))  # 깔끔하고만!

# 인간의 언어로!
welfare$sex = ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

# 수입에 따른 분석

class(welfare$income)  # 숫자군!
summary(welfare$income)  # 아주 유용하군. 결측치(NA's)도 바로 알 수 있음. NA's가 12030나 되넴..
is.na(welfare$income)
table(is.na(welfare$income))  # 위와 같이 12030!

# 일단 좀 봅시다.
qplot(welfare$income)  # 그래프가 왼쪽으로 아주 쏠려있군. 중간 오른쪽이 텅텅비었어. -> 결측치인듯! 저 12030들!

# x축을 제한해보장
qplot(welfare$income) +
  xlim(0,1000)  # 이뻐짐. 좀 더 잘 보임.

summary(welfare$income)
table(is.na(welfare$income))
welfare$income

# 이상치를 결측치로 처리하장
welfare$income = ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
# income이 0이나 9999면 결측치로.

table(is.na(welfare$income))

# 2단계 : 변수 간의 관계 분석

# 성에 따른 수입을 분석해보자.
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
# 일단 welfare에서 income 결측치는 제외하고, 성별로 구분하고, income 평균 보여줭!

sex_income
# female : 162, male : 312
qplot(sex_income)  # 오잉 요건 안 먹네!!!!!!!!! 빈도만 되는듯? sex_income은 축을 정해줘야 되는듯?

ggplot(data = sex_income, aes(x = sex, y = mean_income)) +
  geom_col()

# 이번엔 나이에 따라 분석해보장.

class(welfare$birth)  # 역시 숫자구만.
summary(welfare$birth)
qplot(welfare$birth)
table(is.na(welfare$birth))

# 혹시나 이상치(ex.9999) 있다면,
welfare$birth = ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

# 우리나라 나이 계산하기
welfare$age = 2018 - welfare$birth +1
summary(welfare$age)
qplot(welfare$age)
# 오잉 초고령사회구만

# 연령별 수입 분석해보자!
age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

head(age_income)

# 그림으로 파악해보자
ggplot(data = age_income, aes(x = age, y = mean_income)) +
  geom_line()

# 나이말고, 연령대별로 분석해보자!

# 연령대 분류1
welfare = welfare %>%
  mutate(ageg = ifelse(age < 30, "young", ifelse(age <= 59, "middle", "old")))

table(welfare$ageg)
qplot(welfare$ageg)
# 수업자료랑 결과 수치다름
# 나이 계산을 2018년도 기준으로 해서유~

# 연령대 분류2 - 한번더 연습~
welfare = welfare %>% 
  mutate(ageg2 = ifelse(age < 20, "young", ifelse(age < 30, "20s", ifelse(age < 40, "30s", ifelse(age < 50, "40s", ifelse(age < 60, "50s", ifelse(age < 70, "60s", ifelse(age < 80, "70s", "elderly"))))))))

table(welfare$ageg2)
qplot(welfare$ageg2)

# 연령대별로, 성별로 수입은 어떨까?
sex_income = welfare %>% 
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))
# 막대그래프로 그려주는데말이지, x축 순서(scale_x_discrete) 를 영,미들,올드 순으로 해줭. fill은 색깔인듯?

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))
# 이번엔 성별로도 분류해서 보여줭!

# 3단계 : 분석표 그래프 그리기!

# 연령별 성별 월급 평균표 만들기
# 참고로, 조금 아까전에는 연령대(그룹)별 성별 이였오.
sex_age = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

head(sex_age)

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) +
  geom_line()
# 와우. 그룹이아니라 연속된 수라서 라인으로 그려야겠쥬


# 비교샷
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) +
  geom_line()


# 역시 그림을 그리니 한 눈에 잘 보인다능. 여성은 30대 이상은 더 이상 올라가지 않네여.

class(welfare$code_job)  # 수치!
table(welfare$code_job)  # 코드가 와방 많네

# 사람의 언어로 바꾸어줍시다!
# 엑셀파일에 코드별 직업이름 활용!

list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)  # 첫 행에는 이름 맞구요(T), 시트는 두번째(2)꺼로 쓸게요.

head(list_job)
dim(list_job)  # 149행 2열(code_job이랑 job(한글 이름))

# 쪼인 시켜줘요!
welfare = left_join(welfare, list_job, id = "code_job")
# 교집합은 code_job을 기준으로! 해체모엿!

head(welfare$job)
table(welfare$job)

# 직업이름 없는 친구들도 꽤 있군! 제외하면,
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

# 직업이름이랑 수입 없는 친구들(결측값) 제외하고 분석해 보자!
# 우선 새로운 변수로 데이터셋 추리고,
job_income = welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income)

top10 = job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

top10

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()

# 숙제 끝
# 왜 안 올라가니..
=======
  # Ch09 Homework
  >>>>>>> d256340b3cee7242268f912b3a00b4a8ddffc4c4
