# 불러오기!!

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare = read.spss(file = "C:/Users/Youmi/Desktop/R/Rtest/Koweps_hpc10_2015_beta1.sav", to.data.fram = T)

# 복사
welfare = raw_welfare
head(welfare)
dim(welfare)

welfare = rename(welfare,
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7)

welfare$sex = ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

list_job = read_excel("C:/Users/Youmi/Desktop/R/Rtest/Koweps_codebook.xlsx", col_names = T, sheet = 2)

welfare = left_join(welfare, list_job, id = "code_job")

# 직업과 남성
job_male = welfare %>% 
  filter(!is.na(job) & sex == "male") %>% 
  group_by(job) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
# 데이터 중에, job이 있는 것 중에, 남성있것만 추려요. 그리고 job으로 구분한다음, 숫자 써머리. 큰거부터 탑10

job_male

# 직업과 여성
job_female = welfare %>% 
  filter(!is.na(job) & sex == "female") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
# 데이터 중에, job이 있는 것 중에, 여성있것만 추려요. 그리고 job으로 구분한다음, 숫자 써머리. 큰거부터 탑10

job_female

# 그림으로 그려보장! 훨신 한 눈에 파악하기 쉬움!
ggplot(data = job_male, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()  # 요건 x, y 뒤짚기

ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()



# 종교에 관한 분석!

class(welfare$religion)  # 데이터 종류 확인
table(welfare$religion)  # 어떤 어떤 변수 있나~

welfare$religion = ifelse(welfare$religion == 1, "yes", "no")  # 인간의 언어로!
table(welfare$religion)  # 확인
qplot(welfare$religion)

# 결혼에 관한 분석 추가!
class(welfare$marriage)  # 데이터 종류
table(welfare$marriage)  # 어떤 어떤 변수 있나~

welfare$group_marriage = ifelse(welfare$marriage == 1, "marriage",
                                ifelse(welfare$marriage == 3, "divorce", NA))
# 인간의 언어로 하는데, 분석 잘 하기 위해서, 둘 중에 하나만 일단 보자! 결혼 or 이혼

table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)


# 종교와 이혼율 분석
religion_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))
# 기존 데이터에, group_marriage 기준으로, 데이터 있는 것만 추출! 종교로 분류 & group_marriage로 분류! 빈도수로 요약! 새로운 변수(총합계) 추가, 새로운 변수(퍼센트) 추가. 근데 이 퍼센트는 반올림해서 구하는데, 소수점 이하 한 자리까지해서!!

religion_marriage

# 이혼변수 
divorce = religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(religion, pct)

divorce
# 1 % 차이밖에 없군... 미미

ggplot(data = divorce, aes(x = religion, y = pct)) + geom_col()

# 연령대(그룹)과 종교 유무에 따른 이혼율
welfare$age = 2018 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

welfare = welfare %>% 
  mutate(ageg = ifelse(age < 30, "young", ifelse(age <= 59, "middle", "old")))

ageg_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))
# 본 그룹에서 필터 적용하는데, group_marriage값이 있는 것만 추출해! 그 다음 연령대로 먼저 묶고, group_marriage 순으로 묶어! 빈도수로 요약하고, 총합변수랑 퍼센트변수 새로 만들어줭!

ageg_marriage

ageg_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg, group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

ageg_divorce = ageg_marriage %>% 
  filter(ageg != "young" & group_marriage == "divorce") %>% 
  select(ageg, pct)
# 어린친구들 그룹은 분석에 필요하지 않죵
# 요래서 사람이 필요하다고요!

ageg_divorce

ggplot(data = ageg_divorce, aes(x = ageg, y = pct)) +
  geom_col()

# 연령대 및 종교 유무에 따른 이혼율!
ageg_religion_marriage = welfare %>% 
  filter(!is.na(group_marriage) & ageg!= "young") %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

ageg_religion_marriage

ageg_religion_marriage = welfare %>%
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  count(ageg, religion, group_marriage) %>% 
  group_by(ageg, religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

# 연령대 및 종교 유무별 이혼율 표!
df_divorce = ageg_religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(ageg, religion, pct)

df_divorce

# 마지막 단계! 그림을 그려보장!
ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion)) +
  geom_col(position = "dodge")

# 노년층이 많은 지역을 찾아보장!
class(welfare$code_region)
table(welfare$code_region)

list_region = data.frame(code_region = c(1:7),
                         region = c("서울", "수도권(인천/경기)", "부산/경남/울산", "대구/경북", "대전/충남", "강원/충북", "광주/전남/전북/제주"))

list_region

welfare = left_join(welfare, list_region, id = "code_region")

welfare %>% 
  select(code_region, region) %>% 
  head

region_ageg = welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 2))

head(region_ageg)

region_ageg = welfare %>% 
  count(region, ageg) %>% 
  group_by(region) %>% 
  mutate(pct = round(n/sum(n)*100, 2))

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip()
