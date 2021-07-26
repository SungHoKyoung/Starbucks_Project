library(dplyr)
install.packages("lawstat")
library(lawstat)

options("scipen" = 100)

## 1. 남성과 여성의 소비금액에 대한 검정
## H0 : 남성과 여성의 소비금액은 차이가 없다. vs H1 : 소비금액의 차이가 있다. 

female_buy <- read.csv('C:/hnooee/pythonwork/midproject/female_buy.csv')
male_buy <- read.csv('C:/hnooee/pythonwork/midproject/male_buy.csv')

female_buy <- female_buy[,2]
male_buy <- male_buy[,2]

var.test(female_buy, male_buy)

## 두 집단은 분산이 다르다.

t.test(female_buy, male_buy, alternative = 'two.sided', mu = 0, paired = FALSE, var.equal = FALSE)


## 남성과 여성의 소비금액에는 명확한 차이가 있다.




## 2. 여성의 연령에 따른 소비금액 분포의 검정
## H0 : 연령에 따른 소비금액의 차이가 없다. vs H1 : 연령에 따른 소비금액의 차이가 있다.

aov_data <- read.csv('C:/hnooee/pythonwork/midproject/aov_data.csv')
aov_data <- aov_data[,-1]

bartlett.test(total_buy~age_units10, data=aov_data)
## 등분산 검정 결과, 데이터가 등분산성을 만족하지 못함, 따라서 비모수적인 검정인 kruskal-walis test를 진행

kruskal.test(total_buy~age_units10,data=aov_data)

## 귀무가설 기각, 연령대 간 평균 소비금액에는 차이가 있다.

age_20 <- aov_data %>% filter(age_units10 == 20)
age_20 <- age_20[,2]
age_30 <- aov_data %>% filter(age_units10 == 30)
age_30 <- age_30[,2]
age_40 <- aov_data %>% filter(age_units10 == 40)
age_40 <- age_40[,2]
age_50 <- aov_data %>% filter(age_units10 == 50)
age_50 <- age_50[,2]
age_60 <- aov_data %>% filter(age_units10 == 60)
age_60 <- age_60[,2]
age_70 <- aov_data %>% filter(age_units10 == 70)
age_70 <- age_70[,2]
age_80 <- aov_data %>% filter(age_units10 == 80)
age_80 <- age_80[,2]
age_90 <- aov_data %>% filter(age_units10 == 90)
age_90 <- age_90[,2]


wilcox.test(age_60,age_90, alternative = 'greater')
wilcox.test(age_50,age_90)

pval = 0.00138

pval_50 = c(0.000000003888, 0.0000003386, 0.00000512, 0.07044, 0.009719, 0.3431, 0.009502)
pval_60 = c(0.0000002646, 0.00002646, 0.0006872, 0.9296, 0.1339, 0.7319, 0.05491)
pval_old = c(0.1409, 0.01944, 0.6862, 0.019)

# t.test(age_50, age_90, alternative = 'two.sided', mu = 0, paired = FALSE, var.equal = FALSE)

## 50대 이상에서는 평균 소비금액이 같다. 20~40대와 50대 이상 간의 소비금액의 차이가 난다.


## 3. 50~60대 여성의 각 프로모션 참여의 분포에 대한 적합성 검정
## H0 : 50~60대 여성의 각 프로모션 참여 기대값은 같다. vs 
## H1 : 기대값은 다르다, 특정 프로모션에 대한 참여 기대값이 더 높다

f_50_60_recevied <- read.csv('C:/hnooee/pythonwork/midproject/f_50_60_recevied.csv')
f_50_60_recevied <- f_50_60_recevied[,2]

prob = rep(0.125, 8)

chisq.test(f_50_60_recevied, p=prob)

## 각 프로모션 참여 기대값은 같다.




## 4. 전체 50~60대 고객의 각 프로모션 참여의 분포에 대한 적합성 검정
## H0 : 전체 50~60대의 각 프로모션 참여 기대값은 같다. vs
## H1 : 기대값은 다르다, 특정 프로모션에 대한 참여 기대값이 더 높다.

age_50_60_received <- read.csv('C:/hnooee/pythonwork/midproject/age_50_60_received.csv')
age_50_60_received <- age_50_60_received[,2]

chisq.test(age_50_60_received, p=prob)

## 각 프로모션 참여 기대값은 같다.




## 5. 신규고객이 선호하는 프로모션에 대한 검정
## H0 : 신규고객의 각 프로모션 참여 기대값은 같다. vs H1: 기대값은 다르다. 특정 선호 프로모션이 존재한다.

group_2018 <- read.csv('C:/hnooee/pythonwork/midproject/group_2018.csv')
group_2018 <- group_2018[,2]

chisq.test(age_50_60_received, p=prob)


## 각 프로모션 참여 기대값은 같다.