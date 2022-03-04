library(dplyr)
#1. 
#mpg 데이터와 midwest 데이터를 이용해서 분석 문제를 해결해 보세요.
#• Q1. mpg 데이터의 cty(도시 연비)와 hwy(고속도로 연비) 간에 어떤 관계가 있는지 알아보려고 합니다. 
#x 축은 cty, y 축은 hwy 로 된 산점도를 만들어 보세요.

ggplot(data=mpg,aes(x=cty,y=hwy))+geom_point()



#• Q2. 미국 지역별 인구통계 정보를 담은 ggplot2 패키지의 midwest 데이터를 이용해서 전체 인구와
#아시아인 인구 간에 어떤 관계가 있는지 알아보려고 합니다. x 축은 poptotal(전체 인구), y 축은
#popasian(아시아인 인구)으로 된 산점도를 만들어 보세요. 전체 인구는 50 만 명 이하, 아시아인 인구는
#1 만 명 이하인 지역만 산점도에 표시되게 설정하세요


ggplot(data=midwest,aes(x=poptotal,y=popasian))+
  geom_point()+
  xlim(0,500000)+
  ylim(0,10000)


#2.
#mpg 데이터를 이용해서 분석 문제를 해결해 보세요.
#• Q1. 어떤 회사에서 생산한 "suv" 차종의 도시 연비가 높은지 알아보려고 합니다. "suv" 차종을 대상으로
#평균 cty(도시 연비)가 가장 높은 회사 다섯 곳을 막대 그래프로 표현해 보세요. 막대는 연비 가 높은 순으로
#정렬하세요.
mpg

mpg_new <- mpg %>% 
  group_by(manufacturer) %>%
  filter(class=="suv") %>% 
  summarise(hwy_mean=mean(hwy)) %>% 
  arrange(desc(hwy_mean)) %>% 
  head(5)
  
mpg_new

ggplot(data=mpg_new,aes(x=reorder(manufacturer,-hwy_mean),y=hwy_mean))+geom_col()


#• Q2. 자동차 중에서 어떤 class(자동차 종류)가 가장 많은지 알아보려고 합니다. 자동차 종류별 빈도를
#표현한 막대 그래프를 만들어 보세요.


mpg_class <- mpg %>% 
  group_by(class) %>%
  summarise(n=n())

ggplot(data=mpg_class,aes(x=reorder(class,-n),y=n))+geom_col()


#3. 타이타닉 데이터에 대해 작업하시오.
df <- read.csv("train.csv")
df
#• Q1.생존/사망에 따른 각 선실 등급별 빈도수 시각화
df_survived_pclass <- df %>% 
  group_by(Pclass) %>%
  filter(Survived==1) %>% 
  summarise(n=n())

ggplot(data=df_survived_pclass,aes(x=Pclass,y=n))+geom_col()

df_death_pclass <- df %>% 
  group_by(Pclass) %>%
  filter(Survived==0) %>% 
  summarise(n=n())

ggplot(data=df_death_pclass,aes(x=Pclass,y=n))+geom_col()


#• Q2. 생존/사망에 따른 나이를 10개 구간으로 나누어 시각화

df_survived_age <- df %>% 
  group_by(Age) %>%
  filter(Survived==1) %>% 
  summarise(n=n())

hist(df_survived_age$Age,breaks=10)

df_death_age <- df %>% 
  group_by(Age) %>%
  filter(Survived==0) %>% 
  summarise(n=n())

hist(df_death_age$Age, breaks=10)

#• Q3. 성별에 따라 승객수를 출력 및 시각화
df_sex <- df %>% 
  group_by(Sex) %>% 
  summarise(n=n())

ggplot(data=df_sex,aes(x=Sex,y=n))+geom_col()

#• Q4. Parch + SibSp를 더한 값으로 FamSize 열을 추가하시오
df %>% 
  mutate(FamSize=Parch+SibSp)

#• Q5. Fare열의 평균값을 출력하시오

mean(df$Fare)
