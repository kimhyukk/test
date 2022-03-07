
install.packages("foreign")
library(foreign) 

str(read.spss("Koweps_hpc10_2015_beta1.sav"))

raw_welfare<-read.spss("Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

welfare<-raw_welfare

library(dplyr) #rename함수가 포함
welfare<-rename(welfare, sex=h10_g3, birth=h10_g4, marriage=h10_g10,
                religion=h10_g11, code_job=h10_eco9, income=p1002_8aq1,
                code_region=h10_reg7)

welfare$sex<-ifelse(welfare$sex==1, "male", "female")

welfare <- welfare %>% 
  mutate(ageg=ifelse(welfare$age>60,"old",ifelse(welfare$age<30,"young","middle")))

welfare$age <- 2015-welfare$birth+1

library(readxl)
list_job <- read_excel("Koweps_Codebook.xlsx", sheet=2)

dim(list_job)

welfare <- left_join(welfare, list_job, id="code_job")

job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income1=mean(income)) 

#1. 하위 직종 10개
tail10 <- job_income %>%
  arrange(mean_income1) %>% 
  head(10)

ggplot(data=tail10, aes(x=reorder(job,-mean_income1), y=mean_income1))+
  geom_col()+
  coord_flip()
#2. 성별에 따라 어떤 직업이 많은지 추출 10개
sex_job <- welfare %>% 
  select(sex, job) %>% 
  filter(!is.na(sex)&!is.na(job)) %>%
  group_by(sex,job) %>% 
  summarise(n=n(),.groups = "drop_last") %>% 
  arrange(desc(n))

male_job_top10 <- sex_job %>% 
  filter(sex=="male") %>% 
  head(10)

female_job_top10 <- sex_job %>% 
  filter(sex=="female") %>% 
  head(10)

top10 <- rbind(male_job_top10,female_job_top10)

top10

#남
ggplot(data=male_job_top10, aes(x=reorder(job,-n),y=n))+
  geom_col()+
  coord_flip()

#여
ggplot(data=female_job_top10, aes(x=reorder(job,-n),y=n))+
  geom_col()+
  coord_flip()

#남여같이
ggplot(data=top10, aes(x=reorder(job,-n),y=n,fill=sex))+
  geom_col()+
  coord_flip()

#3. 종교 유무에 따른 이혼률
welfare$religion <- ifelse(welfare$religion==1,"O","X")


rel_mrg <- welfare %>% 
  select(religion,marriage) %>% 
  filter(marriage==3|marriage==1) %>%
  group_by(religion,marriage) %>% 
  summarise(n=n(),.groups = "drop_last")

rel_mrg

ggplot(data=rel_mrg, aes(x=religion, y=n, fill=marriage))+
  geom_col()

#4. 연령대(y,m,o) 및 종교 유무에 따른 이혼률 조사
ageg_rel_mrg <- welfare %>% 
  select(religion,marriage,ageg) %>% 
  filter(marriage==3|marriage==1) %>% 
  group_by(religion,ageg,marriage) %>% 
  summarise(n=n(),.groups = "drop_last")

O <- ageg_rel_mrg %>% 
  filter(religion=="O")
X <- ageg_rel_mrg %>% 
  filter(religion=="X")

ggplot(data=O, aes(x=ageg, y=n, fill=marriage))+
  geom_col()

ggplot(data=X, aes(x=ageg, y=n, fill=marriage))+
  geom_col()

#어려운문제
#5. 노년층이 많이 사는 지역은?

welfare$code_region

list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))

#list_region 과 welfare를 연결


welfare <- left_join(welfare,list_region,id="region")

welfare$region


reg_old <- welfare %>% 
  filter(ageg=="old") %>% 
  group_by(region) %>% 
  summarise(n=n()) %>% 
  arrange(n)

ggplot(data=reg_old,aes(x=reorder(region,-n),y=n))+
  geom_col()
