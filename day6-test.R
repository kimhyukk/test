library(dplyr)

# 1. for문으로 다음과 같이 월 이름을 출력
# The month of January
# ...
# The month of December

for (i in month.name) {
  res <- paste("the month of",i)
  print(res)
}




# 2. 짝수이면 TRUE, 홀수이면 FALSE를 출력하는 함수 작성.
# 다음 벡터로 테스트하시오.
# c(-5:5)

even <- function(x){x%%2==0}

even(c(-5:5))
 
# 3. 짝수 개수를 세는 함수 작성.
# 다음 벡터로 테스트하시오.
# c(-5:5)

length(c(-5:5)[even(c(-5:5))])
 
# 4. 주어진 숫자가 원주율보다 크면 TRUE, 아니면 FALSE를 출력하는 함수 작성.
# 3과 1:5 벡터에 대해 테스트하시오
func1 <- function(x){x>pi}

func1(c(3,1:5))

# 5. 주어진 그림과 같은 데이터프레임을 생성하여
# df_midterm에 저장하시오
english=c(90,80,60,70),
math=c(50,60,100,20),
class=c(1,1,2,2)

df_midterm <- data.frame(english=english,math=math,class=class)

df_midterm
# - 각 과목별 평균을 구하시오.
colMeans(df_midterm[,c(1,2)])

# - 각 번호별 평균을 구하시오.

rowMeans(df_midterm[,-3])

df_midterm %>% 
  mutate(mean_eng_math=(english+math)/2) %>% 
  select(mean_eng_math)
 
# 6. 2~99까지 수에 대해
# - 3의 배수에 해당하는 수의 합계를 구하시오.
# - 3의 배수에 해당하는 수의 개수를 구하시오.

sum(c(2:99)[c(2:99)%%3==0])

length(c(2:99)[c(2:99)%%3==0])

# 7. 임의의 수 n을 전달받아, n!을 출력하는 함수를 완성하시오. (n>=2, 5!=5*4*3*2*1)

func2 <- function(x){factorial(x)}

func2(5)

# 
# 8. 반복문을 이용하여 구구단을 출력하시오
for (i in 2:9){
  v <- c(1:9)
  ggd <- paste(i,"*",v,"=",i*v)
  print(ggd)
}



# 
# 9. 반복문을 활용하여 출력하시오
#   *
#   ***
#   *****
#   *******

for (i in seq(1,7,2)){
  s <- rep("*",i)
  print(s)
}

#   10.  타이타닉 데이터 전처리
# - train.csv 파일 읽을 때 "" 는 na로 처리하시오.
# - Surived 컬럼의 타입을 확인하시오.
# - Survived 컬럼의 타입을 factor 타입으로 바꾸어 저장하시오

df <- read.csv("train.csv",na.strings = "") 


str(df$Survived)

df$Survived <- factor(df$Survived)

str(df$Survived)

# 11. mtcars데이터 weight열 추가, 무게가 중위수보다 큰 자동차는 heavy, 그렇지 않은 자동차는 light를 저장
# - 각 종류별 데이터 건수 출력, 비율

median(mtcars$wt)
mtcars %>% 
  mutate(weight=ifelse(mtcars$wt>median(mtcars$wt),"heavy","light"))


# 12. mtcars 데이터셋 열들이 왼쪽에서 오른쪽으로 오름차순으로 배치되도록 작성

mtcars[,order(colnames(mtcars))]


# 13. iris데이터에서 70% 데이터를 무작위 표본추출

iris_ran <- sample(1:nrow(iris),nrow(iris)*0.7)

iris[iris_ran,]


# 14. 타이타닉 데이터 분석
# -타이타닉 데이터 불러오기

df <- read.csv("train.csv",na.strings = "")

# - 생존자 수, 사망자 수 출력

table(df$Survived)

# - pclass, embarked 별 승객수 출력(비율)

round(table(df$Pclass)/nrow(df)*100,2)


round(table(df$Embarked)/nrow(df)*100,2)

# - Name에서 호칭 종류 출력, 호칭 종류 별 승객수 출력




# -age열의 구간별 인원수 출력
# 10대 미만, 10대, 20대, 30대, 40대, 50대 이상

table(cut(df$Age,breaks = c(0,10,20,30,40,50,Inf),right = F))

# -cabin 컬럼의 1번째 글자 출력(NA는 제외)

df_cabin <- df %>% 
  filter(!is.na(df$Cabin))

df_cabin$Cabin

substr(df_cabin$Cabin,1,1)

# - fare열 값에 대해 최대/최소/평균/표준편차(sd) 출력
min(df$Fare)
max(df$Fare)
mean(df$Fare)
sd(df$Fare)
# - sibsp + parch를 더하여 새롭게 family열에 저장

df <- df %>% 
  mutate(family=SibSp+Parch)
