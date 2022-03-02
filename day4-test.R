mpg

mpg_new <- mpg
##1
#Q1. mpg 데이터의 class 는 "suv", "compact" 등 자동차를 특징에 따라 일곱 종류로 분류한 변수입니다. 
#어떤 차종의 연비가 높은지 비교해보려고 합니다. class 별 cty 평균을 구해보세요.
mpg_new %>% 
  group_by(class) %>% 
  summarise(cty_mean=mean(cty))

#Q2. 앞 문제의 출력 결과는 class 값 알파벳 순으로 정렬되어 있습니다. 어떤 차종의 도시 연비가 높은지
#쉽게 알아볼 수 있도록 cty 평균이 높은 순으로 정렬해 출력하세요.

mpg_new %>% 
  group_by(class) %>% 
  summarise(cty_mean=mean(cty)) %>% 
  arrange(desc(cty_mean))

#Q3. 어떤 회사 자동차의 hwy(고속도로 연비)가 가장 높은지 알아보려고 합니다. hwy 평균이 가장 높은 회사
#세 곳을 출력하세요.

mpg_new %>% 
  group_by(manufacturer) %>% 
  summarise(hwy_mean=mean(hwy)) %>% 
  arrange(desc(hwy_mean)) %>% 
  head(3)

#Q4. 어떤 회사에서 "compact"(경차) 차종을 가장 많이 생산하는지 알아보려고 합니다. 각 회사별
#"compact" 차종 수를 내림차순으로 정렬해 출력하세요

mpg_new %>% 
  group_by(manufacturer) %>%
  filter(class=="compact") %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) 
  



##2

fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel
#Q1. mpg 데이터에는 연료 종류를 나타낸 fl 변수는 있지만 연료 가격을 나타낸 변수는 없습니다. 위에서
#만든 fuel 데이터를 이용해서 mpg 데이터에 price_fl(연료 가격) 변수를 추가하세요.

mpg_new <- left_join(mpg_new,fuel,by="fl")






#Q2. 연료 가격 변수가 잘 추가됐는지 확인하기 위해서 model, fl, price_fl 변수를 추출해 앞부분 5 행을
#출력해 보세요



mpg_new %>% 
  select("model","fl","price_fl") %>% 
  head(5)

##3
#문제 1. popadults 는 해당 지역의 성인 인구, poptotal 은 전체 인구를 나타냅니다. midwest 데이터에
#'전체 인구 대비 미성년 인구 백분율' 변수를 추가하세요.

midwest <- ggplot2::midwest

midwest <- midwest %>% 
  mutate(per=(poptotal-popadults)/poptotal*100)

midwest$per
#문제 2. 미성년 인구 백분율이 가장 높은 상위 5 개 county(지역)의 미성년 인구 백분율을 출력하세요.

midwest %>% 
  mutate(per=(poptotal-popadults)/poptotal*100) %>% 
  arrange(desc(per)) %>% 
  head(5) %>% 
  select(county,per)

#문제 3. 분류표의 기준에 따라 미성년 비율 등급 변수를 추가하고, 각 등급에 몇 개의 지역이 있는지
#알아보세요.

midwest <- midwest %>% 
  mutate(size=midwest$per)

midwest$size <- ifelse(midwest$size>=40,"large",ifelse(midwest$size<40 & midwest$size>=30,"middle","small"))

midwest %>% 
  select(per,size)
#문제4. popasian은 해당 지역의 아시아인 인구를 나타냅니다. '전체 인구 대비 아시아인 인구 백분율' 
#변수를 추가하고, 하위 10개 지역의 state(주), county(지역명), 아시아인 인구 백분율을 출력하세요

midwest %>% 
  mutate(per_asian=popasian/poptotal*100) %>% 
  arrange(per_asian) %>% 
  head(10) %>% 
  select(state,county,per_asian)


##4

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65, 124, 131, 153, 212), "hwy"]=NA
mpg$hwy

#Q1. drv(구동방식)별로 hwy(고속도로 연비) 평균이 어떻게 다른지 알아보려고 합니다. 분석을 하기 전에
#우선 두 변수에 결측치가 있는지 확인해야 합니다. drv 변수와 hwy 변수에 결측치가 몇 개 있는지
#알아보세요.

mpg
table(is.na(mpg$drv))
table(is.na(mpg$hwy))

#Q2. filter()를 이용해 hwy 변수의 결측치를 제외하고, 어떤 구동방식의 hwy 평균이 높은지 알아보세요. 
#하나의 dplyr 구문으로 만들어야 합니다

mpg %>% 
  filter(!is.na(mpg$hwy)) %>% 
  group_by(drv) %>% 
  summarise(hwy_mean=mean(hwy))

##5

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10, 14, 58, 93), "drv"] <- "k"
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)

mpg

#Q1. drv 에 이상치가 있는지 확인하세요. 이상치를 결측 처리한 다음 이상치가 사라졌는지 확인하세요. 결측
#처리 할 때는 %in% 기호를 활용하세요.

mpg %>% 
  group_by(drv) %>% 
  summarise(n=n())

mpg$drv <- ifelse(mpg$drv %in% "k",NA,mpg$drv)

#Q2. 상자 그림을 이용해서 cty 에 이상치가 있는지 확인하세요. 상자 그림의 통계치를 이용해 정상 범위를
#벗어난 값을 결측 처리한 후 다시 상자 그림을 만들어 이상치가 사라졌는지 확인하세요.

boxplot(mpg$cty)
boxplot(mpg$cty)$stats

mpg$cty <- ifelse(mpg$cty<9|mpg$cty>26,NA,mpg$cty)

boxplot(mpg$cty)


mpg %>% 
  filter(!is.na(mpg$cty) & !is.na(mpg$drv)) %>% 
  group_by(drv) %>% 
  summarise(cty_mean=mean(cty))


##6
#cars 데이터에서  각 speed마다 평균 dist를 tapply를 사용해 계산한다음, 이를 plot()하세요

#방법1
cars_mean_dist <- cars %>% 
  group_by(speed) %>% 
  summarise(dist_mean=mean(dist))

plot(cars_mean_dist,pch=20,cex=2)


#방법2
table(cars$speed)

tapply(cars$dist,cars$speed,mean)

plot(tapply(cars$dist,cars$speed,mean))
