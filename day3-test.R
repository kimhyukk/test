mpg

library(dplyr)
##1

#1-1
mpg4.5 <- mpg %>% 
  filter(displ>=4) %>% 
  filter(displ<=5)

sapply(split(mpg4.5$hwy,mpg4.5$manufacturer),mean)

#1-2
split(mpg$cty,mpg$manufacturer)

sapply(split(mpg$cty,mpg$manufacturer),mean)

#1-3
mpg.cfh <- mpg %>% 
  filter(manufacturer %in% c("chevrolet","ford","honda"))

mpg.cfh

mean(mpg.cfh$hwy)



##2

#2-1
mpg_new <- mpg %>% 
  select(class,cty)

#2-2
split(mpg_new$cty,mpg_new$class)

sapply(split(mpg_new$cty,mpg_new$class), mean)

##3
mpg_audi <- mpg %>% 
  filter(manufacturer=="audi")

mpg_audi_hwy <- mpg_audi %>% 
  arrange(desc(hwy))

mpg_audi_hwy[1:5,]


##4

#4-1,2,3
mpg_copy <- mpg

mpg_copy_arg_avg_totaly <- mpg_copy %>% 
  mutate(totaly=cty+hwy) %>% 
  mutate(avg_totaly=totaly/2) %>% 
  arrange(desc(avg_totaly))

mpg_copy_arg_avg_totaly[1:3,]


#4-4
mpg %>% 
  mutate(totaly=cty+hwy) %>% 
  mutate(avg_totaly=totaly/2) %>% 
  arrange(desc(avg_totaly)) %>% 
  head(3)


##5

#5-1,2
myfunc <- function(x,y){x^2+y+10}

#5-3
sample(1:45,6)

