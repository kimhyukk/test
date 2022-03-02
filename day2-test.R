#Q1
score<-c(80,60,70,50,90)
score

#Q2
mean(score)

#Q3
score_mean<-mean(score)

score_mean


#Q4
df<-data.frame(c("사과","딸기","수박"),c(1800,1500,3000),c(24,38,13))

colnames(df)<-c("제품","가격","판매량")

df

#Q5
mean(df[,2]), mean(df[,"가격"])

mean(df[,3]), mean(df[,"판매량"])


#Q6
mpg<-ggplot2::mpg
mpg_new<-mpg

#Q7
mpg_new<-rename(mpg_new,city=cty)

mpg_new<-rename(mpg_new,highway=hwy)

#Q8
mpg_new
