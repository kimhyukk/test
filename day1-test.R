
#1번
a<-c(2,5,3)
a

rep(a,5)

rep(a,3,10)

rep(a,c(2,5,3))

#2번
m=matrix(1:12, nrow=3)
m

colnames(m)<-letters[1:4]
m
rownames(m)<-letters[1:3]
m

#3번
m1=m[,c(1,3)]
m1
m2=m[,c(2,4)]
m2

m1+m2
m1-m2
m1*m2
m1/m2

m1%*%m2 #불가 2x3 2x3 행렬이라서


#4번
k=matrix(1:9999, ncol=9)


k[c(109:111),c(8,9)]
