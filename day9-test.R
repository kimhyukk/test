library(dplyr)


train <- read.csv("train.csv", na.string="")


train$Survived


train %>% 
  group_by(Survived) %>% 
  summarise(n=n())


table(train$Survived)




pclass.table <- as.data.frame(table(train$Pclass))


pclass.table %>% 
  mutate(ratio=pclass.table$num/nrow(train))


pclass.table

colnames(pclass.table) <- c('pclass','num')

pclass.table$ratio <- round(pclass.table$num/nrow(train),3)

pclass.table




proportions(train$Survived)






ifelse(train$Embarked=="C",1,ifelse(train$Embarked=="Q",2,3))






train$Age


train$Child <- 0


train


#train$Age<18,train$Survived==1,train$Child==1


train$Child[train$Age<18] <- 1

train$Child





table(train$Child,train$Sex,train$Survived)



train %>% 
  group_by(Child,Sex,Survived) %>% 
  summarise(n=n())





aggregate(Survived~Sex+Child,data=train,FUN=length)



round(table(train$Survived)/nrow(train),2)

prop.table(table(train$Survived))

aggregate(Survived~Sex,data=train,FUN = function(x){sum(x)/length(x)})



round(table(train$Pclass)/nrow(train),2)

prop.table(table(train$Pclass))

aggregate(Survived~Pclass,data=train,FUN = function(x){round(sum(x)/length(x),2)})


age_cut <- cut(train$Age, breaks=c(0,10,20,30,40,50,60,70,80,Inf))

aggregate(Survived~age_cut,data=train,FUN = function(x){round(sum(x)/length(x),2)})


prop.table(table(train$Survived))

train$Name

strsplit(train$Name, split=" ")

lst<-unlist(strsplit(train$Name, split=" "))

lst
grep("Mr.", lst, ignore.case = TRUE)
grep("Mlle",lst)


train$Name2 <- 0

train$Name2[grep("Mlle",train$Name)] <- "Miss"
train$Name2[grep("Ms",train$Name)]<- "Miss"
train$Name2[grep("Lady",train$Name)]<- "Miss"
train$Name2[grep("Dona",train$Name)]<- "Miss"
train$Name2[grep("Mme",train$Name)]<- "Mrs"
train$Name2[grep("Capt",train$Name)]<- "Officer"
train$Name2[grep("Col",train$Name)]<- "Officer"
train$Name2[grep("Major",train$Name)]<- "Officer"
train$Name2[grep("Dr",train$Name)]<- "Officer"
train$Name2[grep("Rev",train$Name)]<- "Officer"
train$Name2[grep("Don",train$Name)]<- "Officer"
train$Name2[grep("Sir",train$Name)]<- "Officer"
train$Name2[grep("the Countess",train$Name)]<- "Officer"
train$Name2[grep("Jonkheer",train$Name)]<- "Officer"
train$Name2[grep("Mr",train$Name)]<- "Mr"
train$Name2[grep("Mrs",train$Name)]<- "Mrs"
train$Name2[grep("Miss",train$Name)]<- "Miss"
train$Name2 <- ifelse(train$Name2=="0","Others",train$Name2)

train %>% 
  select(Name,Name2)




train$names2<-lst[grep(".{2,}\\.$", lst)]

table(train$names2)

train$name3 <- train$names2

train$name3 <- ifelse(train$name3 %in% c("Mlle.","Ms.","Lady.","Dona."),"Miss",train$name3)

train$name3 <- ifelse(train$name3=="Mme.","Mrs",train$name3)

train$name3 <- ifelse(train$name3 %in% c("Capt.","Col.","Major.","Dr.","Rev.","Don.","Sir.","Countess.","Jonkheer."),
                      "Officer",train$name3)

train$name3 <- ifelse(train$name3=="Master.","Others",train$name3)

train$name3 <- ifelse(train$name3=="Miss.","Miss",train$name3)
train$name3 <- ifelse(train$name3=="Mrs.","Mrs",train$name3)
train$name3 <- ifelse(train$name3=="Mr.","Mr",train$name3)

table(train$name3)

train$name3
##################################

train$Fare


Fare2<- cut(train$Fare,breaks=c(0,10,20,30,Inf),right = F)


aggregate(Survived~Fare2+Pclass+Sex,data=train,FUN = function(x){round(sum(x)/length(x),2)})
