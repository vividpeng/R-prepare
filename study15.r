#깜신 15~17탄
install.packages("moonBook")

data("mtcars")
View(mtcars)

table(mtcars$cyl, mtcars$am)

#table에 labeling하기.
#1
mtcars$tm<-ifelse(mtcars$am==0, "auto", "manual")
table(mtcars$cyl, mtcars$tm)

#2
mtcars$sm<-factor(mtcars$am, labels = c("auto","manual"))
table(mtcars$cyl, mtcars$sm)

result<-table(mtcars$cyl, mtcars$sm)

#총합을 table에 추가하기
addmargins(result)

# relative risk & odds ratio(상대위험도와 오즈비)
a <- read.csv(file = "D:/Code/R-prepare/study15.csv",header = TRUE, sep = ",")
# RR=p1/p2, OR={p1/(1-p1)}/{p2/(1-p2)}
# p1=p2이면 RR=OR

#Chi-square test  
chisq.test(result) #error message는 기대도수가 5보다 작은 cell이 많아서.


#Fisher's exact test : 기대도수가 5보다 작은 cell(칸)이 20%이상 일 때
fisher.test(result) #p<0.05이므로 두 변수는 연관성이 있다고 할 수 있다.


#Cochran-Amitage (trend) test : trend가 있는 변수에 대하여 결과를 검증할 때(상중하.. 등등의 의미)
require(moonBook)
data(acs)
table(acs$HBP,acs$smoking)
#level에 따라 순서 바꾸기
acs$smoking<-factor(acs$smoking, levels=c("Never","Ex-smoker","Smoker"))
result<-table(acs$HBP,acs$smoking)
result[2,]
colSums(result)
prop.trend.test(result[2,],colSums(result))#numbers of event, numbers of trials:p<0.05이므로 연관이 있다.


