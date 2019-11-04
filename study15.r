#깜신 15~19탄.reraltive risk와 odds ratio
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
result

#mosaicplot그리기.(모자이크그래프)
help(mosaicplot)
mosaicplot(result)
mosaicplot(result, color = c("tan1","firebrick2"), ylab="HTN", xlab = "Smoking")

colors()
demo("colors") #다양한 색깔의 demo보기.

t(result) #x,y축 뒤집기.

mytable(smoking~age, data=acs)
#보다시피 age가 일정하지 않아 교란변수로 작용. (담배피는 사람이 고혈압이 많은 이유.)

#barplot그리기(막대그래프)
result<-table(mtcars$cyl, mtcars$sm)
barplot(result)
barplot(result, ylim = c(0,20), legend=rownames(result)) #legend(범례) 넣기.
mylegend=paste(rownames(result), "cyl")
barplot(result, ylim = c(0,20), legend=mylegend) #legend 변경하기.
barplot(result, ylim = c(0,20), legend=mylegend, beside = T) #옆으로 쌓기.
barplot(result, ylim = c(0,20), legend=mylegend, beside=T,horiz = T) #수평그래프.

mycol=c("tan1","coral2","firebrick2")
mycol
barplot(result, ylim = c(0,20), legend=mylegend, beside=T,horiz = T,col=mycol)#색넣기.
