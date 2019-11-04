#깜신20~22탄.짝을 이루는 data.(a약을 먹었을 때와 b약을 먹었을 때 효과 차이, 성형 전후 자존감 차이 등)
install.packages("tidyr")
install.packages("PairedData")


#paired t-test: 정규분포o
pd<-read.csv("pairedData.csv",header = T)#쥐 10마리로 한 몸무게 변화 데이터.

#paire t를 하기 위해서는 wide type을 long type data로 만들어야됨.
library(tidyr)
pd2<-gather(pd, key = "group", value = "result",-ID)

shapiro.test(pd2$result[pd2$group=="before"])
shapiro.test(pd2$result[pd2$group=="After"])
d<-pd2$result[pd2$group=="After"]-pd2$result[pd2$group=="before"]
shapiro.test(d)
library(PairedData)
b<-subset(pd2, group=="before", result, drop = T)
a<-subset(pd2, group=="After", result, drop = T)
pd3<-paired(b,a)
pd3
plot(pd3, type="profile")+theme_bw()
t.test(result~group,data = pd2, paired=T)

#wilcoxon signed rank test: 정규분포 x or 서열변수.
data(sleep)
head(sleep)


#세그룹이상.
#(one way)repeated measures ANOVA: 정규분포o


#two way repeated measures ANOVA


#Friedman test: 정규분포 x or 서열변수.