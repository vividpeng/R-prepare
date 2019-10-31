#깜신7~9탄.문건웅 의학논문 작성을 위한 R통계와 그래프.
#두그룹의 비교
install.packages("moonBook")
library(moonBook)
require(moonBook)
data(acs)
View(acs)
acs
summary(acs)

str(acs)

moonBook::densityplot(age~sex, data=acs)
shapiro.test(acs$age[acs$sex=="Male"])
shapiro.test(acs$age[acs$sex=="Female"])


#Mann whitney U test=wilcoxon rank sum test
#평균값이 아니라 등수를 비교하여 두데이터를 비교하는 통계법 (비모수적 통계)
#(1.결과값이 연속변수 x or 2.정규분포 x)
wilcox.test(age~sex, data=acs)


#variation check(등분산성 검정, P<0.05 면 등분산 아님)
var.test(age~sex, data=acs)

#student's t-test(1.결과값이 연속변수 O, 2.정규분포 O, 3.등분산 O)
t.test(age~sex,data=acs, var.equal=T) 

#Welch's test(1.결과값이 연속변수O, 2.정규분포 O, 3.등분산 X)
t.test(age~sex,data=acs, var.equal=F) 
