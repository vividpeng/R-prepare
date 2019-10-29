############ csv 파일을 로딩하는 코드 ##########################

KY <- read.csv(file = "E:/r/KY.csv",header = TRUE, sep = ",")


############ pwr 패키지 설치 먼저 해야합니다. ##################

install.packages("pwr")
library(pwr)


############ 각 그룹의 평균을 구하는 코드  #####################

mean.1 <- mean(KY$score[KY$group == 1])

mean.2 <- mean(KY$score[KY$group == 2])


############ 각 그룹의 표준편차를 구하는 코드  #################

sd.1 <- sd(KY$score[KY$group == 1])

sd.2 <- sd(KY$score[KY$group == 2])


############ 전체 데이터를 살펴보는 코드 #######################

summary(KY)


############ 전체 데이터의 열(row) 갯수를 알려주는 코드 ########

nrow(KY)


############ 전체 데이터의 행(columns) 갯수를 알려주는 코드 ####

length(KY)


############루트 값을 구하는 코드   ############################

sqrt(2)


############ 절대값을 구하는 코드 ##############################

abs(1-3)


############ 새로운 변수에 실험군과 대조군을 나누어 담는 코드 ###########

group.1 <- subset(KY, KY$group==1)

group.2 <- subset(KY, KY$group==2)


############ effect size를 구하는 코드 #########################

effectSize <- abs(mean.1-mean.2)/(sqrt((sd.1^2+sd.2^2)/2))

effectSize <- abs(mean.1-mean.2)/(sqrt((sd.1^2*(nrow(group.1)-1)+sd.2^2*(nrow(group.2)-1))/((nrow(group.1)-1)+(nrow(group.2)-1))))


############ power analysis를 통해 적정 n 수를 구하는 코드 (pwr 패키지)#############

pwr.t.test(d= effectSize,power=.8,sig.level=.05,type="two.sample",alternative="two.sided")



############ 각 그룹의 정규성을 검정하기 위한 코드 ####################

shapiro.test(group.1$score)

shapiro.test(group.2$score)


#출처: https://jinmedi.tistory.com/381 [깜신의 작은 진료소]