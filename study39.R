#깜신 39탄. 포아송 회귀분석(Poisson Regression).
#~에 따른 발생률, 사망률 등을 분석할 때 쓰임.
install.packages("robust")
install.packages("qcc")
install.packages("moonBook")

data(breslow.dat, package="robust")
summary(breslow.dat)
head(breslow.dat)

library(qcc)
#과산포 먼저 확인.(과산포면 quasipoisson으로 아니면 poisson으로.)
qcc.overdispersion.test(breslow.dat$sumY, type = "poisson")#p<0.05 과산포있다.

fit = glm(sumY ~ Base + Age +Trt, family = quasipoisson, data =breslow.dat)
summary(fit)#p=0.355702 투약 효과 없음. ㅋㅋ

library(moonBook)
extractOR(fit, digits =3)

ORplot(fit, type = 2, show.CI=T, main = "Plot for Quasipoisson")
