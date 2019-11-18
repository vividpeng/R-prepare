#깜신 40~42탄.생존 분석(Survival analysis),cumulative hazard ratio<회귀분석.
install.packages("survival")
library(survival)
data(colon)
View(colon)
colon1<-na.omit(colon)
str(colon1)
colon1$TS<-Surv(colon1$time, colon1$status==1)
head(colon1)
fit = survfit(TS ~ rx, data = colon1)
plot(fit, col = 1:3, lty=1:3)
legend("topright", legend = levels(colon$rx), col=1:3, lty=1:3)
#생존률은 내려가는 그래프. 
#cumulative hazard ratio에 맞는 그래프 형태 만들기.
plot(fit, col = 1:3, lty=1:3, fun="cumhaz", mark.time = F)

#Log-rank test.
#42탄:00:30