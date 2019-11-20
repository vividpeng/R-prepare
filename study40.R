#깜신 40~42탄.생존 분석(Survival analysis),cumulative hazard ratio<회귀분석.
install.packages("survival")
install.packages("moonBook")
library(survival)
data(colon)
View(colon)
colon1<-na.omit(colon)
str(colon1)
colon1$TS<-Surv(colon1$time, colon1$status==1)#살아있는(status==1)사람의 타임에 +넣어서 구분해주기.
head(colon1)
fit = survfit(TS ~ rx, data = colon1)
plot(fit, col = 1:3, lty=1:3)
legend("topright", legend = levels(colon$rx), col=1:3, lty=1:3)
#생존률은 내려가는 그래프. 
#cumulative hazard ratio에 맞는 그래프 형태 만들기.
plot(fit, col = 1:3, lty=1:3, fun="cumhaz", mark.time = F)

#Log-rank test.
survdiff(Surv(time, status == 1)~rx, data=colon1)#p<0.05이므로 유의한 차이가 있다.

#Cox Regression.
out = coxph(Surv(time, status == 1)~rx, data=colon1)
summary(out)

#Hazard ratios of all individual variables.
out = coxph(Surv(time, status == 1)~rx, data=colon1)
library(moonBook)
attach(colon1)#매번 colon1쓰기 귀찮아서...
out = mycph(TS~.-id-study-time-status-etype, data = colon1)#.- 전체에서 -한거빼고 다해줭.
out2=coxph(TS~.-id-study-time-status-etype, data = colon1)
final=step(out2, direction = "backward")
HRplot(final, type=2, show.CI = T)
