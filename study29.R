#깜신 29~30탄. 회귀분석. 10:00분.
#빅데이터, 인공지능(머신러닝, 딥러닝)에서도 씀.
#단순선형회귀분석:그동안 해왔던거. y=ax+b.
#다중회귀분석.y=a1x1+a2x2+a3x3+.....+b.
#적정모형의 선택:많은 인자를 넣는 다고 잘 나오지 않음.영향이 많은 걸 잘 골라야.
#과적합 상태가되어 오히려 구려짐.

#1>Backward regression.:다 넣고 P-value 높은 것부터 빼보기.
#2>Forward regression.:하나씩 p-value 낮은 것부터 넣어보기.
#3>all subset regression.
#4>cross validation.
#...
#AIC(Akaike Information Criteria)

#단순선형회귀.
women
plot(weight~height,data=women)
fit<-lm(weight~height,data=women) 
abline(fit, col="blue")
summary(fit)#R-squared(결정계수):  0.991. 99.1%정도 설명한다.
r<-cor.test(women$weight,women$height)
r[["estimate"]][["cor"]]^2#==R-squared(결정계수)

