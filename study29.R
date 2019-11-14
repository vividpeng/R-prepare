#깜신 29~33탄. 회귀분석.
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
install.packages("car",dependencies = T)

#단순선형회귀분석.
women
plot(weight~height,data=women)
fit<-lm(weight~height,data=women) 
abline(fit, col="blue")
summary(fit)#R-squared(결정계수):  0.991. 99.1%정도 설명한다.
r<-cor.test(women$weight,women$height)
r[["estimate"]][["cor"]]^2#==R-squared(결정계수)
plot(fit)

par(mfrow=c(2,2))#4개로 나눠서 4개 다 보여줭.
plot(fit)
par(mfrow=c(1,1))#1개로 합쳐서 1개만 보여줭.

#다항회귀모형.(ax^2같은거).polynomial regression.
fit2<-lm(weight~height + I(height^2),data=women)
summary(fit2)
lines(women$height, fitted(fit2), col="red")
plot(fit2)
fit3<-lm(weight~height + I(height^2) + I(height^3),data=women)
plot(fit3)#residuals(잔차)는 좋아졌지만,??....


#다중회귀분석.
state.x77
states<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
states
fit<-lm(Murder~Population+Illiteracy+Income+Frost,data=states)
summary(fit)
plot(fit)
#다중공선성(Multicolinearity)
#:회귀모델의 각 변수는 독립되어 있어야한다.
#:(ex>illiteracy와 income이 서로 영향력있는 변수이면 안됨.)
#:VIF(variance inflation factor)값으로 확인한다.
library(car)
vif(fit)
sqrt(vif(fit))#<2여야 다중공선성(Multicolinearity) 문제로부터 자유롭다.
#이상관측치.확인하기.
#:이상치(outlier)->표준잔차*2만큼 차이값.이면 재확인.
#:큰지레점(high leverage points)->평균hat통계량[p(인수들의 개수<a1x1++a2x2+b>는 3개)/n(샘플수)]의 2~3배.면 재확인!
#:영향관측치(influential observation)->Cook's distance or D statistics.=4/n(샘플수)-k(<a1x1++a2x2+b>는 2개.)-1>0.1크게는 1.면 재확인.
influencePlot(fit, id.method = "identify")#y축.표준잔차.x축.Hat-Values.원지름은 Cook's D.

#회귀모형의 교정.33탄.car package.
#회귀분석은 정규성,독립성,선형성,등분산성.
states
#정규성.교정.
powerTransform(states$Murder)
summary(powerTransform(states$Murder))
#선형성.교정.
boxTidwell(Murder~ Population+Illiteracy, data=states)
#등분산성.교정.
ncvTest(fit)
spreadLevelPlot(fit)
