#깜신 35~38탄. 로지스틱 회귀분석.
#:일반화 선형모델(generalized linear model)의 일종으로 
#'ln(y)=ax+b' == 'y=e^(ax+b)' 의 형태.
#생존률 같이 결과가 all or none(0 or 1) 등의 형태 값일 때 사용.
install.packages("moonBook")

require(survival)
str(colon)
colon1<-na.omit(colon)#빈칸있는 행 제거.
result<-glm(status ~ rx+sex+age+obstruct+perfor+adhere+nodes+differ+extent+surg,
            family=binomial, data=colon1)
summary(result)
reduced.model = step(result)
summary(reduced.model)
require(moonBook)
extractOR(reduced.model) #이런 데이터는 일반적으로 OR 값 등을 구해서 결과로..
#?extractOR:OR=odds ratio. lcl=lower confidence limit. ucl=upper confidence limit.
#과산포(overdispersion)
fit=glm(formula = status ~ rx + obstruct + adhere + nodes + extent + 
          surg, family = binomial, data = colon1)
fit.od=glm(formula = status ~ rx + obstruct + adhere + nodes + extent + 
             surg, family = quasibinomial, data = colon1)
#과산포가 있으면 quasibinomial!
pchisq(summary(fit.od)$dispersion*fit$df.residual,fit$df.residual, lower=F)
#>0.05면 과산포 없다.binomial!로 쓰면 됨.

ORplot(fit, main="Plot for Odds Ratios")
ORplot(fit, type=2, show.OR = F, show.CI = T, pch = 15, lwd = 3,
       col=c("blue, red"), main="Plot for Odds Ratios")
ORplot(fit, type=3, show.OR = F, show.CI = T, pch = 15, lwd = 3,
       col=c("blue, red"), main="Plot for Odds Ratios")
