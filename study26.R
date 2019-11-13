#깜신 26~28탄.상관 분석.과 그래프.
#산점도(scatter plot), -1<상관 계수<1. x를 y는 상관계수^2*100%정도로 설명한다고 표현.

#spearman 상관 계수 (S). 서열변수. 비모수.
#kendall의 tau (K). 서열변수. 비모수.

install.packages("UsingR")
install.packages("SwissAir")
install.packages("hexbin")
install.packages("IDPmisc")

#pearson 상관계수 (N). 정규분포o.
library(UsingR)
data(galton)
View(galton)
str(galton)
plot(child~parent, data =galton)
cor.test(galton$child, galton$parent)#상관계수:0.4587624.
#선형모델 만들기.
out=lm(child~parent, data=galton)
summary(out)#y=ax+b; a:0.64629, b:23.94153.
abline(out, col="red")
#예쁘게 만들기.
plot(jitter(child,5)~jitter(parent,5), galton)
sunflowerplot(galton)
library(SwissAir)
data("AirQual")
View(AirQual)
str(AirQual)
ox<-AirQual[,c("ad.O3","lu.O3","sz.O3")]+
  AirQual[,c("ad.NOx","lu.NOx","sz.NOx")]-
  AirQual[,c("ad.NO","lu.NO","sz.NO")]
str(ox)
names(ox)<-c("ad","lu","sz")
plot(lu~sz, data=ox)
library(hexbin)
bin=hexbin(ox$lu,ox$sz,xbins=50)
plot(bin)
smoothScatter(ox$lu,ox$sz)
library(IDPmisc)
iplot(ox$lu,ox$sz)
