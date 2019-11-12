#깜신 26~27탄.상관 분석.
#산점도(scatter plot), -1<상관 계수<1. x를 y는 상관계수^2정도로 설명한다고 표현.

#spearman 상관 계수 (S). 서열변수. 비모수.
#kendall의 tau (K). 서열변수. 비모수.

install.packages("UsingR")

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
