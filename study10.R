#깜신10~13탄.
#세그룹 이상의 비교
install.packages("moonBook")
library(moonBook)
data(acs)
summary(acs)

moonBook::densityplot(LDLC~Dx, data=acs)
densityplot(LDLC~Dx, data=acs)

shapiro.test(acs$LDLC[acs$Dx=="NSTEMI"]) #p>0.05면 정규분포; So, 정규분포 아님.
shapiro.test(acs$LDLC[acs$Dx=="STEMI"]) #p>0.05면 정규분포; So, 정규분포.
shapiro.test(acs$LDLC[acs$Dx=="Unstable Angina"])

#정규성 한꺼번에 확인하는 법
out = aov(LDLC~Dx, data = acs)
shapiro.test(resid(out)) #p<0.05 세 그룹 중에서 하나이상이 정규분포를 하지 않음!
shapiro.test(resid(aov(LDLC~Dx, data = acs))) #p<0.05 세 그룹 중에서 하나이상이 정규분포를 하지 않음!


#세그룹의 등분산 확인하기
bartlett.test(LDLC~Dx, data = acs) #p<0.05이면, 등분산!


#kruskal-wallis H test(결과값이 연숙변수X or 정규분포X)
kruskal.test(LDLC~factor(Dx), data = acs) #p<0.05이면, 세그룹의 평균이 모두 같지는 않다!



#Welch's ANOVA(결과값이 연숙변수 O, 정규분포 O, 등분산X)
oneway.test(LDLC~Dx, data = acs, var.equal = F) #p<0.05이면, 세그룹의 평균이 모두 같지는 않다!



#ANOVA(결과값이 연숙변수 O, 정규분포 O, 등분산O)
aov(LDLC~Dx, data = acs) #p<0.05이면, 세그룹의 평균이 모두 같지는 않다!


#multiple comparison; Tukey; 사후 검정;

#ANOVA에서 사후 검정
TukeyHSD(aov(LDLC~Dx, data = acs))

#kruskal-wallis H test에서 사후 검정; https://jinmedi.tistory.com/395
#window환경
install.packages('userfriendlyscience')

library(userfriendlyscience)

data(InsectSprays)

densityplot(count~spray, data=InsectSprays)

posthocTGH(InsectSprays$spray, y = InsectSprays$count, method = 'games-howell') 

#Welch's ANOVA에서 사후검정
install.packages("nparcomp")
library(nparcomp)
summary(mctp(LDLC~Dx, data=acs))

