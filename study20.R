#깜신20~25탄.짝을 이루는 data.(a약을 먹었을 때와 b약을 먹었을 때 효과 차이, 성형 전후 자존감 차이 등)
install.packages("tidyr")
install.packages("PairedData")
install.packages("gplots")
install.packages("car")
install.packages("reshape")
install.packages("coin")

#paired t-test: 정규분포o
pd<-read.csv("pairedData.csv",header = T)#쥐 10마리로 한 몸무게 변화 데이터.

#paire t를 하기 위해서는 wide type을 long type data로 만들어야됨.
library(tidyr)
pd2<-gather(pd, key = "group", value = "result",-ID)

shapiro.test(pd2$result[pd2$group=="before"])
shapiro.test(pd2$result[pd2$group=="After"])
d<-pd2$result[pd2$group=="After"]-pd2$result[pd2$group=="before"]
shapiro.test(d)
library(PairedData)
b<-subset(pd2, group=="before", result, drop = T)
a<-subset(pd2, group=="After", result, drop = T)
pd3<-paired(b,a)
pd3
plot(pd3, type="profile")+theme_bw()
t.test(result~group,data = pd2, paired=T)

#wilcoxon signed rank test: 정규분포 x or 서열변수o.
data(sleep)
head(sleep)
View(sleep)

shapiro.test(sleep$extra[sleep$group==2]-sleep$extra[sleep$group==1]) #p<0.05 이므로 최소 둘중에 하나는 정규분포아님.

#with 함수 typing 줄이기..
with(sleep, shapiro.test(extra[group==2]-extra[group==1]))

b<-subset(sleep, group=="1", extra, drop = T)
a<-subset(sleep, group=="2", extra, drop = T)
library(PairedData)
sleep2<-paired(b,a)
plot(sleep2, type="profile")+theme_bw()

with(sleep, wilcox.test(extra[group==2]-extra[group==1], exact=F)) #p<0.05 그래프의 변화가 통계적 의미가 있다.

#세그룹이상.
#(one way)repeated measures ANOVA: 정규분포o
ow<-read.csv("onewaySample.csv", header =T)

ow<-ow[,2:6] #1열지우기
means<-c(mean(ow$score0),mean(ow$score1),mean(ow$score3),mean(ow$score6))
means
library(gplots)
plotCI(x=means, type = "l", ylab = "score", xlab = "month", main="One way test")

multmodel<-lm(cbind(ow$score0,ow$score1,ow$score3,ow$score6)~1)
multmodel
trials<-factor(c("score0","score1","score3","score6"),ordered=F)
trials
library(car)
model1<- Anova(multmodel, idata=data.frame(trials), idesign= ~trials, type="III")
summary(model1, multivariate = F) #Pr<0.05이므로 유의한 결과이다.(1.054398e^-12)
#사후검정 (3개에상의 데이터에서 어느구간이 유의미한지 확인하기)
library(tidyr)
owlong<-gather(ow, key = "ID", value = "score")#long type으로
owlong<-owlong[8:35,] #ID값 뺌뺌

out<-aov(score~ID,data=owlong)
shapiro.test(resid(out))
TukeyHSD(out)#4개의 데이터이므로 p<0.5/4(Bonferroni correction)이어야 하며 맞으므로 각각의 단계가 모두 유의한 차이다!
#표준오차(standard error구하기)
se<-sd(owlong$score)/sqrt(length(owlong$score))
plotCI(x=means, uiw=se, type = "l", ylab = "score", xlab = "month", main="One way test")


#two way repeated measures ANOVA
acne<-read.csv("10_rmanova.csv",header=T)

acL<-reshape(acne, direction ="long",varying = 3:6, sep="")
str(acL)
acL$group<-factor(acL$group)
acL$id<-factor(acL$id)
acL$time<-factor(acL$time)
str(acL)

interaction.plot(acL$time,acL$group,acL$month)

acD<-aov(month~group*time+Error(id),data=acL)
summary(acD)

#사후검정.
acL0<-acL[acL$time=="0",]
acL1<-acL[acL$time=="1",]
acL3<-acL[acL$time=="3",]
acL6<-acL[acL$time=="6",]

t.test(month~group, data=acL0)
t.test(month~group, data=acL1)
t.test(month~group, data=acL3)
t.test(month~group, data=acL6)
#마찬가지로 본페로니 0.5/4기준으로 진행.

#Friedman test: 정규분포 x or 서열변수.
RT<-read.csv("RT.csv", header =T) #?friedman.test에서 가져온 데이터임.
RT<-RT[,2:4] 

library(reshape)

RT2<-melt(RT)
rt3<-melt(RoundingTimes)
out<-aov(value~variable,data=RT2)
shapiro.test(resid(out))
boxplot(value~variable, data=RT2)
friedman.test(as.matrix(RT))#matrix형태여야 계산됨.p<0.05이므로 유의함!
#사후검정해야됨.
library(coin)

friedman.test.with.post.hoc <- function(formu, data, to.print.friedman = T, to.post.hoc.if.signif = T,  to.plot.parallel = T, to.plot.boxplot = T, signif.P = .05, color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
{
  # formu is a formula of the shape: 	Y ~ X | block
  # data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor) ]]
  
  # Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, then that entire block should be removed.
  
  
  # Loading needed packages
  if(!require(coin))
  {
    print("You are missing the package 'coin', we will now try to install it...")
    install.packages("coin")
    library(coin)
  }
  
  if(!require(multcomp))
  {
    print("You are missing the package 'multcomp', we will now try to install it...")
    install.packages("multcomp")
    library(multcomp)
  }
  
  if(!require(colorspace))
  {
    print("You are missing the package 'colorspace', we will now try to install it...")
    install.packages("colorspace")
    library(colorspace)
  }
  
  
  # get the names out of the formula
  formu.names <- all.vars(formu)
  Y.name <- formu.names[1]
  X.name <- formu.names[2]
  block.name <- formu.names[3]
  
  if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]	# In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
  
  # Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
  
  # stopping in case there is NA in the Y vector
  if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
  
  # make sure that the number of factors goes with the actual values present in the data:
  data[,X.name ] <- factor(data[,X.name ])
  data[,block.name ] <- factor(data[,block.name ])
  number.of.X.levels <- length(levels(data[,X.name ]))
  if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
  
  # making the object that will hold the friedman test and the other.
  the.sym.test <- symmetry_test(formu, data = data,	### all pairwise comparisons
                                teststat = "max",
                                xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
                                ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
  )
  # if(to.print.friedman) { print(the.sym.test) }
  
  
  if(to.post.hoc.if.signif)
  {
    if(pvalue(the.sym.test) < signif.P)
    {
      # the post hoc test
      The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")	# this is the post hoc of the friedman test
      
      
      # plotting
      if(to.plot.parallel & to.plot.boxplot)	par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
      
      if(to.plot.parallel)
      {
        X.names <- levels(data[, X.name])
        X.for.plot <- seq_along(X.names)
        plot.xlim <- c(.7 , length(X.for.plot)+.3)	# adding some spacing from both sides of the plot
        
        if(color.blocks.in.cor.plot)
        {
          blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
        } else {
          blocks.col <- 1 # black
        }
        
        data2 <- data
        if(jitter.Y.in.cor.plot) {
          data2[,Y.name] <- jitter(data2[,Y.name])
          par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"
        } else {
          par.cor.plot.text <- "Parallel coordinates plot"
        }
        
        # adding a Parallel coordinates plot
        matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
                                  direction="wide")[,-1])  ,
                type = "l",  lty = 1, axes = FALSE, ylab = Y.name,
                xlim = plot.xlim,
                col = blocks.col,
                main = par.cor.plot.text)
        axis(1, at = X.for.plot , labels = X.names) # plot X axis
        axis(2) # plot Y axis
        points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
      }
      
      if(to.plot.boxplot)
      {
        # first we create a function to create a new Y, by substracting different combinations of X levels from each other.
        subtract.a.from.b <- function(a.b , the.data)
        {
          the.data[,a.b[2]] - the.data[,a.b[1]]
        }
        
        temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
                             direction="wide") 	#[,-1]
        wide.data <- as.matrix(t(temp.wide[,-1]))
        colnames(wide.data) <- temp.wide[,1]
        
        Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
        names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
        
        the.ylim <- range(Y.b.minus.a.combos)
        the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))	# adding some space for the labels
        is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
        
        boxplot(Y.b.minus.a.combos,
                names = names.b.minus.a.combos ,
                col = is.signif.color,
                main = "Boxplots (of the differences)",
                ylim = the.ylim
        )
        legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
        abline(h = 0, col = "blue")
        
      }
      
      list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
      if(to.print.friedman) {print(list.to.return)}
      return(list.to.return)
      
    }	else {
      print("The results where not significant, There is no need for a post hoc test")
      return(the.sym.test)
    }
  }
  
  # Original credit (for linking online, to the package that performs the post hoc test) goes to "David Winsemius", see:
  # http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
}

friedman.test.with.post.hoc(value~X2 | X1, rt3)
#p<0.05/3(본페로니)보다 작은 Wide Angle - Round Out에서 유의한 것이다!
