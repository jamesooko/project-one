productivity=read.csv("garments_worker_productivity.csv")
head(productivity)
View(productivity)
library(rcompanion)
par(mfrow=c(2,3)) #used to split the screen into 2 rows and 3 columns
plotNormalHistogram(productivity$actual_productivity,xlab="productivity")
plotNormalHistogram(productivity$over_time,xlab="overtime")
plotNormalHistogram(productivity$smv,xlab="time allocated for task")
plotNormalHistogram(productivity$incentive,xlab="incentive")
plotNormalHistogram(productivity$no_of_workers,xlab="number of workers")

par(mfrow=c(2,3)) #used to split the screen into 2 rows and 3 columns
boxplot(productivity$actual_productivity,xlab="productivity")
boxplot(productivity$over_time,xlab="overtime")
boxplot(productivity$smv,xlab="time allocated for task")
boxplot(productivity$incentive,xlab="incentive")
boxplot(productivity$no_of_workers,xlab="number of workers")

productivity$idle_time_recode=ifelse(productivity$idle_time==0,0,1)
library(epiDisplay)
par(mfrow=c(1,2))
style_change=productivity$no_of_style_change
idle_time=productivity$idle_time_recode
tab1(style_change)
tab1(idle_time)

library(psych)
cont=productivity[,c(15,9,10,7,14)]
describe(cont)
library(PerformanceAnalytics)
cor_data=productivity[,c(15,9,10,7,13,14,16)]
chart.Correlation(cor_data,histogram =F,pch=19)

library(ggplot2)
g1=ggplot(cor_data,aes(x=over_time,y=actual_productivity))+
  geom_point()+
  geom_smooth(formula = y~x)+
  labs(x="overtime minutes",y="productivity")
g2=ggplot(cor_data,aes(x=incentive,y=actual_productivity))+
  geom_point()+
  geom_smooth(formula = y~x)+
  labs(x="Incentives",y="productivity")
g3=ggplot(cor_data,aes(x=smv,y=actual_productivity))+
  geom_point()+
  geom_smooth(formula = y~x)+
  labs(x="minutes allocated to task",y="productivity")
g4=ggplot(cor_data,aes(x=no_of_workers,y=actual_productivity))+
  geom_point()+
  geom_smooth(formula = y~x)+
  labs(x="mnumber of workers",y="productivity")
library(gridExtra)
grid.arrange(g1,g2,g3,g4,ncol=2,nrow=2)

##regression
cor_data$no_of_style_change=factor(cor_data$no_of_style_change)
mod=lm(actual_productivity~.,data=cor_data)
summary(mod)
c2=cor_data[,-6]

modb=lm(actual_productivity~.,data=c2)
summary(modb)

c3=c2[,-4]
modc=lm(actual_productivity~.,data=c3)
summary(modc)

confint(modb)

c4=c3[,-2]
modd=lm(actual_productivity~.,data=c4)
summary(modd)

newdata=data.frame(incentive=mean(productivity$incentive),no_of_style_change=0,idle_time_recode=0)
newdata$no_of_style_change=factor(newdata$no_of_style_change)
pred=predict(modd,newdata,se.fit=T,interval="confidence",level=0.95)
pred$fit

newdata2=data.frame(incentive=quantile(productivity$incentive,prob=0.9),no_of_style_change=0,idle_time_recode=0)
newdata2$no_of_style_change=factor(newdata2$no_of_style_change)
pred2=predict(modd,newdata2,se.fit=T,interval="confidence",level=0.95)
pred2$fit

