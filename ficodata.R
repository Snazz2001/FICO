setwd("C:/My Projects/Zheng - FICO")
fico<-read.csv("fico.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(fico)[1]<-"ProposalID"
library(ggplot2)
cor(fico$fico,fico$mixedscore)
plot(density(fico$fico))
ficogood<-fico[which(fico$fico>680),]
ficobad<-fico[which(fico$fico<= 680),]
cor(ficogood$mixedscore,ficogood$fico)
cor(ficobad$mixedscore,ficobad$fico)
opar<-par(no.readonly=TRUE)
par(mfrow=c(2,2))
plot(density(ficogood$fico))
plot(density(ficobad$fico))
plot(density(ficogood$mixedscore))
plot(density(ficobad$mixedscore))

ficoperf<-read.csv("ficoperf.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
summary(ficoperf)
colnames(ficoperf)[1]<-"ProposalID"
ficoperfgood<-ficoperf[which(ficoperf$FICO_SCORE>680),]
ficoperfbad<-ficoperf[which(ficoperf$FICO_SCORE<=680),]
ficoperf<-ficoperf[complete.cases(ficoperf),]
ficoperfgood<-ficoperfgood[complete.cases(ficoperfgood),]
ficoperfbad<-ficoperfbad[complete.cases(ficoperfbad),]
> dim(ficoperfgood)
[1] 2304    5
> dim(ficoperf)
[1] 5314    5

colnames(ficoperf)[3]<-"FICO"
colnames(ficoperfgood)[3]<-"FICO"

> length(ficoperfgood$FICO[which(ficoperfgood$Arrears45==1)])/dim(ficoperfgood)[1]
[1] 0.3307292
> length(ficoperfbad$FICO[which(ficoperfbad$Arrears45==1)])/dim(ficoperfbad)[1]
[1] 0.4398671
> length(ficoperf$FICO[which(ficoperf$Arrears45==1)])/dim(ficoperf)[1]
[1] 0.392548

> mean(ficoperfgood$MixedScore)
[1] 823.4136
> mean(ficoperfbad$MixedScore)
[1] 766.4146
> mean(ficoperf$MixedScore)
[1] 791.137
> sd(ficoperfbad$MixedScore)
[1] 158.3854
> sd(ficoperfgood$MixedScore)
[1] 164.3048
> sd(ficoperf$MixedScore)
[1] 163.4247

> ficoperf$Goodfico<-ifelse(ficoperf$FICO>680,"FICOGood","FICOBad")
> ficoperf$Goodmixedscore<-ifelse(ficoperf$MixedScore>583,"MscoreGood","MscoreBad")
c<-data.frame(ficoperf$Goodfico,ficoperf$Goodmixedscore)
table(c)

p<-ggplot(ficoperf,aes(x=FICO,y=MixedScore))
p+geom_point()
p+geom_point(aes(colour=factor(Arrears45)))

> sum(ficoperf$Arrears45[which(ficoperf$Goodfico=="FICOGood"&ficoperf$Goodmixedscore=="MscoreGood")])/length(ficoperf$Arrears45[which(ficoperf$Goodfico=="FICOGood"&ficoperf$Goodmixedscore=="MscoreGood")])
[1] 0.3082742
> sum(ficoperf$Arrears45[which(ficoperf$Goodfico=="FICOGood"&ficoperf$Goodmixedscore=="MscoreBad")])/length(ficoperf$Arrears45[which(ficoperf$Goodfico=="FICOGood"&ficoperf$Goodmixedscore=="MscoreBad")])
[1] 0.5820106
> sum(ficoperf$Arrears45[which(ficoperf$Goodfico=="FICOBad"&ficoperf$Goodmixedscore=="MscoreBad")])/length(ficoperf$Arrears45[which(ficoperf$Goodfico=="FICOBad"&ficoperf$Goodmixedscore=="MscoreBad")])
[1] 0.6858407
> sum(ficoperf$Arrears45[which(ficoperf$Goodfico=="FICOBad"&ficoperf$Goodmixedscore=="MscoreGood")])/length(ficoperf$Arrears45[which(ficoperf$Goodfico=="FICOBad"&ficoperf$Goodmixedscore=="MscoreGood")])
[1] 0.3963224
> cor(ficoperf$MixedScore,ficoperf$Arrears45)
[1] -0.3910871
> cor(ficoperf$FICO,ficoperf$Arrears45)
[1] -0.1432947

> p<-ggplot(ficoperfall,aes(x=FICO,y=MixedScore))
> p+geom_point()
> p+geom_point(aes(colour=factor(Accepted)))



ficoperfall<-read.csv("ficoperfall.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
ficoperfall$Accepted<-ifelse(is.na(ficoperfall$AgreementReference),0,1)
colnames(ficoperfall)[1]<-"ProposalID"
colnames(ficoperfall)[3]<-"FICO"
ficoperfall$Accdepted<-ifelse(is.na(ficoperfall$AgreementReference),0,1)

> p<-ggplot(ficoperfall,aes(x=FICO,y=MixedScore))
> p+geom_point()
> p+geom_point(aes(colour=factor(Accepted)))

> ficoperfall$Goodfico<-ifelse(ficoperfall$FICO>680,"FICOGood","FICOBad")
> ficoperfall$Goodmixedscore<-ifelse(ficoperfall$MixedScore>583,"MscoreGood","MscoreBad")
> d<-data.frame(ficoperfall$Goodfico,ficoperfall$Goodmixedscore)
> table(d)

sum(ficoperfall$Accepted[which(ficoperfall$Goodfico=="FICOGood"&ficoperfall$Goodmixedscore=="MscoreGood")])/length(ficoperfall$Accepted[which(ficoperfall$Goodfico=="FICOGood"&ficoperfall$Goodmixedscore=="MscoreGood")])

sum(ficoperfall$Accepted[which(ficoperfall$Goodfico=="FICOGood"&ficoperfall$Goodmixedscore=="MscoreBad")])/length(ficoperfall$Accepted[which(ficoperfall$Goodfico=="FICOGood"&ficoperfall$Goodmixedscore=="MscoreBad")])

sum(ficoperfall$Accepted[which(ficoperfall$Goodfico=="FICOBad"&ficoperfall$Goodmixedscore=="MscoreBad")])/length(ficoperfall$Accepted[which(ficoperfall$Goodfico=="FICOBad"&ficoperfall$Goodmixedscore=="MscoreBad")])

sum(ficoperfall$Accepted[which(ficoperfall$Goodfico=="FICOBad"&ficoperfall$Goodmixedscore=="MscoreGood")])/length(ficoperfall$Accepted[which(ficoperfall$Goodfico=="FICOBad"&ficoperfall$Goodmixedscore=="MscoreGood")])


ficoperf$ficoprob<-ficoperf$FICO/1354
ficoperf$mscoreprob<-ficoperf$MixedScore/1000

require(ROCR)
opar<-par(no.readonly=TRUE)
par(mfrow=c(1,2))
pred<-prediction(1-ficoperf$ficoprob,ficoperf$Arrears45)
auc<-performance(pred,"auc")
auc
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC curve for fico and Arrears45",colorize=T)
pred<-prediction(1-ficoperf$mscoreprob,ficoperf$Arrears45)
auc<-performance(pred,"auc")
auc
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC curve for mixed score and Arrears45",colorize=T)





