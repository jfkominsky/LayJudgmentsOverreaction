rm(list=ls())

library(ggplot2)
library(tidyverse)
library(heplots)
library(pwr)
library(lsr)
library(afex)
library(effsize)
library(BayesFactor)
library(rjags)
library(Hmisc)

#setwd("~/Desktop/Active Studies/Overreaction/data/Experiment 4")
setwd("../../Data/Experiment3")
dat <- read.csv('OverreactionExp4.csv')
dat.good <- dat %>% filter(GNG == 1) %>%
  mutate(SeverityScore = HadCOVID*(6-as.numeric(HadSeverity)))


# Let's start with some simple descriptives and plotting.
summaryAllMeasures <- dat.good %>%
  summarise_if(is.numeric,list(mean,sd),na.rm=TRUE)
# Mean overall overreaction score is 39, but interestingly the mean scores for each individual measure are higher.

# Graph some key measures

#Let's get overall overreaction, severity, and how bad on one graph
dat.long.graph1 <- dat.good %>%
  pivot_longer(c(ThreatPublic,ThreatPersonal,HowBad,BetterWorse,OrxnOverall), names_to="Item",values_to="Rating")

SummaryPlots.1 <- ggplot(data=dat.long.graph1, aes(Rating, after_stat(count))) +
  facet_wrap(~Item) +
  geom_density()+
  xlim(c(0,100))
SummaryPlots.1

# Next up, all of the individual overreaction scales. There's a lot of them.
dat.long.graph2 <- dat.good %>%
  pivot_longer(c(OrxnBusinesses,OrxnCurfew,OrxnSchools,OrxnMask,OrxnDistance,OrxnQuarantine,OrxnTravel,
                 OrxnCapacity,OrxnSanitize), names_to="Item",values_to="Rating") %>%
  filter(Rating != 50)
SummaryPlots.2 <- ggplot(data=dat.long.graph2, aes(Rating)) +
  facet_wrap(~Item) +
  geom_density()+
  xlim(c(0,100))
SummaryPlots.2
# Interestingly, most of the individual measures all end up looking like normal distributions around 50 
# with different variance.

# We can start with some basic correlations as well.
corTable <- dat.good[, c(2,3,4,5,6,36,7)]
bruteCor <- rcorr(as.matrix(corTable))
bruteCor
# Overreaction has negative correlations with ThreatPublic, ThreatPersonal, HowBad, 
# and positive with BetterWorse (i.e., things would have been better = more overreaction)
# Unsurprisingly all significant with this much power.
# The varous measures are also all very strongly intercorrelated, except preventmitigate and severityscore.
# orxnoverall does not correlate with severity score, severity correlates NEGATIVELY 
# with threatpublic, threatpersonal, howbad, preventmitigate correlates with orxn, betterworse, howbad.
# Let's try a partial corr
library(psych)
partial.r(corTable)
library(ppcor)
# Problem with pcor is that there is a missing value in prevent/mitigate.
dat.good.noNA <- dat.good %>%
  filter(is.na(PreventMitigate) == FALSE)
corTableNoNA <- dat.good.noNA[, c(2,3,4,5,6,36,7)]
partialCorrelations <- pcor(as.matrix(corTableNoNA))
write.csv(partialCorrelations$estimate, 'partialCorrs.csv')
write.csv(partialCorrelations$p.value, 'partialCorrPVals.csv')
t.test(OrxnOverall~PreventMitigate, data=dat.good)
cohen.d(dat.good$OrxnOverall, dat.good$PreventMitigate)
prevmitSummary <- dat.good %>%
  group_by(PreventMitigate) %>%
  summarise(OrxnMean = mean(OrxnOverall), OrxnSD = sd(OrxnOverall), n=n())



# Does KnowSomeone or HadCovid predict overall overreaction ratings? Let's find out.
t.test(OrxnOverall~HadCOVID, data=dat.good)
# NS, but might need to check by severity. However, it's <10% of sample, so very uneven groups
#Severity:
summary(aov(OrxnOverall~HadSeverity, data=dat.good))
# No effect.
# Summarize means by severity:
dat.byseverity <- dat.good %>%
  group_by(HadSeverity) %>%
  summarise(MeanOrxn = mean(OrxnOverall), ThreatToPublic=mean(ThreatPublic),
              ThreatToSelf=mean(ThreatPersonal),HowBad=mean(HowBad),WouldBeBetter=mean(BetterWorse),
              MeanAge = mean(Age), n=n())

t.test(OrxnOverall~KnowSomeone, data=dat.good)
# NS as well, roughly 25% of sample said yes.
t.test(ThreatPublic~KnowSomeone, data=dat.good) # sig
t.test(ThreatPersonal~KnowSomeone, data=dat.good) # sig
t.test(HowBad~KnowSomeone, data=dat.good) # sig
t.test(BetterWorse~KnowSomeone, data=dat.good) # ns
dat.knowsomeone <- dat.good %>%
  group_by(KnowSomeone) %>%
  summarise(MeanOrxn = mean(OrxnOverall), ThreatToPublic=mean(ThreatPublic),
            ThreatToSelf=mean(ThreatPersonal),HowBad=mean(HowBad),WouldBeBetter=mean(BetterWorse),n=n())

# Let's try to make a composite compliance score.
# Inverse-coded items: MaskPublic. Neutral items (exclude for now) LeftHouse, Order Food
dat.compliance <- dat.good %>%
  mutate(MaskPublicInv = (-1*MaskPublic) + 4) %>%
  mutate(NonComplianceScore = (GoneIndoor + InterstateTravel+ IntlTravel + NoMask + MaskPublicInv)/5) 

# ComplianceScore is "lower is better", that is, lower indicates more compliance.
simpleComplianceRegression <- lm(NonComplianceScore~OrxnOverall, data=dat.compliance)
summary(simpleComplianceRegression)
# Yep it is predictive!
# Graph compliance score distribution and then graph relationship
complianceDescript <- ggplot(data=dat.compliance, aes(NonComplianceScore, after_stat(count))) +
  geom_density() +
  xlim(c(0,4))
complianceDescript
# Generally high levels of compliance, very little variance.
# Correlation
complianceOrxnScatter <- ggplot(data=dat.compliance, aes(x=OrxnOverall, y=NonComplianceScore)) +
  geom_point()+
  stat_smooth(method="lm", col="red") + 
  theme(panel.background = element_rect(fill='white',color='white'))+
  theme(text = element_text(size=20))
complianceOrxnScatter
# Yep, that's predictable. Higher overreaction ratings = less compliance overall. 

# Composite score of individual invervention overreaction scores
dat.orxnavg <- dat.compliance %>%
  mutate(OrxnAvg = (OrxnBusinesses+OrxnCurfew+OrxnSchools+OrxnMask+OrxnDistance+OrxnQuarantine+OrxnTravel+OrxnCapacity+OrxnSanitize)/9) %>%
  mutate(MaskPublicInv = (-1*MaskPublic) + 4) %>%
  mutate(NonComplianceScore = (GoneIndoor + InterstateTravel+ IntlTravel + NoMask + MaskPublicInv)/5)

# correlate composite score w/overall score
rcorr(dat.orxnavg$OrxnOverall,dat.orxnavg$OrxnAvg)
# r=.68, p<.01
# Summarize overreaction average
dat.summ.avg <- dat.orxnavg %>%
  summarise_if(is.numeric,list(mean,sd),na.rm=TRUE)
#  average of averages is 49, compared to 39 for the overall.
t.test(dat.orxnavg$OrxnOverall, dat.orxnavg$OrxnAvg)
# Yes, they are significantly different.
# Graph
orxnAvgScatter <- ggplot(data=dat.orxnavg, aes(x=OrxnOverall, y=OrxnAvg)) +
  geom_point() + 
  geom_smooth()
orxnAvgScatter

# Relationship of average orxn score to compliance
complianceOrxnAvgScatter <- ggplot(data=dat.orxnavg, aes(x=OrxnAvg, y=NonComplianceScore)) +
  geom_point() +
  geom_smooth()
complianceOrxnAvgScatter
# little weird.
simpleComplianceRegressionAvg <- lm(NonComplianceScore~OrxnAvg, data=dat.orxnavg)
summary(simpleComplianceRegressionAvg)

#Age
dat.orxnavg <- dat.orxnavg %>%
  filter(Age>0)
ageOrxnScatter <- ggplot(data=dat.orxnavg, aes(x=Age, y=OrxnOverall)) +
  geom_point() +
  geom_smooth()
ageOrxnScatter

ageLM <- lm(OrxnOverall~Age, data=dat.orxnavg)
summary(ageLM)
# There is a significant relationship, however. Weirdly, it's positive.

ageKnowSomeoneGLM <- lm(OrxnOverall~Age*KnowSomeone, data=dat.orxnavg)
summary(ageKnowSomeoneGLM)
# No effect of knowing someone who had covid.


bigComplianceRegression <- lm(ComplianceScore~OrxnOverall*SeverityScore*ThreatPublic*ThreatPersonal*HowBad*BetterWorse*PreventMitigate, data=dat.compliance)
summary(bigComplianceRegression)
# Only surviving main effect is betterworse, which is actually not bad. Ah, and a orxnoverall*betterworse interaction.
# nice significant 4-way too: OrxnOverall:ThreatPublic:ThreatPersonal:BetterWorse
# and: OrxnOverall:ThreatPublic:ThreatPersonal:BetterWorse:PreventMitigate

bigOrxnRegression <- lm(OrxnOverall~SeverityScore*ThreatPublic*ThreatPersonal*HowBad*BetterWorse*PreventMitigate, data=dat.compliance)
summary(bigOrxnRegression)
# Interestingly this gives us NO significant effects or interactions.

# Model comparison approahc w/ and w/out overreaction
model.0 <- lm(NonComplianceScore~SeverityScore*ThreatPublic*ThreatPersonal*HowBad*BetterWorse*PreventMitigate, data=dat.compliance)
model.1 <- lm(NonComplianceScore~OrxnOverall*SeverityScore*ThreatPublic*ThreatPersonal*HowBad*BetterWorse*PreventMitigate, data=dat.compliance)
model.2 <- lm(NonComplianceScore~OrxnOverall+SeverityScore*ThreatPublic*ThreatPersonal*HowBad*BetterWorse*PreventMitigate, data=dat.compliance)
anova(model.0, model.2)
# Significant improvement of fit.
anova(model.1, model.2)
# Adding it as an interaction term is better still, of course, but it adds 48 new things.
anova(model.0, model.1)
summary(model.2)
# estimate of Orxn ME term in this model is beta = .004047, t=4.187, p<.001
summary(model.1)
# In this one it isn't a main effect, but it interacts with a ton of stuff.

modelb.0 <- lm(NonComplianceScore~SeverityScore+ThreatPublic+ThreatPersonal+HowBad+BetterWorse+PreventMitigate, data=dat.compliance)
modelb.1 <- lm(NonComplianceScore~OrxnOverall+SeverityScore+ThreatPublic+ThreatPersonal+HowBad+BetterWorse+PreventMitigate, data=dat.compliance)
anova(modelb.0, modelb.1)
# And even if we only look at main effects and ignore all interactions. Great!


# New approach: Backwards stepwise regression.
library(MASS)
stepwise.model <- stepAIC(model.1, direction="backward")
# Too many factors to really make sense of but Overreaction survives. It basically kills a bunch of interaction terms but leaves many more.
stepwise.model$anova
stepwise.modelb<- stepAIC(modelb.1, direction="backward")
stepwise.modelb$anova
# surprisingly only removes "threatpublic", best-fitting model inclues everything else.

