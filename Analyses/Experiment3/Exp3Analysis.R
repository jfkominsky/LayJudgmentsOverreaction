rm(list=ls())
library(ggplot2)
library(tidyverse)
library(heplots)
library(pwr)
library(lsr)
library(afex)
library(effsize)
setwd("../../data/Experiment3")
#setwd("~/Desktop/Active Studies/Overreaction/data/Experiment 3")

dat <- read.csv("OverreactionExp3.csv")

dat.2 <- dat %>%
  filter(OverallGNG == 1)

# First check conditions
dat.condCount <- dat.2 %>%
  group_by(Risk,Intent,Outcome) %>%
  summarize(n=n())

# What is going on here.
dat.failcheck <- dat %>%
  group_by(Risk,Intent,Outcome) %>%
  summarize(meanFRisk = mean(FRiskCheck), meanFIntent = mean(FIntentCheck), meanDRisk = mean(DRiskCheck), meanDIntent = mean(DIntentCheck))
  

# Analysis of preratings by scenario
dat.pre.scenario <- dat.2 %>%
  gather(key="Scenario", value="Rating", DamPre_1, FirePre_1) %>%
  mutate(Scenario = factor(Scenario))

prescenario <- mixed(Rating~Risk*Intent*Scenario+ (1|ResponseId), data=dat.pre.scenario)
prescenario
# Risk, Scenario, no interactions

# Analysis of post-ratings by scenario
dat.post.scenario <- dat.2 %>%
  gather(key="Scenario", value="Rating", DamPost_1, FirePost_1) %>%
  mutate(Scenario = factor(Scenario))

postscenario <- mixed(Rating~Risk*Intent*Outcome*Scenario + (1|ResponseId), dat=dat.post.scenario)
postscenario
# small outcome*scenario interaction, main effects of risk, outcome, and scenario.

#Let's try the overall analysis with dif scores.
dat.diffscores <- dat.2 %>%
  mutate(DamDif = DamPost_1 - DamPre_1, FireDif = FirePost_1 - FirePre_1)
dat.post.scenario.diff <- dat.diffscores %>%
  gather(key="Scenario", value="Diffscore", DamDif, FireDif) %>%
  mutate(Scenario = factor(Scenario))

post.diff.mixed <- mixed(Diffscore~Risk*Intent*Outcome*Scenario + (1|ResponseId), dat=dat.post.scenario.diff)
post.diff.mixed
# Risk*outcome*scenario interaction and a risk*outcome interaction, as well as ME of Risk, Outcome, and Scenario.

# Bottom line is that "intent" might have an effect on pre-ratings but that's it.

# Let's graph pre-ratings by scenario

pre.plot  <- ggplot(dat.pre.scenario, aes(x = Intent, y=Rating, fill=Risk)) +
  facet_wrap(~Scenario) +
  #geom_bar(stat='summary', position='dodge') +
  geom_violin()+
  geom_point(position=position_dodge(width=.9))+
  geom_errorbar(stat='summary', position='dodge')
pre.plot
# Basically looks like effect of risk stronger in the dam case.The overall ME of intent is prevent > mitigate.

# Combined analyses.
dat.combined<- dat.2 %>%
  mutate(PreRating = (DamPre_1+FirePre_1)/2, PostRating = (DamPost_1 + FirePost_1)/2)

pre.analysis <- aov(PreRating~Risk*Intent, data=dat.combined)
summary(pre.analysis)
etasq(pre.analysis)
# Only a main effect of risk.
TukeyHSD(pre.analysis) # Low > high
# Descriptives
dat.pre.descriptives.risk <- dat.combined %>%
  group_by(Risk) %>%
  summarize(Mean = mean(PreRating), SD = sd(PreRating))
# Vs. 50
dat.pre.lowrisk <- dat.combined %>%
  filter(Risk == 'Low')
dat.pre.highrisk <- dat.combined %>%
  filter(Risk == 'High')
t.test(dat.pre.lowrisk$PreRating, mu=50)
t.test(dat.pre.highrisk$PreRating, mu=50)

# Post
post.analysis <- aov(PostRating~Risk*Intent*Outcome, data=dat.combined)
summary(post.analysis)
etasq(post.analysis)
# Risk and outcome
TukeyHSD(post.analysis)
# Low > High and Good > Bad
# Descriptives
dat.post.descriptives.risk <- dat.combined %>%
  group_by(Risk) %>%
  summarize(Mean = mean(PostRating), SD = sd(PostRating))
dat.post.descriptives.outcome <- dat.combined %>%
  group_by(Outcome) %>%
  summarize(Mean = mean(PostRating), SD = sd(PostRating))
# For comparisons against 50, do we want to do each individual cell? No, each split
t.test(dat.pre.lowrisk$PostRating, mu=50)
t.test(dat.pre.highrisk$PostRating, mu=50)
# high risk sig. below, low risk not sig dif.
dat.post.badoutcome <- dat.combined %>%
  filter(Outcome == 'Bad')
dat.post.goodoutcome <- dat.combined %>%
  filter(Outcome == 'Good')
t.test(dat.post.badoutcome$PostRating, mu=50)
t.test(dat.post.goodoutcome$PostRating, mu=50)
#bad sig below, good sig above.

dat.post.low.bad <- dat.post.badoutcome %>%
  filter(Risk=='Low')
dat.post.low.good <- dat.post.goodoutcome %>%
  filter(Risk=='Low')
dat.post.high.bad <- dat.post.badoutcome %>%
  filter(Risk=='High')
dat.post.high.good <- dat.post.goodoutcome %>%
  filter(Risk=='High')

t.test(dat.post.low.bad$PostRating, mu=50)
t.test(dat.post.low.good$PostRating, mu=50)
t.test(dat.post.high.bad$PostRating, mu=50)
t.test(dat.post.high.good$PostRating, mu=50)

# Dam
dam.pre <- aov(DamPre_1~Risk*Intent, data = dat.2)
summary(dam.pre)
#ME of Risk and intent.

dam.post <- aov(DamPost_1~Risk*Intent*Outcome, data=dat.2)
summary(dam.post)
# ME of risk and outcome, but not intent.

dampostplot1 <- ggplot(dat.2, aes(x = Intent, fill = Risk, y=DamPost_1)) +
  facet_wrap(~Outcome) +
  #geom_bar(stat='summary', position='dodge') +
  geom_violin()+
  geom_point(position=position_dodge(width=.9))+
  geom_errorbar(stat='summary', position='dodge')

dampostplot1  


# Difference scores redux

dam.dif <- aov(DamDif~Risk*Intent*Outcome, data=dat.diffscores)
summary(dam.dif)
# Risk interacts with outcome, and each has main effects.

damdiffplot1 <- ggplot(dat.diffscores, aes(x = Intent, fill = Risk, y=DamDif)) +
  facet_wrap(~Outcome) +
  #geom_bar(stat='summary', position='dodge')+
  geom_violin()+
  geom_point(position=position_dodge(width=.9))+
  geom_errorbar(stat='summary', position='dodge')

damdiffplot1  
# Bad outcome is uniform negative, good outcome shows strong effect of risk.


# Fire

fire.pre <- aov(FirePre_1~Risk*Intent, data=dat.2)
summary(fire.pre)   
# ME of risk only. Interaction with scenario is, at worst, the me of risk is stronger in the dam case.

fire.post <- aov(FirePost_1~Risk*Intent*Outcome, data=dat.2)
summary(fire.post)
# MEs of Risk and Outcome. Basically same as the dam.

firepostplot1 <- ggplot(dat.2, aes(x = Intent, fill = Risk, y=FirePost_1)) +
  facet_wrap(~Outcome) +
  #geom_bar(stat='summary', position='dodge') +
  geom_violin()+
  geom_point(position=position_dodge(width=.9))+
  geom_errorbar(stat='summary', position='dodge')
firepostplot1 
#  Odd. It looks like there should be an interaction, but basically just ME of risk (low > high) and outcome (good > bad)

fire.dif <- aov(FireDif~Risk*Intent*Outcome, data=dat.diffscores)
summary(fire.dif)
# Outcome and risk, but no interaction this time. That's the 3-way interaction.

firediffplot1 <- ggplot(dat.diffscores, aes(x = Intent, fill = Risk, y=FireDif)) +
  facet_wrap(~Outcome) +
  geom_bar(stat='summary', position='dodge') +
  geom_errorbar(stat='summary', position = "dodge")
firediffplot1  
# Very similar to dam, but with the effect of risk no longer interacting with outcome.

# Nice-looking graphs

riskPalette <- c("#0072B2", "#D55E00")
causalPalette <- c("#009E73","#CC79A7")
dodge <- position_dodge(width=.9)
pre.plot  <- ggplot(dat.pre.scenario, aes(x = Intent, y=Rating, fill=Risk)) +
  geom_boxplot() +
  ggtitle("A")+
  scale_y_continuous(limits=c(0,100)) +
  scale_fill_manual(values=riskPalette) +
  geom_hline(yintercept=50, linetype="dashed") + 
  theme(panel.background = element_rect(fill='white',color='white'),
        legend.position = "none") 
  
pre.plot

post.plot <- ggplot(dat.post.scenario, aes(x=Outcome, y=Rating, fill=Risk)) +
  facet_wrap(~Intent) +
  geom_boxplot() + 
  ggtitle("B")+
  scale_y_continuous(limits=c(0,100)) +
  geom_hline(yintercept=50, linetype="dashed") + 
  scale_fill_manual(values=riskPalette) +
  theme(panel.background = element_rect(fill='white',color='white'))
post.plot

