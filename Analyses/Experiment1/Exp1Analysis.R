rm(list=ls())
library(ggplot2)
library(tidyverse)
library(heplots)
library(pwr)
library(lsr)
library(afex)
library(effsize)
setwd("../../data/Experiment1")
#setwd("~/Desktop/Active Studies/Overreaction/data/Experiment 1")

dat <- read.csv("OverreactionExp1.csv")

dat.2 <- dat %>%
  filter(OverallGNG == 1)

dat.diffscores <- dat.2 %>%
  mutate(DamDif = DamPost_1 - DamPre_1, FireDif = FirePost_1 - FirePre_1)

# First check conditions
dat.condCount <- dat.2 %>%
  group_by(Risk,Outcome,Causal) %>%
  summarize(n=n())

# Analysis of preratings by scenario
dat.pre.scenario <- dat.2 %>%
  gather(key="Scenario", value="Rating", DamPre_1, FirePre_1) %>%
  mutate(Scenario = factor(Scenario))

prescenario <- mixed(Rating~Risk*Scenario+ (1|ResponseId), data=dat.pre.scenario)
prescenario
# Strong interaction.

# Analysis of post-ratings by scenario
dat.post.scenario <- dat.2 %>%
  gather(key="Scenario", value="Rating", DamPost_1, FirePost_1) %>%
  mutate(Scenario = factor(Scenario))

postscenario <- mixed(Rating~Risk*Outcome*Causal*Scenario + (1|ResponseId), dat=dat.post.scenario)
postscenario
# Full four-way interaction. 

#Let's try the overall analysis with dif scores.
dat.post.scenario.diff <- dat.diffscores %>%
  gather(key="Scenario", value="Diffscore", DamDif, FireDif) %>%
  mutate(Scenario = factor(Scenario))

post.diff.mixed <- mixed(Diffscore~Risk*Outcome*Causal*Scenario + (1|ResponseId), dat=dat.post.scenario.diff)
post.diff.mixed
# No, still produces a four-way interaction.

# So we will be analyzing this separately for each scenario probably.

# All descriptive statistics
dat.all.descriptives <- dat.diffscores %>%
  group_by(Risk, Outcome, Causal) %>%
  summarize(meanDamPre = mean(DamPre_1), sdDamPre = sd(DamPre_1), meanFirePre = mean(FirePre_1), sdFirePre = sd(FirePre_1),
             meanDamPost = mean(DamPost_1), sdDamPost = sd(DamPost_1), meanFirePost = mean(FirePost_1), sdFirePost = sd(FirePost_1),
             meanDamDiff = mean(DamDif), sdDamDiff = sd(DamDif), meanFireDiff = mean(FireDif), sdFireDiff = sd(FireDif),
             pDamPre = t.test(DamPre_1, mu=50)$p.value, pFirePre = t.test(FirePre_1, mu=50)$p.value, 
             pDamPost = t.test(DamPost_1, mu=50)$p.value, pFirePost = t.test(FirePost_1, mu=50)$p.value, .groups='keep')



dat.pre.descriptives <- dat.diffscores %>%
  group_by(Risk) %>%
  summarize (meanDamPre = mean(DamPre_1), sdDamPre = sd(DamPre_1), meanFirePre = mean(FirePre_1), sdFirePre = sd(FirePre_1))


# Let's graph pre-ratings by scenario

pre.plot  <- ggplot(dat.pre.scenario, aes(x = Scenario, y=Rating, fill=Risk)) +
  geom_bar(stat='summary', position='dodge') +
  geom_errorbar(stat='summary', position='dodge')
pre.plot


# Dam
dam.pre <- t.test(dat.2$DamPre_1~dat.2$Risk, paired=FALSE, var.equal=TRUE)
dam.pre   # Clear expected effect
cohen.d(dat.2$DamPre_1~dat.2$Risk)

# one-way against 50
dam.pre.high <- dat.2 %>%
  filter(Risk == 'High')
dam.pre.low <- dat.2 %>%
  filter(Risk == 'Low')
t.test(dam.pre.high$DamPre_1, mu=50)
t.test(dam.pre.low$DamPre_1, mu=50)

dam.post <- aov(DamPost_1~Risk*Outcome*Causal, data=dat.2)
dam.post
summary(dam.post)
# Three-way interaction. 
etasq(dam.post)

dampostplot1 <- ggplot(dat.2, aes(x = Outcome, fill = Causal, y=DamPost_1)) +
  facet_wrap(~Risk) +
  geom_bar(stat='summary', position='dodge') +
  geom_errorbar(stat='summary', position='dodge')

dampostplot1  

# Split by risk, high risk shows what looks like a main effect of outcome, while low risk
# shows an interaction between causality and outcome. 
# For the dam, post-ratings show that in low-risk cases, causality matters, but in high-
# risk cases, it does not. Particularly true in good outcomes.

dat.highrisk <- dat.2 %>%
  filter(Risk == "High")
dam.post.high <- aov(DamPost_1~Outcome*Causal, data=dat.highrisk)
summary(dam.post.high) # Main effects of outcome,  causality

dat.lowrisk <- dat.2 %>%
  filter(Risk == "Low")
dam.post.low <- aov(DamPost_1~Outcome*Causal, data=dat.lowrisk)
summary(dam.post.low) # both main effects, and huge interaction.
# Decompose to outcome
dat.lowrisk.bad <- dat.lowrisk %>%
  filter(Outcome == 'Bad')
t.test(dat.lowrisk.bad$DamPost_1~dat.lowrisk.bad$Causal, paired=FALSE, var.equal=TRUE)
dat.lowrisk.good <- dat.lowrisk %>%
  filter(Outcome == 'Good')
t.test(dat.lowrisk.good$DamPost_1~dat.lowrisk.good$Causal, paired=FALSE, var.equal=TRUE)


# Difference scores


dam.dif <- aov(DamDif~Risk*Outcome*Causal, data=dat.diffscores)
summary(dam.dif)
# Similar to post-ratings alone.

damdiffplot1 <- ggplot(dat.diffscores, aes(x = Outcome, fill = Causal, y=DamDif)) +
  facet_wrap(~Risk) +
  geom_bar(stat='summary', position='dodge')+
  geom_errorbar(stat='summary', position='dodge')

damdiffplot1  
# oh that IS interesting. Up to 20 points of movement, but the most interesting thing
# is that good outcomes have little movment, except low/good/causal which has a huge drop.


# Fire

fire.pre <- t.test(dat.2$FirePre_1~dat.2$Risk, paired=FALSE, var.equal=TRUE)
fire.pre   # Clear expected main effect.
cohen.d(dat.2$FirePre_1~dat.2$Risk)
# Compare ratings to midpoint
t.test(dam.pre.high$FirePre_1, mu=50)
t.test(dam.pre.low$FirePre_1, mu=50)

fire.post <- aov(FirePost_1~Risk*Outcome*Causal, data=dat.2)
summary(fire.post)
etasq(fire.post)
# Interesting, this time there's just two main efects of outcome and causality.

firepostplot1 <- ggplot(dat.2, aes(x = Outcome, fill = Causal, y=FirePost_1)) +
  facet_wrap(~Risk) +
  geom_bar(stat='summary', position='dodge') +
  geom_errorbar(stat='summary', position='dodge')
firepostplot1 
# Higher ratings of good outcome, higher ratings in non-causal. OK. That's clear.

fire.dif <- aov(FireDif~Risk*Outcome*Causal, data=dat.diffscores)
summary(fire.dif)
# Basically identical to the post ratings alonge.

firediffplot1 <- ggplot(dat.diffscores, aes(x = Outcome, fill = Causal, y=FireDif)) +
  facet_wrap(~Risk) +
  geom_bar(stat='summary', position='dodge') +
  geom_errorbar(stat='summary', position='dodge')
firediffplot1  
# Actually similar to dam at first glance, good/low/causal stands out again.

# Nice-looking graphs
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
riskPalette <- c("#0072B2", "#D55E00")
causalPalette <- c("#009E73","#CC79A7")

levels(dat.pre.scenario$Scenario) = c("Dam","Fire")

dodge <- position_dodge(width=.9)
pre.plot  <- ggplot(dat.pre.scenario, aes(x = Scenario, y=Rating, fill=Risk)) +
  geom_boxplot() +
  ggtitle("A")+
  #geom_errorbar(stat='summary', position=dodge,width=.5) +
  geom_hline(yintercept=50, linetype="dashed")+
  scale_y_continuous(limits=c(0,100)) +
  scale_fill_manual(values=riskPalette) +
  theme(panel.background = element_rect(fill='white',color='white'))
pre.plot

levels(dat.post.scenario$Scenario) = c("Dam","Fire")
levels(dat.post.scenario$Causal) = c("Causal", "Non-Causal")
levels(dat.post.scenario$Risk) = c("High Risk", "Low Risk")

post.plot <- ggplot(dat.post.scenario, aes(x=Outcome, y=Rating, fill=Causal)) +
  facet_wrap(~Scenario*Risk) +
  geom_boxplot() + 
  ggtitle("B") +
  geom_hline(yintercept=50, linetype="dashed") + 
  scale_y_continuous(limits=c(0,100)) +
  scale_fill_manual(values=causalPalette) +
  theme(panel.background = element_rect(fill='white',color='white'))
post.plot


