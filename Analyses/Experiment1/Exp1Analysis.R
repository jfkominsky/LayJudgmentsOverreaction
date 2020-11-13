rm(list=ls())
library(ggplot2)
library(tidyverse)
library(heplots)
library(pwr)
library(lsr)
library(afex)
setwd("../../data/Experiment1")

dat <- read.csv("OverreactionExp1.csv")

dat.2 <- dat %>%
  filter(OverallGNG == 1)

# First check conditions
dat.condCount <- dat.2 %>%
  group_by(Risk,Outcome,Causal) %>%
  summarize(n=n())

# Analysis of preratings by scenario
dat.pre.scenario <- dat %>%
  gather(key="Scenario", value="Rating", DamPre_1, FirePre_1) %>%
  mutate(Scenario = factor(Scenario))

prescenario <- mixed(Rating~Risk*Scenario+ (1|ResponseId), data=dat.pre.scenario)
prescenario
# Strong interaction.

# Analysis of post-ratings by scenario
dat.post.scenario <- dat %>%
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

# Let's graph pre-ratings by scenario

pre.plot  <- ggplot(dat.pre.scenario, aes(x = Scenario, y=Rating, fill=Risk)) +
  geom_bar(stat='summary', position='dodge') +
  geom_errorbar(stat='summary', position='dodge')
pre.plot


# Dam
dam.pre <- t.test(dat.2$DamPre_1~dat.2$Risk)
dam.pre   # Clear expected effect

dam.post <- aov(DamPost_1~Risk*Outcome*Causal, data=dat.2)
dam.post
summary(dam.post)
# Three-way interaction. 

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
summary(dam.post.high) # Main effect outcome, nothing else.

dat.lowrisk <- dat.2 %>%
  filter(Risk == "Low")
dam.post.low <- aov(DamPost_1~Outcome*Causal, data=dat.lowrisk)
summary(dam.post.low) # both main effects, but huge interaction.

# Difference scores

dat.diffscores <- dat.2 %>%
  mutate(DamDif = DamPost_1 - DamPre_1, FireDif = FirePost_1 - FirePre_1)

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

fire.pre <- t.test(dat.2$FirePre_1~dat.2$Risk)
fire.pre   # Clear expected main effect.

fire.post <- aov(FirePost_1~Risk*Outcome*Causal, data=dat.2)
fire.post
summary(fire.post)
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
  geom_bar(stat='summary', position='dodge')
firediffplot1  
# Actually similar to dam at first glance, good/low/causal stands out again.


