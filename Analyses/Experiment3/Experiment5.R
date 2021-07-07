rm(list=ls())
library(ggplot2)
library(tidyverse)
library(heplots)
library(pwr)
library(lsr)
library(afex)
library(effsize)
#setwd("~/Desktop/Active Studies/Overreaction/data/Experiment 5")
setwd("../data/SupplementalExperiment1")
dat <- read.csv("OverreactionExp5.csv")

dat.good <- dat %>%
  filter(GNG == 1)

dat.condCount <- dat.good %>%
  group_by(Risk) %>%
  summarize(n=n())

# Preregistered analysis: Simple mixed 2x2 of scenario and risk.
dat.long <- dat.good %>%
  gather(key="Scenario", value="PercentRisk", DamPercentage, FirePercentage) %>%
  mutate(Scenario = factor(Scenario))

fullmodel <- mixed(PercentRisk~Risk*Scenario + (1|ResponseId), data=dat.long)
fullmodel
#both main effects and interaction, but risk is definitely the big one.


# Dam only
t.test(DamPercentage~Risk, data=dat.good, var.equal=TRUE)
cohen.d(DamPercentage~Risk, data=dat.good, var.equal=TRUE)
# Fire only
t.test(FirePercentage~Risk, data=dat.good, var.equal=TRUE)
cohen.d(FirePercentage~Risk, data=dat.good, var.equal=TRUE)
# Descriptives
dat.summary <- dat.good %>%
  group_by(Risk) %>%
  summarize(n=n(), meanDam=mean(DamPercentage), sdDam=sd(DamPercentage),meanFire=mean(FirePercentage),sdFire=sd(FirePercentage))

#Looks like a slightly stronger effect in the fire scenario.

# Simple descriptives: how many alternatives?
dat.alternatives.summary <- dat.good %>%
  group_by(Risk) %>%
  summarize(n=n(),DamAlternatives = sum(DamInterventionYN), FireAlternatives=sum(FireInterventionYN))
# Interesting. Might be an effect of risk with more alternatives provided in the low-risk conditions.

# Post-hoc
dat.alternatives <- dat.good %>%
  gather(key="Scenario", value="Alternative", DamInterventionYN, FireInterventionYN) %>%
  mutate(Scenario = factor(Scenario))

altmodel <- mixed(Alternative~Risk*Scenario + (1|ResponseId), data=dat.alternatives)
summary(altmodel)
# No significant effects, though at p = .0567 and a categorical dv I'm not going to hang my hat on it.
altchisq <- chisq.test(dat.alternatives$Alternative, dat.alternatives$Risk)
altchisq
# Yeah, in a straight chi-square it's at p=.04. Iffy at best.
