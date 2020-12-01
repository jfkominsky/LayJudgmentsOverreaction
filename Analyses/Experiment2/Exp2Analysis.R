rm(list=ls())
library(ggplot2)
library(tidyverse)
library(heplots)
library(pwr)
library(lsr)
library(afex)
setwd("../../data/Experiment2")
#setwd("~/Desktop/Active Studies/Overreaction/data/Experiment 2")

dat <- read.csv("OverreactionExp2.csv")

dat.2 <- dat %>%
  filter(OverallGNG == 1)

# First check conditions
dat.condCount <- dat.2 %>%
  group_by(Intervention,Valence,Realism) %>%
  summarize(n=n())

# Analysis of preratings by scenario
dat.pre.scenario <- dat %>%
  gather(key="Scenario", value="Rating", DamPre_1, FirePre_1) %>%
  mutate(Scenario = factor(Scenario))

# Analysis of post-ratings by scenario
dat.post.scenario <- dat %>%
  gather(key="Scenario", value="Rating", DamPost_1, FirePost_1) %>%
  mutate(Scenario = factor(Scenario))

# Here's an initial analysis with a single one-way factor for condition.
postscenario.coarse = mixed(Rating~CondCode*Scenario + (1|ResponseId), dat=dat.post.scenario)
postscenario.coarse
# Main effect of condition. This time scenario doesn't matter, so they will be combined.

# With separated factors
postscenario <- mixed(Rating~Valence*Intervention*Realism*Scenario + (1|ResponseId), dat=dat.post.scenario)
postscenario
# No interactions, just all main effects, valence, intervention, and realism! 

# Let's graph pre-ratings by scenario

pre.plot  <- ggplot(dat.pre.scenario, aes(x = Scenario, y=Rating, fill=CondCode)) +
  geom_bar(stat='summary', position='dodge') +
  geom_errorbar(stat='summary', position='dodge')
pre.plot 

pre.plot.split <- ggplot(dat.pre.scenario, aes(x = Valence, y=Rating, fill=Realism)) +
  facet_wrap(~Intervention)+
  geom_bar(stat='summary', position='dodge') +
  geom_errorbar(stat='summary', position='dodge')
pre.plot.split # Pre-ratings are different despite being identical across all conditions. Random variance?
  
post.plot <- ggplot(dat.post.scenario, aes(x = Scenario, y=Rating, fill=CondCode)) +
  geom_bar(stat='summary', position='dodge') +
  geom_errorbar(stat='summary', position='dodge')
post.plot

post.plot.split <- ggplot(dat.post.scenario, aes(x = Valence, y=Rating, fill=Realism)) +
  facet_wrap(~Intervention)+
  geom_bar(stat='summary', position='dodge') +
  geom_errorbar(stat='summary', position='dodge')
post.plot.split

# Analysis of post-ratings collapsing across scenario
post_ratings.oneway = aov(Rating~CondCode, dat=dat.post.scenario)
summary(post_ratings.oneway)

post_ratings.factorial = aov(Rating~Valence*Intervention*Realism,data=dat.post.scenario)
summary(post_ratings.factorial)
# Three main effects, marginal valence * intervention interaction. Interesting.
TukeyHSD(post_ratings.factorial)
# Valence: Good > Bad
#Intervention: Without > With
#Realism: Unrealistic > the other two.
#Valence*intervention: Effect of intervention for good but not bad valence ish.


# Post-hoc analyses of pre-ratings (with scenario):
pre_ratings.sceanrio = mixed(Rating~Valence*Intervention*Realism*Scenario + (1|ResponseId),data=dat.pre.scenario)
pre_ratings.sceanrio
# Main effect of scenario, intervention*realism interaction.

t.test(Rating~Scenario, data=dat.pre.scenario, paired=TRUE)
dat.scenario.summary <- dat.pre.scenario %>%
  group_by(Scenario) %>%
  summarize(mean=mean(Rating))
dat.pre.WithoutIntervention <- dat.pre.scenario %>%
  filter(Intervention == "WithoutIntervention")
dat.pre.Intervention <- dat.pre.scenario %>%
  filter(Intervention == "Intervention")
withoutAOV <- aov(Rating~Realism, data=dat.pre.WithoutIntervention)
summary(withoutAOV)
# No effect of realism in "without intervention" (counterfactual) condition

interventionAOV <- aov(Rating~Realism, data=dat.pre.Intervention)
summary(interventionAOV)
TukeyHSD(interventionAOV)
# Difference between unrealistic and others in "intervention" condition, but again, participants saw
# exactly the same thing as in the without-intervention conditions at the time that they made this rating. 

# Pre-ratings (w/out scenario):
pre_ratings.factorial = aov(Rating~Valence*Intervention*Realism,data=dat.pre.scenario)
summary(pre_ratings.factorial)
# A weird intervention*realism interaction.
TukeyHSD(pre_ratings.factorial)
# Seems to be driven by the difference in the intervention condition between unrealistic and slightly
# being larger than the corresponding difference in the without-intervention condition. Odd.

# Difference scores, which will control for variance in pre-ratings.

diffscores <- dat.2 %>%
  mutate(DamDif = DamPost_1 - DamPre_1, FireDif = FirePost_1 - FirePre_1)

dat.diff.long <- diffscores %>%
  gather(key="Scenario", value="Rating", DamDif, FireDif)
  

diffplot <- ggplot(dat.diff.long, aes(x = Valence, y=Rating, fill=Realism)) +
  facet_wrap(~Intervention)+
  geom_bar(stat='summary', position='dodge') +
  geom_errorbar(stat='summary', position='dodge')
diffplot
# Short version: unrealistic bad changes less than the other two, and counterfactuals tend to fall closer to neutral regarldess of valence.

diff.analysis <-  aov(Rating~Valence*Intervention*Realism,data=dat.diff.long)
summary(diff.analysis)
# Similar to post-ratings except for a valence*intervention interaction. Pretty easy to see,
# withoutIntervention goes positive (post > pre) in the good case but not the bad case, while
# intervention goes negative in both.

dat.diff.Intervention <- dat.diff.long %>%
  filter(Intervention=="Intervention")
dat.diff.WithoutIntervention <- dat.diff.long %>%
  filter(Intervention=="WithoutIntervention")

t.test(Rating~Valence, data = dat.diff.Intervention)
t.test(Rating~Valence, data=dat.diff.WithoutIntervention)


