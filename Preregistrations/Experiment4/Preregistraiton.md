# Experiment 4 Preregistration

## Design

This experiment is designed to examine people's judgments of different measures that have been employed agains the COVID-19 pandemic. In particular, participants are asked whether they feel the measures don't do enough, are appropriate, or are overreactions, using the same 100-point scale as Experiments 1-3. They will make these judgments for all of the regulations that have been employed in aggregate, and then again for a series of individual regulations.

Participants will also be asked to make judgments of several things that Experiments 1-3 have indicated may be relevant to judgments of overreaction. Specifically
- How dangerous the COVID-19 pandemic is
- How bad things have actually been
- Whether things would be worse without these regulations
- Whether the goals of these regulations are to prevent or mitigate the spread of COVID-19

We will also ask, as exploratory measures
- Whether they have engaged in various activities affected by these regulations in the last three months, such as going into public indoor spaces without a mask
- Whether the participant has lost someone close to them to COVID-19
- Whether the participant has had COVID-19
- If the participant has had COVID-19, how severe the symptoms were

## Planned sample size and exclusion criteria
We plan to recruit approximately 450 participants located in the US from Prolific Academic, replacing participants who fail any of our three attention-checks, but our sample will ultimately be restricted by budgetary considerations. We will recruit as close to 450 participants as we can. Recruitment will happen in waves, with an initial sample of 20 to verify the survey works as intended, a subsequent wave of 100 to ensure data quality is acceptable (i.e. examining exclusion rate and descriptive statistics only), and assuming no unexpected issues arrive, the rest of the sample thereafter. 

There are three check questions we will use as exclusion criteria. The first is a yes/no question "Do you live in the US?", anyone who answers "no" will be excluded. The second is a scale item mixed into the ratings of individual measures that instructs participants to move that item as close to 50 as possible. Any response outside the range of 45-55 will be excluded (and participants can see the exact numerical response when using the slider). The third is a free-text response asking participants what state they live in. Any participant who gives a response that is not a state, territory, or state acronym will be excluded.

## Planned analysis

The primary analysis will be a Bayesian computational model that attempts to predict overreaction ratings on the "overall" item based on responses to the predictors listed above. We will estimate the priors for the items based on Experiments 1-3 based on the effect sizes in those studies, while the exploratory predictors will be estimated with neutral priors. We will also examine which individual regulations are most predictive of ratings of COVID-19 regulations as a whole, as well as look for systematic differences in ratings of individual regulations. We also expect to conduct some exploratory correlational analyses to inform future studies, based on any patterns we observe in the data.