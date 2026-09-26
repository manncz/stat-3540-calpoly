# Load libraries ---------------------------------------------------------------

library(tidyverse)

library(lme4)
library(lmerTest)
library(emmeans)

library(multcompView)
library(multcomp)

# set as sum to zero
options(contrasts = c("contr.sum", "contr.poly"))

### Example 6.2: Random treatment effects

personnel_data <- read_csv("02-notes/data/06-personnel-data.csv") |> 
  mutate(across(officer:candidate, as.factor))
personnel_data

personnel_mod <- lmer(rating ~ (1 | officer), REML = TRUE, data = personnel_data)
anova(personnel_mod)
summary(personnel_mod)
ranef(personnel_mod)

personnel_mod2 <- lm(rating ~ 1, data = personnel_data)
anova(personnel_mod2)
summary(personnel_mod2)

### Example 6.3: Running Back Agility
kneebrace_data <- read_csv("02-notes/data/06-kneebrace-data.csv") |> 
  mutate(across(RunningBack:KneeBrace, as.factor))
kneebrace_data

kneebrace_mod <- lmer(AgilityTime ~ KneeBrace + (1 | RunningBack),
                      data = kneebrace_data)
anova(kneebrace_mod)
summary(kneebrace_mod)
ranova(kneebrace_mod)

kneebrace_lmeans <- emmeans(kneebrace_mod, ~ KneeBrace, infer = c(T, T))

emmip(kneebrace_mod, ~ KneeBrace, CIs = T) +
  labs(y = "Estimated Agility (mins)")

kneebrace_lmeans |> 
  cld(Letters = LETTERS, decreasing = T, adjust = 'tukey')

kneebrace_lmeans |> 
  pairs(adjust = 'tukey', infer = c(T,T))

# Example 6.4: Revisit turkeys
turkey_data <- read_csv("02-notes/data/01_turkey_animal_data.csv") |> 
  mutate(across(Diet:Turkey, as.factor))
turkey_data

turkey_mod <- lmer(ADG ~ Diet + (1 | Diet:Pen), data = turkey_data)
anova(turkey_mod)
summary(turkey_mod)

turkey_emmeans <- emmeans(turkey_mod, ~ Diet, infer = c(T, T))

turkey_emmeans |> 
  cld(Letters = LETTERS, decreasing = T, adjust = 'tukey')
