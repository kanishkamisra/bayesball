library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)
library(ggthemes)
library(broom)

#colors: 
yellow <- "#f1c40f"
blue <- "#30a2da"
red <- "#fc4f30"
green <- "#77ab43"
purple <- "#9b59b6"

batters <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(Hits = sum(H), AB = sum(AB)) %>%
  mutate(avg = Hits/AB)

batters <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(batters, by = "playerID")

batters_filtered <- batters %>%
  filter(AB >= 500)

library(MASS)
#Fit a maximum likelihood model to the beta distribution
mle <- fitdistr(batters_filtered$avg, "beta", start = list(shape1 = 1, shape2 = 12))
tidy(mle)

#get estimates for alpha and beta
alpha = tidy(mle)$estimate[1]
beta = tidy(mle)$estimate[2]

batters_estimates <- batters_filtered %>%
  mutate(estimate = (alpha + Hits)/(alpha + beta + AB), alpha1 = (Hits + alpha), beta1 = (AB - Hits + beta))

# They tell me Hank Aaron is a legendary player, with a career average of 0.300.
hank_aaron <- batters_estimates %>%
  filter(name == "Hank Aaron")

hank_aaron_estimate <- hank_aaron$estimate
# Approx 0.309, this leads us to suspect that his true probability of hitting is more than .300

# Let's look at his posterior distribution
hank_aaron %>%
  do(data_frame(x = seq(0.27, 0.33, 0.0002),
                density = dbeta(x, .$alpha1, .$beta1))) %>%
  ggplot(aes(x, density)) + 
  geom_line(size = 1.25) +
  geom_ribbon(aes(ymin = 0, ymax = density * (x < .3)),
              alpha = .3, fill = yellow) +
  geom_vline(color = red, lty = 2, xintercept = .3, size = 1.2) +
  theme_fivethirtyeight() + 
  ggtitle("Hank Aaron's posterior distribution")

# There is a probability that his true average is less than 0.300

hank_aaron

# probability of hank's true average lying inside the yellow area:

pbeta(0.3, hank_aaron$alpha1, hank_aaron$beta1)

# 17% chance that his true probability of hitting is actualyl less than 0.300
# This metric is called the Posterior Exclusion Probability (PEP)
# PEP = 1 - PIP or the Posterior Inclusion Probability
# Let's do this for all players and check their chances of being above 0.300

batters_estimates <- batters_estimates %>%
  mutate(PEP = pbeta(0.3, alpha1, beta1))

batters_estimates %>%
  ggplot(aes(estimate, PEP, color = AB)) +
  geom_point(size = 1) +
  xlab("(Shrunken) batting average estimate") +
  ylab("Posterior Error Probability (PEP)") +
  geom_vline(color = "red", lty = 2, xintercept = .3) +
  scale_colour_gradient(trans = "log", breaks = 10 ^ (1:5))
