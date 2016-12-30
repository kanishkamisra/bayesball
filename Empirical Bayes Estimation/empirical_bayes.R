library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)
library(ggthemes)
library(MASS)
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
  select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(batters, by = "playerID") %>%
  select(-playerID)

batters %>%
  filter(AB >= 500) %>%
  ggplot(aes(avg)) +
  geom_histogram(binwidth = .005, fill = yellow) + 
  theme_fivethirtyeight() + 
  ggtitle("Distribution of Batters' Batting Averages")

batters_filtered <- batters %>%
  filter(AB >= 500)

#Fit a maximum likelihood model to the beta distribution
mle <- fitdistr(batters_filtered$avg, "beta", start = list(shape1 = 1, shape2 = 12))
tidy(mle)

#get estimates for alpha and beta
alpha = tidy(mle)$estimate[1]
beta = tidy(mle)$estimate[2]

batters_filtered %>%
  ggplot() +
  geom_histogram(aes(avg, y = ..density..),binwidth = .005, fill = yellow) + 
  stat_function(fun = function(x) dbeta(x, alpha, beta), color = blue, size = 2) + 
  theme_fivethirtyeight() + 
  ggtitle("Fitting beta distribution with estimated priors")

batters_estimates <- batters_filtered %>%
  mutate(estimate = (alpha + Hits)/(alpha + beta + AB))

#Empirical Bayesian shrinkage towards a beta prior xd

ggplot(batters_estimates, aes(avg, estimate, color = AB)) +
  geom_hline(yintercept = alpha / (alpha + beta), color = red, lty = 2) +
  geom_point() +
  geom_abline(color = red) +
  scale_colour_gradient(trans = "log", breaks = 10 ^ (1:5)) +
  xlab("Batting average") +
  ylab("Empirical Bayes batting average")

# These extraordinary outliers need extraordinary evidence


