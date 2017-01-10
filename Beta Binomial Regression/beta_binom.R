library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)
library(ggthemes)
library(broom)
library(extrafont)

options(digits = 3)
font_import()
loadfonts(device = "win")

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

#Fit a maximum likelihood model to the beta distribution
mle <- MASS::fitdistr(batters_filtered$avg, "beta", start = list(shape1 = 1, shape2 = 12))
tidy(mle)

#get estimates for alpha and beta
alpha = tidy(mle)$estimate[1]
beta = tidy(mle)$estimate[2]
prior_mu = alpha/(alpha + beta)

batters_estimates <- batters_filtered %>%
  mutate(estimate = (alpha + Hits)/(alpha + beta + AB), alpha1 = (Hits + alpha),
         beta1 = (AB - Hits + beta)) %>%
  arrange(desc(estimate))

# Add this to initial thing as well
# How averages vary over at-bats.
batters %>%
  filter(AB >= 20) %>%
  ggplot(aes(AB, avg)) +
  geom_point(color = yellow) +
  geom_smooth(method = "lm", se = FALSE, color = blue, size = 1.5) +
  scale_x_log10() + 
  theme_fivethirtyeight() + 
  theme(plot.title=element_text(family="Roboto")) +
  theme(axis.title = element_text(family = "Roboto", face = "bold", color="#666666", size = 12)) +
  theme(axis.text = element_text(family = "Roboto", face = "bold", color = "#535353", size = 11)) +
  geom_hline(yintercept=0,size=1.2,colour="#535353") +
  ggtitle("At-bats vs logarithmic batting average")

# Lower ABs have more variance, as AB increases, average also increases.
# Better batters are played more so they tend to increase the average batting average
# as number of games go up.

batters_estimates %>%
  filter(AB >= 20) %>%
  gather(type, value, avg, estimate) %>%
  mutate(type = plyr::revalue(type, c(avg = "Raw",
                                      estimate = "With EB Shrinkage"))) %>%
  ggplot(aes(AB, value)) +
  geom_point(color = yellow) +
  geom_hline(color = red, size = 1.5, yintercept = prior_mu) +
  facet_wrap(~type) +
  ylab("average") +
  geom_smooth(method = "lm", color = blue, size = 1.5) +
  scale_x_log10() +
  theme_fivethirtyeight() + 
  theme(plot.title=element_text(family="Roboto")) +
  theme(axis.title = element_text(family = "Roboto", face = "bold", color="#666666", size = 12)) +
  theme(axis.text = element_text(family = "Roboto", face = "bold", color = "#535353", size = 11)) +
  theme(strip.text.x = element_text(family = "Roboto", face = "bold", size = 11)) + 
  ggtitle("At-bats vs log averages and estimates")
  # geom_hline(yintercept=0,size=1.2,colour="#535353")

# Low AB value players are getting grossly overestimated by the shrinkage model
# There is a need to make a model that considers AB

# By re-parameterizing the true probability(posterior) distribution, we can express it in terms of
# mean of the batting average and how spread out the distribution is
# p ~ Beta(mu/sigma, (1-mu)/sigma)

# To include AB, we can define mu as 
# I give up

library(gamlss)

fit <- gamlss(cbind(Hits, AB - Hits) ~ log(AB),
              data = batters_estimates,
              family = BB(mu.link = "identity"))

tidy.fit <- tidy(fit)

mu_0 <- tidy.fit$estimate[1]
mu_AB <- tidy.fit$estimate[2]
sigma <- exp(tidy.fit$estimate[3])

crossing(x = seq(0.08, .35, .001), AB = c(1, 10, 100, 1000, 10000)) %>%
  mutate(density = dbeta(x, (mu_0 + mu_AB * log(AB)) / sigma,
                         (1 - (mu_0 + mu_AB * log(AB))) / sigma)) %>%
  mutate(AB = factor(AB)) %>%
  ggplot(aes(x, density, color = AB, group = AB)) +
  geom_line(size = 1.5) +
  xlab("Batting average") +
  ylab("Prior density") + 
  theme_fivethirtyeight() + 
  theme(plot.title=element_text(family="Roboto")) +
  theme(axis.title = element_text(family = "Roboto", face = "bold", color="#666666", size = 12)) +
  theme(axis.text = element_text(family = "Roboto", face = "bold", color = "#535353", size = 11)) +
  theme(strip.text.x = element_text(family = "Roboto", face = "bold", size = 11)) +
  geom_hline(yintercept=0,size=1.2,colour="#535353") +
  scale_color_manual(values = c(blue, red, yellow, green , purple)) + 
  ggtitle("Estimates based on at-bats")
  

mu <- fitted(fit, parameter = "mu")
sigma <- fitted(fit, parameter = "sigma")

estimates_wAB <- batters_estimates %>%
  dplyr::select(name, Hits, AB, original = estimate) %>%
  mutate(mu = mu,
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + Hits,
         beta1 = beta0 + AB - Hits,
         new = alpha1 / (alpha1 + beta1))

ggplot(estimates_wAB, aes(original, new, color = AB)) +
  geom_point() +
  geom_abline(color = red, size = 1.4) +
  xlab("Original EB Estimate") +
  ylab("EB Estimate w/ AB term") +
  scale_color_continuous(trans = "log", breaks = 10 ^ (0:4)) +
  # theme_minimal()+
  theme_fivethirtyeight(base_family = "Roboto Condensed") +
  theme(plot.title=element_text(family="Roboto Condensed", face = "bold")) +
  theme(axis.title = element_text(family = "Roboto", face = "bold", color="#666666", size = 12)) +
  theme(axis.text = element_text(family = "Roboto", face = "bold", color = "#535353", size = 11)) +
  theme(strip.text.x = element_text(family = "Roboto", face = "bold", size = 11)) +
  # geom_hline(yintercept=0,size=1.2,colour="#535353") +
  ggtitle("Shrinkage of Estimates with AB") + 
  labs(subtitle = "Kanishka Misra", caption = "A Plot by Kanishka Misra")
  