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
  ggtitle("At-bats vs log averages and estimates") +
  # geom_hline(yintercept=0,size=1.2,colour="#535353") +
  labs(subtitle = "Kanishka Misra")

# Low AB value players are getting grossly overestimated by the shrinkage model
# There is a need to make a model that considers AB

# By re-parameterizing the true probability(posterior) distribution, we can express it in terms of
# mean of the batting average and how spread out the distribution is
# p ~ Beta(mu/sigma, (1-mu)/sigma)

# To include AB, we can define mu as 
# I give up


