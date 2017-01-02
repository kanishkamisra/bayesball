library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)
library(ggthemes)
library(broom)

options(digits = 3)

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

batters_estimates <- batters_filtered %>%
  mutate(estimate = (alpha + Hits)/(alpha + beta + AB), alpha1 = (Hits + alpha), beta1 = (AB - Hits + beta)) %>%
  arrange(desc(estimate))

haaron <- batters_estimates %>% filter(name == "Hank Aaron")
mpiazza <- batters_estimates %>% filter(name == "Mike Piazza")
two_players <- bind_rows(haaron, mpiazza)


# Comparing posterior distributions, or the plausible values of averages when randomly picked.
two_players %>%
  inflate(x = seq(0.28, 0.33, .00025)) %>%
  mutate(density = dbeta(x, alpha1, beta1)) %>%
  ggplot(aes(x, density, color = name)) +
  geom_line(size = 1.2) +
  theme_fivethirtyeight() + 
  ggtitle("Posterior Distributions (Aaron vs Piazza")

# there is quite a bit of overlapping between the two and in some cases, it is possible that aaron's
# true average is higher than that of piazza's

# Simulation of posterior beta distributions of both the players.
piazza_simulation <- rbeta(1e6, mpiazza$alpha1, mpiazza$beta1)
aaron_simulation <- rbeta(1e6, haaron$alpha1, haaron$beta1)

sim <- mean(piazza_simulation > aaron_simulation)
sim

# There is 60.6% probability that piazza is better than hank aaron..

