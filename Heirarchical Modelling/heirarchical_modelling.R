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