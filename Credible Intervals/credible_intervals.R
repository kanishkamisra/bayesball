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

#Let's say I am working for the 1998 yankees team
yankee_players <- c("brosisc01", "jeterde01", "knoblch01", "martiti02", "posadjo01", "strawda01", "willibe02")

yankee_1998 <- batters_estimates %>%
  filter(playerID %in% yankee_players)

yankee_beta <- yankee_1998 %>%
  inflate(x = seq(0.18, 0.33, 0.0002)) %>%
  ungroup() %>%
  mutate(density = dbeta(x, alpha1, beta1))

ggplot(yankee_beta, aes(x, density, color = name)) +
  geom_line(size = 1.5) +
  stat_function(fun = function(x) dbeta(x, alpha, beta),
                lty = 2, color = "black", size = 1.5) + 
  theme_fivethirtyeight() + 
  ggtitle("Posterior distributions of 1998 Yankees Batters")

#Let's look at Derek Jetter

jeter <- yankee_beta %>% filter(name == "Derek Jeter")

jeter_pred <- jeter %>%
  mutate(cumulative = pbeta(x, alpha1, beta1)) %>%
  filter(cumulative > .025, cumulative < .975)

jeter_low <- qbeta(.025, jeter$alpha1[1], jeter$beta1[1])
jeter_high <- qbeta(.975, jeter$alpha1[1], jeter$beta1[1])

jeter %>%
  ggplot(aes(x, density)) +
  geom_line(size = 1.25) +
  geom_ribbon(aes(ymin = 0, ymax = density), data = jeter_pred,
              alpha = .25, fill = blue) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0),
                lty = 2, color = "black") +
  geom_errorbarh(aes(xmin = jeter_low, xmax = jeter_high, y = 0), height = 3.5, color = red, size = 1.5) +
  xlim(.18, .34)+
  theme_fivethirtyeight() + 
  ggtitle("Derek Jeter's Credible Interval")

# Pretty High!

yankee_1998 <- yankee_1998 %>%
  mutate(low  = qbeta(.025, alpha1, beta1),
         high = qbeta(.975, alpha1, beta1))

yankee_1998 %>% 
  mutate(name = reorder(name, avg)) %>%
  ggplot(aes(avg, name)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = low, xmax = high), size = 1) +
  geom_vline(xintercept = alpha / (alpha + beta), color = red, lty = 2, size = 1) +
  theme_fivethirtyeight() + 
  ggtitle("95% credible interval for batting averages of the 1998 yankees")

#Derek Jeter destroys balls I guess
