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

pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

batters <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(Hits = sum(H), AB = sum(AB)) %>%
  mutate(avg = Hits/AB)

batters <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast, bats) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(batters, by = "playerID")

library(gamlss)

fit <- gamlss(cbind(Hits, AB - Hits) ~ log(AB),
              data = dplyr::select(batters, -bats),
              family = BB(mu.link = "identity"))

batters_estimates <- batters%>%
  mutate(mu = fitted(fit, "mu"),
         sigma = fitted(fit, "sigma"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + Hits,
         beta1 = beta0 + AB - Hits,
         estimate = alpha1 / (alpha1 + beta1))


batters2 <- batters %>%
  filter(!is.na(bats)) %>%
  mutate(bats = relevel(bats, "R"))

fit2 <- gamlss(cbind(Hits, AB - Hits) ~ log(AB) + bats,
               data = batters2,
               family = BB(mu.link = "identity"))

tidy(fit2)

sigma <- fitted(fit2, "sigma")[1]

crossing(bats = c("L", "R"),
         AB = c(1, 10, 100, 1000, 10000)) %>%
  augment(fit2, newdata = .) %>%
  rename(mu = .fitted) %>%
  crossing(x = seq(.1, .36, .0005)) %>%
  mutate(alpha = mu / sigma,
         beta = (1 - mu) / sigma,
         density = dbeta(x, alpha, beta)) %>%
  ggplot(aes(x, density, color = factor(AB), lty = bats)) +
  geom_line(size = 1.4) +
  theme_fivethirtyeight() + 
  theme(plot.title=element_text(family="Roboto")) +
  theme(axis.title = element_text(family = "Roboto", face = "bold", color="#666666", size = 12)) +
  theme(axis.text = element_text(family = "Roboto", face = "bold", color = "#535353", size = 11)) +
  theme(strip.text.x = element_text(family = "Roboto", face = "bold", size = 11)) +
  geom_hline(yintercept=0,size=1.2,colour="#535353") +
  scale_color_manual(values = c(blue, red, yellow, green , purple)) + 
  ggtitle("New Estimates based on at-bats")+
  labs(x = "Batting average",
       y = "Prior density",
       color = "AB",
       lty = "Batting hand")
