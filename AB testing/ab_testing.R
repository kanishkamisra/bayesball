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
prior_mu = alpha/(alpha + beta)

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

# Density Cloud
x <- seq(.29, .318, .0002)
crossing(piazza_x = x, aaron_x = x) %>%
  mutate(piazza_density = dbeta(piazza_x, mpiazza$alpha1, mpiazza$beta1),
         aaron_density = dbeta(aaron_x, haaron$alpha1, haaron$beta1),
         joint = piazza_density * aaron_density) %>%
  ggplot(aes(piazza_x, aaron_x, fill = joint)) +
  geom_tile() +
  geom_abline() +
  scale_fill_gradient2(low = "white", high = red) +
  labs(x = "Piazza batting average",
       y = "Aaron batting average",
       fill = "Joint density") +
  theme_fivethirtyeight()


# Closed form approximation (Normal approximation of the beta function)

two_players %>%
  mutate(mu = alpha1 / (alpha1 + beta1),
         var = alpha1 * beta1 / ((alpha1 + beta1) ^ 2 * (alpha1 + beta1 + 1))) %>%
  inflate(x = seq(.28, .33, .00025)) %>%
  mutate(density = dbeta(x, alpha1, beta1),
         normal = dnorm(x, mu, sqrt(var))) %>%
  ggplot(aes(x, density, group = name)) +
  geom_line(aes(color = name), size = 1.75) +
  geom_line(lty = 2, size = 1.5) + 
  theme_fivethirtyeight()

# Virtually indistinguishable lines!

# Probability that one normal is greater than the other:
h_approx <- function(alpha_a, beta_a,
                     alpha_b, beta_b) {
  u1 <- alpha_a / (alpha_a + beta_a)
  u2 <- alpha_b / (alpha_b + beta_b)
  var1 <- alpha_a * beta_a / ((alpha_a + beta_a) ^ 2 * (alpha_a + beta_a + 1))
  var2 <- alpha_b * beta_b / ((alpha_b + beta_b) ^ 2 * (alpha_b + beta_b + 1))
  pnorm(0, u2 - u1, sqrt(var1 + var2))
}

h_approx(mpiazza$alpha1, mpiazza$beta1, haaron$alpha1, haaron$beta1)
# Again, 60.6% in favor of Mike Piazza!




