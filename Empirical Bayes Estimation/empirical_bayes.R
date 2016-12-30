library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)
library(ggthemes)

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
  arrange(avg) %>%
  head(5)

batters %>%
  filter(AB >= 500) %>%
  ggplot(aes(avg)) +
  geom_histogram(binwidth = .005, fill = yellow) + 
  theme_fivethirtyeight() + 
  ggtitle("Distribution of Batters' Batting Averages")

