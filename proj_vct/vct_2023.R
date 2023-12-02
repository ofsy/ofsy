## Project VCT 2023
getwd()
setwd("/Users/OFSy/Project/Side Project/VCT2023")

## Import File
library(tidyverse)
vct <- read_csv("vct_2023.csv")
vct <- drop_na(vct)
vct <- vct[-c(1251:1260), ] ## Remove team BNY VS SPB cause it's show match

## Data from 25 Mar 2023 - 28 Aug 2023

## the most agent picked in VCT
agent_p <- vct %>%
  group_by(agent) %>%
  summarise(count_agent = n()) %>%
  arrange(desc(count_agent)) %>%
  mutate(percent = round((count_agent / (length(unique(vct$game_id))*2))*100, 2))
head(agent_p)
##### the most agent picked in VCT In each map
agent_m <- vct %>%
  group_by(agent, map) %>%
  summarise(count_agent = n()) %>%
  arrange(desc(count_agent)) %>%
  mutate(percent = round((count_agent / (length(unique(vct$game_id)) * 2)) * 100, 2)) %>%
  pivot_wider(names_from = map, values_from = c(percent), names_prefix = "map_") 
# Melt the data frame for easier plotting
data_long <- agent_m %>%
  pivot_longer(cols = starts_with("map"), names_to = "map_variable", values_to = "value")
# Select the top 5 agents based on percentage for each map
top5_agents <- data_long %>%
  group_by(map_variable) %>%
  top_n(5, wt = value) %>%
  arrange(map_variable, desc(value)) %>%
  mutate(rank = row_number())

# Assuming top5_agents is your data frame
ggplot(top5_agents, aes(x = map_variable, y = value, label = agent)) +
  geom_point(color = "skyblue", size = 1.25) +
  geom_text(vjust = -0.5, size = 5, color = "darkred") +
  labs(title = "Top 5 Agents by Map",
       subtitle = "Based on percentage",
       x = "Map",
       y = "Percent") +
  theme_minimal() + 
  ## make the graph more beautiful
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "none"  # Remove legend if not needed
  )

## headshot percentage correlation the dead
str(vct$`hs%`)
# Remove the percentage sign from the 'hs%' column
vct$hsv2 <- (as.numeric(gsub("%", "", vct$`hs%`)))/100
mean(vct$hsv2, na.rm = T)

vct$kastv2 <- (as.numeric(gsub("%", "", vct$`kast%`)))/100
mean(vct$kastv2, na.rm = T)
cor.test(vct$death, vct$kastv2)

#Kill/Death ratio
vct$kds <- round(vct$kill / vct$death,2)

summary(vct)

####kds per team
kds_team <- vct %>%
  group_by(team) %>%
  summarise(Avg_kds = round(mean(kds),2)) %>%
  arrange(desc(Avg_kds))

kds_team$team <- factor(kds_team$team, levels = unique(kds_team$team[order(kds_team$Avg_kds)]))

library(ggplot2)

# Assuming kds_team is your data frame
ggplot(kds_team, aes(x = Avg_kds, y = team, fill = team)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Avg_kds), vjust = 0, size = 2, color = "black") +  # Add data values
  geom_vline(xintercept = mean(kds_team$Avg_kds), color = "red", linetype = "dotted", size = 0.75) +  # Add mean line
  labs(title = "Average Kill/Death in Each Teams", x = "Average Kill/Death", y = "Team") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_blank())

##kds per player
kds_player <- vct %>%
  group_by(team, player) %>%
  summarise(Avg_kds_mean = round(mean(kds), 2),
            Avg_kds_var = round(sd(kds), 2)) %>%
  arrange(desc(Avg_kds_mean))

ggplot(kds_player, aes(x = Avg_kds_mean, y = Avg_kds_var, label = player)) +
  geom_point() +
  geom_text(aes(color = ifelse(player == c("aspas","AAAAY","kamyk"), "darkred", "salmon")),
            vjust = -0.5, hjust = 0.5, size = 7)  +  # Adjust label position and size
  labs(title = "Scatter Plot of Average vs. Standard Deviation",
       x = "Average KDs",
       y = "SD KDs") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
theme(
  plot.title = element_text(size = 16, face = "bold"),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 10, color = "black"),
  legend.position = "none"  # Remove legend if not needed
)


kds_tp <- left_join(kds_player,kds_team, by = "team" )
 ##### The Player that stand out of the team

sto_t <- kds_tp %>%
  group_by(player, team) %>%
  summarise(sto = Avg_kds_mean - Avg_kds) %>%
  left_join(kds_tp, by = "player") %>%
  arrange(desc(sto)) %>%
  head(5)
sto_t$team.x <- factor(sto_t$team.x, levels = unique(sto_t$team.x[order(sto_t$sto)]))
ggplot(sto_t, aes(x = Avg_kds, y = team.x)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue", position = "dodge") +
  geom_point(aes(x = Avg_kds_mean), size = 2) +
  geom_text(aes(x = Avg_kds_mean, label = player), position = position_jitter(width = 0.01),
            vjust = 0.3, hjust = 0, size = 5) +
  labs(title = "Top 5 Player that Average K/D stand out the team",
       x = "Average KDS",
       y = "Team") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_blank())




team_p <- c("FNC", "EG", "LOUD", "PRX")
# Assuming kds_tp is your data frame
kds_tp <- kds_tp %>%
  arrange(desc(Avg_kds)) %>%
  filter(team %in% team_p)

kds_tp$team <- factor(kds_tp$team, levels = unique(kds_tp$team[order(kds_tp$Avg_kds)]))

ggplot(kds_tp, aes(x = Avg_kds, y = team)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue", position = "dodge") +
  geom_point(aes(x = Avg_kds_mean), size = 2) +
  geom_text(aes(x = Avg_kds_mean, label = player), position = position_jitter(width = 0.01),
            vjust = 0.3, hjust = 0, size = 5)+
  geom_vline(xintercept = mean(kds_team$Avg_kds), color = "red", linetype = "dotted", size = 0.75) +  # Add mean line
  labs(title = "Top 5 Teams - Average KDS with Individual Player Points",
       x = "Average KDS",
       y = "Team") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_blank())

###########################################################################################
## count team won the maps
filt_match <- vct %>%
  select(match_id, game_id, team, opponent, win_lose,map, map_pick) %>%
  filter(win_lose == "team win") %>%
  unique()

count_w <- filt_match %>% 
  group_by(team) %>%
  summarise(count_win = n()) %>%
  arrange(desc(count_win))

count_l <- filt_match %>% 
  group_by(opponent) %>%
  summarise(count_lose = n()) %>%
  arrange(desc(count_lose))

result <- left_join(count_w, count_l, by = c("team" = "opponent") )
result$percent_win <- (round(result$count_win / (result$count_win + result$count_lose),3))
###########################################################################################

fnc <- filt_match %>%
  filter(team == "FNC" | opponent == "FNC") %>%
  group_by(match_id) %>%
  summarise(g_won = sum(ifelse(team == "FNC", 1, 0)) - sum(ifelse(opponent == "FNC", 1, 0))) %>%
  filter(g_won < 0)

fnc_l <- select()


fnc <- filt_match %>%
  filter(team == "FNC" | opponent == "FNC") %>%
  group_by(match_id) %>%
  mutate(game_outcome = ifelse(team == "FNC", 1, 0)) %>%
  summarise(match_outcome = sum(game_outcome)) #%>%
  ungroup() %>%
  select(match_id, match_outcome)

  