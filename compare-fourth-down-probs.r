###########################################################
# Compare fourth down probabilities between:
#   - naive model just when going for it
#   - heckman model
#   - model with 3rd and 4th downs from nfl4th package
###########################################################

source("gg_field.r")

library(dplyr)
library(nfl4th)

seasons <- 2014:2021

fourth_down_list <- vector(mode = "list", length = length(seasons))

for(i in 1:length(seasons)){
  season <- seasons[i]
  fourth_down_probs <- load_4th_pbp(season) %>% 
    filter(
    down == 4,
    qb_kneel == 0,
    rush == 1 | pass == 1 | punt_attempt == 1 | field_goal_attempt == 1,
    !is.na(posteam),
    !is.na(yardline_100),
    !is.na(score_differential),
    !is.na(first_down),
    game_half %in% c("Half1", "Half2"),
    play_type != "no_play",
    week <= 17,
    !is.na(first_down_prob)
  ) %>%
    select(play_id, game_id, home_team, away_team, season_type, week,
                               posteam, posteam_type, ydstogo, yardline_100, first_down_prob,
           score_differential,
           half_seconds_remaining, game_half)
  fourth_down_list[[i]] <- fourth_down_probs
}

fourth_downs <- as_tibble(do.call(rbind, fourth_down_list))
gc()

# read in fourth down data

fourth_downs <- fourth_downs %>% left_join(read.csv("data/fourth-down-naive-probs.csv") %>% 
                                             select(game_id, play_id, bayes_naive_prob),
                                           by = c("game_id" = "game_id", "play_id" = "play_id"))


fourth_downs <- fourth_downs %>% left_join(read.csv("data/fourth-down-heckman-probs-vary-rho-rand-effects.csv") %>% 
                                             select(game_id, play_id, heckman_prob_rand_effect),
                                           by = c("game_id" = "game_id", "play_id" = "play_id"))


# Overall differences
summary(fourth_downs$bayes_naive_prob - fourth_downs$heckman_prob_rand_effect)

# Plot comparing the probabilities heckman rand vs naive

fourth_down_probs_transform <- fourth_downs %>% 
  filter(ydstogo <= 10) %>% 
  mutate(ydstogo_transform = ydstogo*5.33 - 5.33/2,
         yardline_100_transform = (100 - yardline_100) + 10)

plot_data <- fourth_down_probs_transform %>% 
  select(play_id, game_id, yardline_100, ydstogo, yardline_100_transform, ydstogo_transform, 
                                                    first_down_prob,
                                                    bayes_naive_prob,
                                                    heckman_prob_rand_effect) %>% 
  filter(!is.na(heckman_prob_rand_effect) & !is.na(bayes_naive_prob)) %>% 
  group_by(yardline_100, ydstogo, ydstogo_transform, yardline_100_transform) %>% 
  summarise(bayes_naive_prob = mean(bayes_naive_prob),
            heckman_prob_rand_effect = mean(heckman_prob_rand_effect),
            first_down_prob = mean(first_down_prob))


red_white <-colorRampPalette(colors = c("red", "white"))
white_green <- colorRampPalette(colors = c("white", "blue"))

colors <- c(red_white(10)[c(1,2,4,6,8)], white_green(10)[c(1,4,6,7,8)])

ggplot(plot_data) + 
  geom_tile(aes(x = yardline_100_transform, y = ydstogo_transform, 
                fill = bayes_naive_prob - heckman_prob_rand_effect), alpha = 1) +
  scale_fill_gradientn(colors = colors,
                       breaks = c(-0.08, -0.04, 0, 0.04, 0.08)) +
  gg_field(field_color = "white", line_color = "black",
           sideline_color = "gray", "endzone_color" = "white") +
  labs(y = "Yards to Gain") +
  scale_y_continuous(breaks = seq(5.33/2,53.33 - 5.33/2, 5.33), labels = 1:10) +
  theme(text = element_text(size = 10), legend.position = "bottom", 
        legend.key.size = unit(0.4, 'cm')) +
  guides(fill = guide_legend(nrow = 1, title = "First Down Probability Bias")) +
  annotate(geom = "text", x = 5, y = 28, label = "Possession Team Endzone", angle = 90) +
  annotate(geom = "text", x = 115, y = 28, label = "Opponent Team Endzone", angle = 270)



# Plot comparing the probabilities nfl4th probs vs naive probs

ggplot(plot_data) + 
  geom_tile(aes(x = yardline_100_transform, y = ydstogo_transform, 
                fill = first_down_prob - bayes_naive_prob), alpha = 1) +
  scale_fill_gradientn(colors = colors,
                       breaks = c(-0.08, -0.04, 0, 0.04, 0.08)) +
  gg_field(field_color = "white", line_color = "black",
           sideline_color = "gray", "endzone_color" = "white") +
  labs(y = "Yards to Gain") +
  scale_y_continuous(breaks = seq(5.33/2,53.33 - 5.33/2, 5.33), labels = 1:10) +
  theme(text = element_text(size = 10), legend.position = "bottom", 
        legend.key.size = unit(0.4, 'cm')) +
  guides(fill = guide_legend(nrow = 1, title = "First Down Prob Bias"))


# Plot comparing the probabilities nfl4th probs vs heckman random probs

ggplot(plot_data) + 
  geom_tile(aes(x = yardline_100_transform, y = ydstogo_transform, 
                fill = first_down_prob - heckman_prob_rand_effect), alpha = 1) +
  scale_fill_gradientn(colors = colors,
                       breaks = c(-0.08, -0.04, 0, 0.04, 0.08)) +
  gg_field(field_color = "white", line_color = "black",
           sideline_color = "gray", "endzone_color" = "white") +
  labs(y = "Yards to Gain") +
  scale_y_continuous(breaks = seq(5.33/2,53.33 - 5.33/2, 5.33), labels = 1:10) +
  theme(text = element_text(size = 10), legend.position = "bottom", 
        legend.key.size = unit(0.4, 'cm')) +
  guides(fill = guide_legend(nrow = 1, title = "First Down Prob"))



