###########################################################
# Fit model to get probability of fourth-down success
# No adjustment for missing data (i.e. naive model)
###########################################################

#libraries 
library(tidyverse)
library(nfl4th)
library(rstanarm)
library(rstan)

###########################################################
# read in play-by-play data

# data
seasons <- 2014:2021
pbp_list <- vector("list", length = length(seasons))

for(i in 1:length(seasons)){
  season <- seasons[i]
  print(season)
  pbp <- nflfastR::load_pbp(season) %>%
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
      week <= 17
    )
  pbp_list[[i]] <- pbp
}

pbp <- do.call("rbind", pbp_list)
rm(pbp_list)
gc()

# create model variables

pbp <- pbp %>% 
  mutate(home_total = (spread_line + total_line) / 2,
         away_total = (total_line - spread_line) / 2,
         posteam_total = if_else(posteam == home_team, home_total, away_total),
         posteam_spread = dplyr::if_else(posteam == home_team, spread_line, -1 * spread_line),
         pos_home_team = if_else(posteam == home_team, 1, 0),
         outdoors = if_else(roof == "outdoors", 1, 0),
         ydstogo_square = ydstogo^2,
         yardline_100_square = yardline_100^2,
         ydstogo_yardline_int = ydstogo*yardline_100,
         year = as.factor(sapply(strsplit(pbp$game_id, "_"), `[`, 1))) %>% 
  mutate(outdoors = replace_na(outdoors, 0)) %>% 
  #standardize continuous covariates to mean 0 var 1
  mutate_at(c("ydstogo", "ydstogo_square", "yardline_100",
              "yardline_100_square",
              "ydstogo_yardline_int", "posteam_spread", "total_line"), ~(scale(.) %>% as.vector))

# add year dummy variables (take 2014 as reference)
pbp <- cbind(pbp, model.matrix(~year, data = pbp)[,-1])

model_vars <- pbp %>%
  select(first_down, ydstogo, ydstogo_square, yardline_100, yardline_100_square,
         ydstogo_yardline_int, outdoors, posteam_spread, total_line, posteam_total,
         pos_home_team, year2015, year2016, year2017, year2018, year2019,
         year2020, year2021, pass, rush) %>% 
  filter(pass ==1 | rush == 1)
  

stan_covariates = model_vars %>%
  select(ydstogo, yardline_100, ydstogo_square, yardline_100_square,
         ydstogo_yardline_int, posteam_spread, total_line,
         year2015, year2016, year2017, year2018, year2019,
         year2020, year2021)
stan_outcome = model_vars %>% pull(first_down)

stan_params <- list(N = nrow(model_vars), K = ncol(stan_covariates),
                    X = stan_covariates,
                    y = stan_outcome,
                    beta_mean = rep(0, ncol(stan_covariates)),
                    beta_scale = rep(0.5, ncol(stan_covariates)))

stan_file <- "fourth-down-success-probit-naive.stan"

fourth_down_naive <- stan(
  file = stan_file,
  data = stan_params,
  iter = 2000,
  chains = 1,
  control = list(max_treedepth = 10))

probit_params <- rstan::extract(fourth_down_naive)[c("beta_0", "beta")]

fourth_down_success_go_for_it <- colMeans(rstan::extract(fourth_down_naive)[c("mu")]$mu)


fourth_down_probs_naive <- rep(NA, nrow(pbp))

X <- pbp %>% select(ydstogo, yardline_100, ydstogo_square, yardline_100_square,
                      ydstogo_yardline_int, posteam_spread, total_line,
                      year2015, year2016, year2017, year2018, year2019,
                      year2020, year2021)

for(i in 1:nrow(pbp)){
  
  fourth_down_probs_naive[i] <- mean(pnorm((probit_params$beta_0 + 
                                                as.vector(as.matrix(X[i,])%*%
                                        t(as.matrix(probit_params$beta))))))
}

pbp %>% select(game_id, old_game_id, play_id, down, first_down, ydstogo, yardline_100,
               home_team, posteam, spread_line, total_line) %>% 
  mutate(bayes_naive_prob = fourth_down_probs_naive) %>% 
  write_csv("data/fourth-down-naive-probs.csv")

