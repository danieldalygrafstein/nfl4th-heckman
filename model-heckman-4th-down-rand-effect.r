###########################################################
# Fit Heckman model to fourth downs
# Considered missing if a punt or a field goal
# Include coach random effects
# Dependence varies with covariates
###########################################################


#libraries 
library(tidyverse)
library(nfl4th)
library(rstan)
library(xml2)
library(rvest)

###########################################################
# read in play-by-play and coaching data

# data
seasons <- 2014:2021
pbp_list <- vector("list", length = length(seasons))

for(i in 1:length(seasons)){
  season <- seasons[i]
  print(season)
  pbp <- load_4th_pbp(season, fast = T) %>%
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
    ) %>% 
    select(-contains(c("yac", "drive", "fumble", "tackle")))
  pbp_list[[i]] <- pbp
}

pbp <- do.call("rbind", pbp_list)
rm(pbp_list)
gc()

coaches <- vector(mode = "list", length = length(seasons))

for(i in 1:length(seasons)){
  season = seasons[i]
  coaches_table <- xml2::read_html(paste0("https://www.pro-football-reference.com/years/",
                                          season,"/coaches.htm#coaches")) %>%
    rvest::html_nodes('table') %>%
    html_table() %>% .[[1]]
  names(coaches_table) <- coaches_table[1,]
  # remove top row, only take coach, team, and game cols
  coaches_table <- coaches_table[-1,c(1,2,3)]
  
  # select coach with most games if multiple coaches for a team
  coaches_table <- coaches_table %>%
    group_by(Tm) %>% 
    arrange(G) %>% slice(1) %>% 
    ungroup() %>% mutate(year = season)
  
  coaches[[i]] <- coaches_table
}

coaches <- do.call("rbind", coaches)

# change teams to match pbp data
coaches <- coaches %>% mutate(
  Tm = case_when(
    Tm == "GNB" ~ "GB",
    Tm == "KAN" ~ "KC",
    Tm %in% c("LAR", "STL") ~ "LA",
    Tm %in% c("LVR", "OAK") ~ "LV",
    Tm == "NWE" ~ "NE",
    Tm == "NOR" ~ "NO",
    Tm == "SDG" ~ "LAC",
    Tm == "SFO" ~ "SF",
    Tm == "TAM" ~ "TB",
    TRUE ~ Tm
  )
)


#######################################################

# create model vars
# set prob of going for it to 100 
# when less than 2min left and score_diff between 4 and 8, or between 12 and 16
# or less than 2min left score diff between 1 and 16, in own half

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
         year = as.factor(sapply(strsplit(pbp$game_id, "_"), `[`, 1)),
         go_for_it = as.numeric((pass == 1 | rush == 1) &
                                  field_goal_attempt == 0 &
                                  punt_attempt == 0),
         second_half = as.numeric(game_half == "Half2"),
         go_100 = as.numeric((game_seconds_remaining < 120 &
                                (between(score_differential, -8, -4) |
                                   between(score_differential, -16, -12))) |
                               game_seconds_remaining < 120 & yardline_100 >= 50 &
                               between(score_differential, -16, -1))) %>% 
  mutate(outdoors = replace_na(outdoors, 0),
         go_100 = replace_na(go_100, 0))

pbp <- pbp %>% left_join(coaches %>% 
                    mutate(year = as.factor(year)) %>% 
                    select(-G),
                  by = c("posteam" = "Tm", "year" = "year")) %>% 
  mutate(coach = as.factor(Coach)) %>% 
  select(-Coach) %>% 
  mutate(coach_num = as.numeric(coach))


model_vars <- pbp %>% 
  select(go_for_it, first_down, ydstogo, ydstogo_square, yardline_100, yardline_100_square,
         ydstogo_yardline_int, outdoors, posteam_spread, total_line, posteam_total,
         pos_home_team, year, score_differential, half_seconds_remaining, second_half,
         posteam_timeouts_remaining, go_100, coach_num)


# add year dummy variables (take 2014 as reference)
model_vars <- cbind(model_vars, model.matrix(~year, data = model_vars)[,-1])

model_vars <- model_vars %>%
  mutate_at(c("ydstogo", "ydstogo_square", "yardline_100",
              "yardline_100_square",
              "ydstogo_yardline_int", "posteam_spread", "total_line",
              "score_differential", "half_seconds_remaining"), ~(scale(.) %>% as.vector))

X <- model_vars %>% select(ydstogo, yardline_100, ydstogo_square, yardline_100_square,
                           ydstogo_yardline_int, posteam_spread, total_line,
                           year2015, year2016, year2017, year2018, year2019,
                           year2020, year2021)

Z <- model_vars %>% select(score_differential, half_seconds_remaining, second_half,
                           posteam_timeouts_remaining)
N <- nrow(model_vars)
N_y <- sum(model_vars$go_for_it)
R <- model_vars$go_for_it
Y <- model_vars %>% filter(go_for_it == 1) %>% pull(first_down)

G <- as.numeric(model_vars$go_100 == 1 & model_vars$go_for_it == 1)

C <- model_vars %>% filter(go_for_it == 1 & go_100 == 0) %>% 
  select(ydstogo, yardline_100, ydstogo_yardline_int,
         score_differential, half_seconds_remaining,
         posteam_timeouts_remaining, second_half, total_line)

num_coaches = max(model_vars$coach_num)
coach = model_vars$coach_num


stan_params <- list(N = N, K = ncol(X), M = ncol(Z), N_y = N_y,
                    X = X, Z = Z,
                    R = R, Y = Y, G = G,
                    num_cor_vars = ncol(C), n_rho = nrow(C) , C = C,
                    num_coaches = num_coaches, coach = coach)

stan_file <- "fourth-down-success-heckman-rho-vary-rand-effect.stan"

heckman_bayes <- stan(
  file = stan_file,
  data = stan_params,
  iter = 4000,
  chains = 1,
  control = list(max_treedepth = 10),
  init = 0
)

heckman_bayes_params <- rstan::extract(heckman_bayes)[c("beta_r_0", "beta_y_0",
                                    "beta_y_x", "beta_r_x", "beta_r_z", 
                                    "rho", "beta_c", "beta_l")]


fourth_down_probs_heckman <- rep(NA, nrow(model_vars))
rho_heckman <- rep(NA, nrow(model_vars))
C_pred <- model_vars %>% 
  select(ydstogo, yardline_100,
         ydstogo_yardline_int,
         score_differential, half_seconds_remaining,
         posteam_timeouts_remaining, second_half, total_line) 

for(i in 1:nrow(model_vars)){
  
  fourth_down_probs_heckman[i] <- mean(pnorm((heckman_bayes_params$beta_y_0 + 
                                as.vector(as.matrix(X[i,])%*%
                                            t(as.matrix(heckman_bayes_params$beta_y_x))))))
  
  rho_heckman[i] <- mean(tanh(as.matrix(C_pred[i,])%*%
                           t(as.matrix(heckman_bayes_params$beta_c))))
}

pbp %>% select(game_id, old_game_id, play_id, down, first_down, ydstogo, yardline_100,
               home_team, posteam, spread_line, total_line,
               score_differential, game_seconds_remaining) %>% 
  mutate(heckman_prob_rand_effect = fourth_down_probs_heckman,
         rho = rho_heckman)%>% 
  write_csv("data/fourth-down-heckman-probs-vary-rho-rand-effects.csv")

saveRDS(heckman_bayes_params, file="data/heckman-params-rho-vary-rand-effects.RData")



  

