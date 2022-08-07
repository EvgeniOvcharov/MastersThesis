# Loading data and libraries ---- 
setwd("C:/Users/plame/OneDrive/Desktop/GradProject")
data <- read.csv("FMEL_Dataset.csv")
summary(data)
library("dplyr")
library("tidyr")
library("ggplot2")
library("reshape2")
library("nloptr")
library("data.table")

# Reshaping the data ----
melt_results <- function(results_df) {
  results_df %>%
    # select only relevant columns
    select(localTeam, visitorTeam, localGoals, visitorGoals, date) %>%
    gather(location, team,  -localGoals, -visitorGoals, -date) %>%
    # calculate goals for/against the team
    mutate(g_for = case_when(
      location == "localTeam" ~ localGoals,
      location == "visitorTeam" ~ visitorGoals
    )) %>%
    mutate(g_ag = case_when(
      location == "localTeam" ~ visitorGoals,
      location == "visitorTeam" ~ localGoals
    )) 
}

data_melted <- data %>%
  melt_results()

# Visualising the data - home vs visiting team
data_melted %>%
  ggplot(., aes(x = g_for, fill = location)) +
  geom_density(adjust = 8, alpha = 0.5) +
  scale_fill_manual(values = c("green", "red")) +
  scale_x_continuous(breaks = 0:6) +
  labs(title = "Goals scored at home and away",
       x = "goals scored",
       y = "density") +
  theme_minimal()


# Game statistics
game_statistics <- data_melted %>%
  mutate(win = ifelse(g_for > g_ag, 1,0),
         loss = ifelse(g_for < g_ag, 1,0),
         draw = ifelse(g_for == g_ag, 1,0))

game_statistics <- game_statistics %>%
  group_by(team) %>%
  summarise ( W = sum(win),
           L = sum (loss),
           D = sum(draw),
           GF = sum(g_for),
           GA = sum(g_ag),
           GD = GF - GA,
           Pts = 3*W + 1*D
           )
  

# # Model with 4 parameters ----
# S_x = sum(data$localGoals)
# S_y = sum(data$visitorGoals)
# 
#   melt(data_melted,id.vars=1:2) %>%
#   dcast(team ~ location + variable) %>%
#   mutate(alfa = localTeam_sum_for/sqrt(S_x),
#          beta = visitorTeam_sum_against/sqrt(S_x),
#          gama = localTeam_sum_against/sqrt(S_y),
#          delta = visitorTeam_sum_for/sqrt(S_y))
# 
# data_combined <- left_join(data, data_model1[c("team", "alfa", "gama")], by = c("localTeam" = "team"))
# data_combined <- left_join(data_combined, data_model1[c("team", "beta","delta")], by = c("visitorTeam" = "team"))
# data_combined <- data_combined %>%
#   mutate(par_local = alfa*beta,
#          par_visit = gama*delta)


# Model with home parameter (3 parameters) ----

# Estimating gama = home advantage
gama = sum(data$localGoals)/sum(data$visitorGoals)

# Estimating alfa and beta  - average number of goals for and against each team
data_model2 <- data_melted %>%
  group_by(team) %>%
  summarise(alfa = mean(g_for),
            beta = mean(g_ag))

# For each game add columns with the estimated parameters both for the local and the visiting teams
data_combined2 <- left_join(data, data_model2[c("team", "alfa", "beta")], by = c("localTeam" = "team"))
data_combined2 <- data_combined2 %>%
  rename(alfa_local=alfa) %>%
  rename(beta_local=beta)

data_combined2 <- left_join(data_combined2, data_model2[c("team", "alfa", "beta")], by = c("visitorTeam" = "team"))
data_combined2 <- data_combined2 %>%
  rename(alfa_visitor=alfa) %>%
  rename(beta_visitor=beta)

# Predict the game scores for both the local and the visting teams
# Local team estimate is multiplied by gamma - "home advantage" parameter
data_combined2 <- data_combined2 %>% 
  mutate(prediction_local = alfa_local*beta_visitor*gama,
         prediction_visit = alfa_visitor*beta_local)

# Estimate the probability of the observed result assuming the model is correct
data_combined2 <- data_combined2 %>%
  mutate(lik_lgoal = dpois(localGoals,prediction_local),
         lik_vgoal = dpois(visitorGoals, prediction_visit))


game_predicted_statistics <- data_combined2 %>%
  # select only relevant columns
  select(localTeam, visitorTeam, prediction_local, prediction_visit, date) %>%
  gather(location, team,  -prediction_local,-prediction_visit, -date) %>%
  # calculate goals for/against the team
  mutate(g_for = case_when(
    location == "localTeam" ~ prediction_local,
    location == "visitorTeam" ~ prediction_visit
  )) %>%
  mutate(g_ag = case_when(
    location == "localTeam" ~ prediction_visit,
    location == "visitorTeam" ~ prediction_local
  )) %>%
  mutate(win = ifelse(g_for > g_ag, 1,0),
         loss = ifelse(g_for < g_ag, 1,0),
         draw = ifelse(g_for == g_ag, 1,0)) %>%
  group_by(team) %>%
  summarise ( W = sum(win),
              L = sum (loss),
              D = sum(draw),
              GF = sum(g_for),
              GA = sum(g_ag),
              GD = GF - GA,
              Pts = 3*W + 1*D
  )
  

# Dixon - Coles optimization ----
x = data[,c(5:8)]
n = n_distinct(x$localTeam)
teams <- matrix(c(unique(x$localTeam),1:n), nrow = n)
teams <- as.data.frame(teams)
x <- left_join(x,teams, by = c("localTeam" = "V1"))
x <- left_join(x,teams, by = c("visitorTeam" = "V1"))

x <- x %>%
  rename(local_team_index = V2.x,
         visitor_team_index = V2.y)
x <-x [,c(3:6)]

x <- as.data.table(x)

x$local_team_index <- as.integer(x$local_team_index)
x$visitor_team_index <- as.integer(x$visitor_team_index)

x <- as.data.table(x)

f_lambda = function(alfa, beta, gama){
  gama*alfa[x$local_team_index]*beta[x$visitor_team_index] 
}

f_mu = function(alfa, beta, gama){
  alfa[x$local_team_index]*beta[x$visitor_team_index]
}

f_l = function(w) {
  alfa = w[1:n] 
  beta = w[(n+1):(2*n)]
  gama = w[2*n+1]
 
  lambda = f_lambda(alfa, beta, gama)
  mu =     f_mu(alfa, beta, gama)
  
  - sum(x$localGoals * log(lambda) + x$visitorGoals * log(mu) - lambda - mu) 
}

eq = function(w) {
  alfa = w[1:n]
  beta = w[(n+1):(2*n)]
  c(sum(alfa), sum(beta))
}


w0 = rep(1, 2*n+1)
S <- slsqp(w0, fn = f_l,     # no gradients and jacobians provided
           heq = eq,
           control = list(xtol_rel = 1e-9, print_level = 0))
