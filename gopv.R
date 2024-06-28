library(dplyr)
library(tidyverse)
library(caret)
library(glmnet)
library(fitdistrplus)
library(withr)
library(purrr)
library(hockeyR)
library(stringi)
library(gt)
library(gtExtras)

shots <- read_csv("shots_2023.csv") 

goalie_shots <- shots %>% filter(!is.na(goalieNameForShot)) %>% mutate(gsax = xGoal - goal, goalie = as.factor(goalieIdForShot), teamCode = as.factor(teamCode)) %>% select(gsax, goalie, shooting_team = teamCode)

dummy <- dummyVars(" ~ .", data = goalie_shots)
goalie_shots_dummy <- data.frame(predict(dummy, newdata = goalie_shots))

preds <- as.matrix(goalie_shots_dummy[,-1])
target <- goalie_shots_dummy[,1]

set.seed(123)

cv_ridge <- cv.glmnet(preds, target, alpha = 0, intercept = TRUE)
lambda <- cv_ridge$lambda.min # was not a good value so just decided to use total linear regression
ridge_model <- glmnet(preds, target, alpha = 0, lambda = 0, intercept = TRUE)

ridge_coefs <- data.frame(as.matrix(coef(ridge_model)))
ridge_coefs$name <- rownames(ridge_coefs)
ridge_coefs <- ridge_coefs %>% rename(coef = s0)
ridge_intercept <- ridge_coefs$coef[which(ridge_coefs$name == "(Intercept)")]
ridge_coefs <- ridge_coefs %>% filter(name != "(Intercept)") %>% mutate(coef = coef + ridge_intercept)

names_ids <- shots %>%
  filter(!is.na(goalieNameForShot)) %>%
  distinct(goalieIdForShot, goalieNameForShot)

goalies <- ridge_coefs %>%
  filter(grepl("goalie", name)) %>%
  mutate(id = as.numeric(sub("goalie.", "", name))) %>%
  left_join(names_ids, by = c("id"="goalieIdForShot")) %>%
  rename(adj_gsax_shot = coef) %>%
  select(name = goalieNameForShot, adj_gsax_shot) %>%
  arrange(-adj_gsax_shot)

final_data <- shots %>%
  group_by(id = goalieIdForShot, name = goalieNameForShot) %>%
  filter(!is.na(goalieNameForShot)) %>%
  summarize(shots = n(), goals = sum(goal), xg = sum(xGoal), gsax = xg - goals) %>%
  arrange(-gsax) %>%
  left_join(goalies, by = "name") %>%
  mutate(adj_gsax = adj_gsax_shot * shots, adj_xg = adj_gsax + goals, gop = goals/xg, adj_gop = goals/adj_xg) %>%
  arrange(adj_gop)

final_data_filt <- final_data %>% filter(shots >= 1000)
dist <- fitdistrplus::fitdist(final_data_filt$adj_gop, "gamma")

prior_shape <- dist$estimate[1]
prior_rate <- dist$estimate[2]

# gamma simulation code adapted from tony el habr's blog on soccer overperformance

simulate_gamma_posterior <- function(
    successes, 
    trials, 
    prior_shape, 
    prior_rate, 
    n_sims = 10000,
    seed = 42
) {
  posterior_shape <- prior_shape + successes
  posterior_rate <- prior_rate + trials
  withr::local_seed(seed)
  posterior_sample <- rgamma(n = n_sims, shape = posterior_shape, rate = posterior_rate)
  list(
    mean = mean(posterior_sample),
    sd = sd(posterior_sample)
  )
}

final_data$bayes_adj_op <- map2(
  final_data$goals, final_data$adj_xg,
  function(g, xg) {
    simulate_gamma_posterior(
      successes = g,
      trials = xg,
      prior_shape = prior_shape,
      prior_rate = prior_rate
    )
  }
)

final_data <- final_data %>%
  unnest_wider(
    bayes_adj_op, 
    names_sep = '_'
  ) %>%
  arrange(bayes_adj_op_mean) 

goalie_stats <- get_goalie_stats_hr(season = 2024) %>%
  group_by(player) %>%
  summarize(team = last(team_abbr))

goalie_stats$player = stri_trans_general(str = goalie_stats$player, id = "Latin-ASCII")
goalie_stats$player[which(goalie_stats$player == "Calvin Petersen")] <- "Cal Petersen"
goalie_stats$player[which(goalie_stats$player == "Daniel Vladar")] <- "Dan Vladar"

final_data <- final_data %>%
  left_join(goalie_stats, by = c("name"="player")) %>%
  mutate(gsax = round(gsax, 1), bayes_adj_op_mean = round(bayes_adj_op_mean, 3)) %>%
  mutate(headshot_link = paste0("https://assets.nhle.com/mugs/nhl/20232024/", team, "/", id, ".png")) %>%
  ungroup() %>%
  dplyr::select(headshot_link, name, team, shots, gsax, gopv = bayes_adj_op_mean)

logos <- team_logos_colors %>%
  dplyr::select(team = team_abbr, team_logo_espn)

final_data$team[which(final_data$team == "VEG")] <- "VGK"

final_data <- left_join(final_data, logos, by = "team") %>%
  dplyr::select(headshot_link, name, team_logo_espn, shots, gsax, gopv)

t10 <- final_data %>% head(10)
b10 <- final_data %>% tail(10) %>% arrange(-gopv)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>MoneyPuck</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

gt_func <- function(df, str) {
  save_table <- df %>% gt() %>% 
    gt_img_rows(columns = team_logo_espn) %>%
    gt_img_rows(columns = headshot_link) %>%
    gt_theme_538() %>%
    cols_align(
      align = "center",
      columns = c(headshot_link, name, team_logo_espn, shots, gsax, gopv)
    ) %>%
    gt_hulk_col_numeric(c(shots, gsax)) %>%
    gt_hulk_col_numeric(gopv, reverse = TRUE) %>%
    cols_label(
      headshot_link = md(""),
      name = md("**Player**"),
      team_logo_espn = md("**Team**"),
      shots = md("**Shots**"),
      gsax = md("**GSAx**"),
      gopv = md("**GOPV**")
    ) %>%
    tab_header(
      title = paste0("2023/24 NHL Goalie Overperformance Value ", str, " 10"),
      subtitle = md("*GOPV = **Linear Bayesian** Adjustment Of **Goals Against** and **xGA** Ratio | **Lower** GOPV Is **Better***")
    ) %>%
    opt_align_table_header(align = "center") %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(name, shots, gsax, gopv)
      )
    ) %>%
    tab_source_note(html(caption)) %>%
    cols_width(headshot_link ~ px(50), name ~ px(150), team_logo_espn ~ px(50), shots ~ px(75), gsax ~ px(75), gopv ~ px(75))
  
  return(save_table)
}

gtsave(gt_func(t10, "Top"), "t10.png")
gtsave(gt_func(b10, "Bottom"), "b10.png")