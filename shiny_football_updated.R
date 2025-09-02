library(shiny)
library(tidyverse)
library(nflfastR)

pbp <- load_pbp(2019:2024) %>%
  arrange(game_id,play_id)


## Punts within 10

offensive_plays <- c("run", "pass", "qb_spike", "qb_kneel")

punts <- pbp %>%
  filter(play_type == "punt") %>%
  select(game_id, play_id, posteam, yardline_100, season)

offensive_next_plays <- pbp %>%
  filter(play_type %in% offensive_plays) %>%
  select(game_id, play_id, posteam, yardline_100)

joined <- punts %>%
  left_join(offensive_next_plays, by = "game_id", suffix = c("_punt", "_next")) %>%
  filter(play_id_next > play_id_punt, posteam_next != posteam_punt) %>%
  group_by(game_id, play_id_punt) %>%
  slice_min(play_id_next) %>%
  ungroup()

total_punts <- nrow(joined)
inside_10_count <- sum(joined$yardline_100_next >= 90, na.rm = TRUE)

joined <- joined %>%
  mutate(FieldPositionGroup = cut(
    yardline_100_punt,
    breaks = seq(30, 80),
    include.lowest = TRUE
  )) %>%
  filter(!is.na(FieldPositionGroup))

team_season_punt_stats <- joined %>%
  group_by(season, posteam_punt) %>%
  summarize(
    total_punts      = n(),
    inside_10_count  = sum(yardline_100_next >= 90, na.rm = TRUE),
    inside_10_pct    = round(100 * inside_10_count / total_punts, 1)
  ) %>%
  arrange(season, desc(-inside_10_pct))


## Aggression

fourth_down_plays <- pbp %>%
  filter(
    down == 4,
    ydstogo <= 5,
    penalty == 0,
    !is.na(epa),  
    !is.na(play_type),
    !play_type %in% c("no_play", "qb_kneel", "qb_spike")  
  )

fourth_down_plays <- fourth_down_plays %>%
  mutate(go_for_it = as.integer(play_type %in% c("run", "pass")))

coach_aggressiveness <- fourth_down_plays %>%
  group_by(posteam, season) %>%
  summarise(
    total_4th_downs = n(),
    go_for_it_attempts = sum(go_for_it),
    coach_aggressiveness = mean(go_for_it)
  ) %>%
  arrange(season, desc(-coach_aggressiveness))


## FG PCT (excluding attempts from within 20 yards (2 yard line))

field_goals <- pbp %>%
  filter(play_type == "field_goal", kick_distance > 20)


team_fg_by_season <- field_goals %>%
  group_by(posteam, season) %>%
  summarize(
    total_fg_att  = n(),
    total_fg_made = sum(field_goal_result == "made", na.rm = TRUE),
    fg_percentage = round(total_fg_made / total_fg_att * 100, 1)
  ) %>%
  arrange(season, desc(-fg_percentage))


## Offensive Yardage

yardage <- pbp %>%
  filter(
    !is.na(yards_gained),
    !is.na(result),
    play_type %in% c("run", "pass"))

total_yardage_by_team_season <- yardage %>%
  group_by(season, posteam) %>%
  summarise(
    total_yardage = sum(yards_gained)
  ) %>%
  ungroup()%>%
  arrange(season, -total_yardage)


## TEAM RANK STATS

team_season_punt_stats <- team_season_punt_stats %>%
  group_by(season) %>%
  mutate(inside_10_pct_rank = dense_rank(desc(inside_10_pct))) %>%
  ungroup()

coach_aggressiveness <- coach_aggressiveness %>%
  group_by(season) %>%
  mutate(coach_aggressiveness_rank = dense_rank(desc(coach_aggressiveness))) %>%
  ungroup()

team_fg_by_season <- team_fg_by_season %>%
  group_by(season) %>%
  mutate(fg_percentage_rank = dense_rank(desc(fg_percentage))) %>%
  ungroup()

total_yardage_by_team_season <- total_yardage_by_team_season %>%
  group_by(season) %>%
  mutate(total_yardage_rank = dense_rank(desc(total_yardage))) %>%
  ungroup()

team_season_punt_stats <- team_season_punt_stats %>%
  rename(posteam = posteam_punt)

final_stats <- team_season_punt_stats %>%
  left_join(coach_aggressiveness,      by = c("season", "posteam")) %>%
  left_join(team_fg_by_season,        by = c("season", "posteam")) %>%
  left_join(total_yardage_by_team_season, by = c("season", "posteam"))

print(final_stats, n = 32)



###### JUST TEAM RANKINGS #######

final_stats_rank <- team_season_punt_stats %>%
  left_join(coach_aggressiveness, by = c("season", "posteam")) %>%
  left_join(team_fg_by_season,    by = c("season", "posteam")) %>%
  left_join(total_yardage_by_team_season, by = c("season", "posteam")) %>%
  select(
    season,
    posteam,
    inside_10_pct_rank,
    coach_aggressiveness_rank,
    fg_percentage_rank,
    total_yardage_rank
  )

pbp <- pbp %>% left_join(final_stats_rank, by = c("season", "posteam"))


pbp <- pbp %>%
  mutate(
    across(c(inside_10_pct_rank, coach_aggressiveness_rank, fg_percentage_rank, total_yardage_rank), as.numeric))


## FILTER DATA

pbp <- pbp %>% filter(down==4,play_type!="no_play",play_type!="qb_kneel") %>%
  mutate(
    # Assign play types based on success & EPA:
    play_type_new = case_when(
      play_type == "run" | play_type == "pass" ~ "go_for_it",  # Converted on 4th down or made FG
      play_type == "punt" ~ "punt",       # Failed conversion or bad punt
      play_type == "field_goal" ~ "field_goal", # Rough estimate for FG makes
      TRUE ~ NA_character_
    ))
real_props <- pbp$play_type_new %>% table() %>% proportions()


##### Group by the Play type and yardline on the field to average epa so that we can group similar plays together #####
pbp <- pbp %>%
  mutate(
    yardline_bin = cut(yardline_100, 
                       breaks = seq(0, 100, by = 10), 
                       labels = seq(10, 100, by = 10), 
                       include.lowest = TRUE)
  ) %>%
  group_by(play_type, yardline_bin) %>%
  mutate(
    expected_epa = mean(epa, na.rm = TRUE) # Average EPA for similar plays in each yardline bin
  ) %>%
  ungroup() %>%
  mutate(
    punt_expected_epa = ifelse(play_type_new == "punt", expected_epa, NA),
    field_goal_expected_epa = ifelse(play_type_new == "field_goal", expected_epa, NA),
    go_for_it_expected_epa = ifelse(play_type_new == "go_for_it", expected_epa, NA)
  )


pbp <- pbp %>%
  group_by(yardline_100) %>%
  mutate(
    punt_expected_epa = ifelse(is.na(punt_expected_epa), mean(punt_expected_epa, na.rm = TRUE), punt_expected_epa),
    field_goal_expected_epa = ifelse(is.na(field_goal_expected_epa), mean(field_goal_expected_epa, na.rm = TRUE), field_goal_expected_epa),
    go_for_it_expected_epa = ifelse(is.na(go_for_it_expected_epa), mean(go_for_it_expected_epa, na.rm = TRUE), go_for_it_expected_epa)
  ) %>%
  ungroup()
pbp$go_for_it_expected_epa <- ifelse(is.na(pbp$go_for_it_expected_epa), mean(pbp$go_for_it_expected_epa, na.rm = TRUE), pbp$go_for_it_expected_epa)
pbp$field_goal_expected_epa[is.na(pbp$field_goal_expected_epa)] = -10
for (i in 1:length(pbp$play_id)){
  pbp$punt_expected_epa[i] <- ifelse(is.na(pbp$punt_expected_epa[i]), mean(pbp$punt_expected_epa, na.rm = TRUE), no = pbp$punt_expected_epa[i])
}
pbp_long <- pbp %>%
  pivot_longer(cols=c("punt_expected_epa","field_goal_expected_epa","go_for_it_expected_epa"), names_to = "Play", values_to = "EPA")


# Create Best Decision
pbp <- pbp %>%
  mutate(
    best_decision = case_when(
      go_for_it_expected_epa == pmax(field_goal_expected_epa, punt_expected_epa,go_for_it_expected_epa) ~ "go_for_it",
      field_goal_expected_epa == pmax(go_for_it_expected_epa, punt_expected_epa, field_goal_expected_epa) ~ "field_goal",
      punt_expected_epa == pmax(go_for_it_expected_epa, field_goal_expected_epa, punt_expected_epa) ~ "punt"
    ) %>%
      as.factor()
  )
epa_props <- pbp$best_decision %>% table() %>% proportions()

library(xgboost)
library(caret)

# Create Machine Learning Model
features <- pbp[,c("score_differential_post","yardline_100","wp","ydstogo","inside_10_pct_rank", "coach_aggressiveness_rank", "fg_percentage_rank", "total_yardage_rank","game_seconds_remaining")]
# Convert to matrix for XGBoost
X <- as.matrix(features)
y <- as.factor(pbp$best_decision)

# Split data (80% train, 20% test)
set.seed(1234)
train_idx <- sample(length(pbp)*0.8,replace=FALSE)
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[-train_idx, ]
y_test <- y[-train_idx]
as.numeric(y_train) - 1 %>% table() %>% proportions()
# Train XGBoost model
xgb_model <- xgboost(
  data = X_train, label = as.numeric(y_train) - 1, # Convert to numeric
  nrounds = 100, objective = "multi:softprob",
  num_class = 3, eval_metric = "mlogloss"
)
#Step 5: Define Dynamic 
#k dynamically by comparing predicted decision confidence:
# Get model predictions
pred_probs <- predict(xgb_model, X_test)
pred_probs <- matrix(pred_probs, ncol = 3, byrow = TRUE)
# Assign k based on decision confidence
k_values <- apply(pred_probs, 1, function(probs) {
  confidence <- max(probs) # Confidence of best choice
  return( (1 - confidence) * 10 ) # Higher confidence -> lower k
})
summary(k_values)
pbp$k_dynamic <- NA
pbp$k_dynamic[-train_idx] <- k_values


epa_fg <- pbp %>% filter(field_goal_attempt == 1)
epa_fg <- epa_fg$field_goal_expected_epa
epa_punt <- pbp$punt_expected_epa
epa_go <- pbp$go_for_it_expected_epa
only_fg_epa <- pbp$field_goal_expected_epa[pbp$field_goal_expected_epa != -10]
pbp <- pbp %>% 
  mutate(
    go_for_it_adjusted_epa = go_for_it_expected_epa - k_dynamic * sd(go_for_it_expected_epa),
    field_goal_adjusted_epa = field_goal_expected_epa - k_dynamic * sd(only_fg_epa),
    punt_adjusted_epa = punt_expected_epa - k_dynamic * sd(punt_expected_epa),
    final_decision = case_when(
      go_for_it_adjusted_epa > pmax(field_goal_adjusted_epa, punt_adjusted_epa) ~ "go_for_it",
      field_goal_adjusted_epa > pmax(go_for_it_adjusted_epa, punt_adjusted_epa) ~ "field_goal",
      punt_adjusted_epa > pmax(go_for_it_adjusted_epa, field_goal_adjusted_epa) ~ "punt"
    )
  )
vec <- c(sd(pbp$punt_expected_epa),
         sd(only_fg_epa),
         sd(pbp$go_for_it_expected_epa))
final_props <- pbp$final_decision %>% table() %>% proportions()
sd_df <- data.frame(Plays=c("Punt SD","FG SD","Go For It SD"),Standard_Deviations = vec)
sd_df
decision_props_df <- data.frame(Play = c("Field Goal","Go For It","Punt"),Coach_decision = as.numeric(real_props),EPA_decision = as.numeric(epa_props), XGBoost_decision = as.numeric(final_props))

compare_df <- pbp %>% select(best_decision,final_decision)
compare_df <- na.omit(compare_df)


punt_sd <- sd(pbp$punt_expected_epa, na.rm = TRUE)
fg_sd <- sd(pbp$field_goal_expected_epa[pbp$field_goal_expected_epa != -10], na.rm = TRUE)
go_sd <- sd(pbp$go_for_it_expected_epa, na.rm = TRUE)


draw_field <- function(yardline_100, decision, home_team = "Home", away_team = "Away", ydstogo = 10) {
  # Ball position (0 = own endzone, 100 = opponent endzone)
  ball_pos <- 100 - yardline_100
  first_down <- ball_pos + ydstogo
  if (first_down > 100) first_down <- 100  # Cap at endzone
  
  # Color ball based on decision
  decision_color <- case_when(
    decision == "go_for_it"   ~ "blue",
    decision == "punt"        ~ "yellow",
    decision == "field_goal"  ~ "red",
    TRUE ~ "orange"
  )
  
  df_field <- data.frame(x = seq(0, 100, by = 10), y = 0)
  
  ggplot() +
    # Field background
    geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 50), 
              fill = "forestgreen", color = "white") +
    
    # Yard lines
    geom_vline(xintercept = seq(0, 100, 10), color = "white") +
    
    # Yard numbers
    geom_text(data = df_field, aes(x = x, y = 25, label = x), 
              color = "white", size = 4) +
    
    # Endzones
    geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = 50), fill = "blue", alpha = 0.6) +
    geom_rect(aes(xmin = 90, xmax = 100, ymin = 0, ymax = 50), fill = "red", alpha = 0.6) +
    
    # Endzone labels
    annotate("text", x = 5, y = 25, label = home_team, angle = 90, color = "white", size = 5, fontface = "bold") +
    annotate("text", x = 95, y = 25, label = away_team, angle = 270, color = "white", size = 5, fontface = "bold") +
    
    # First down marker
    geom_vline(xintercept = first_down, color = "yellow", size = 1.5, linetype = "dashed") +
    
    # Ball
    geom_point(aes(x = ball_pos, y = 25), 
               color = decision_color, size = 8, shape = 21, fill = decision_color) +
    
    # Coordinates
    coord_fixed(ratio = 1/2, xlim = c(0, 100), ylim = c(0, 50)) +
    theme_void() +
    ggtitle("Ball Position on Field")
}


nfl_teams <- c(
  "ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE",
  "DAL","DEN","DET","GB","HOU","IND","JAX","KC",
  "LV","LAC","LAR","MIA","MIN","NE","NO","NYG",
  "NYJ","PHI","PIT","SEA","SF","TB","TEN","WAS"
)


## SHINY APP ##

ui <- fluidPage(
  titlePanel("4th Down Decision Maker"),
  sidebarLayout(
    sidebarPanel(
      numericInput("yardline_100", "Yardline (distance from opponent's end zone)", value = 50, min = 1, max = 99),
      numericInput("off_score", "Offense Score", value = 0, min = 0),
      numericInput("def_score", "Defense Score", value = 0, min = 0),
      numericInput("game_seconds", "Seconds Remaining in Game", value = 900),
      numericInput("ydstogo", "Yards To Go", value = 4),
      numericInput("wp", "Win Probability (0 to 1)", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("inside_10_rank", "Punt Efficiency Rank", value = 16, min = 1, max = 32),
      numericInput("coach_rank", "Coach Aggressiveness Rank", value = 16, min = 1, max = 32),
      numericInput("fg_rank", "FG % Rank", value = 16, min = 1, max = 32),
      numericInput("yardage_rank", "Offensive Yardage Rank", value = 16, min = 1, max = 32),
      selectInput(
        inputId = "def_team", 
        label = "Choose a Team:", 
        choices = nfl_teams, 
        selected = "NE"
      ),
      selectInput(
        inputId = "off_team", 
        label = "Choose a Team:", 
        choices = nfl_teams, 
        selected = "NE"
      ),
      actionButton("predict", "Get Best Decision")
    ),
    mainPanel(
      h3("Scoreboard:"),
      uiOutput("scoreboard_ui"),
      h3("Recommended Decision:"),
      verbatimTextOutput("final_decision"),
      plotOutput("field_plot", height = "300px")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    score_diff <- reactive({
      input$off_score - input$def_score
    })
    # Build feature matrix for prediction
    input_matrix <- matrix(c(
      score_diff(),
      input$yardline_100,
      input$wp,
      input$ydstogo,
      input$inside_10_rank,
      input$coach_rank,
      input$fg_rank,
      input$yardage_rank,
      input$game_seconds
    ), nrow = 1)
    
    # Predict decision probabilities
    probs <- predict(xgb_model, input_matrix)
    probs <- matrix(probs, ncol = 3, byrow = TRUE)
    
    confidence <- max(probs)
    k <- (1 - confidence) * 10  # Dynamic k
    
    # Estimate EPA by yardline bin
    yard_bin <- cut(input$yardline_100, breaks = seq(0, 100, by = 10), labels = seq(10, 100, by = 10), include.lowest = TRUE)
    
    avg_epas <- pbp %>%
      filter(!is.na(yardline_bin), yardline_bin == yard_bin) %>%
      summarise(
        go_for_it = mean(go_for_it_expected_epa, na.rm = TRUE),
        punt = mean(punt_expected_epa, na.rm = TRUE),
        fg = mean(field_goal_expected_epa[field_goal_expected_epa != -10], na.rm = TRUE)
      )
    
    go_epa <- avg_epas$go_for_it - k * go_sd
    punt_epa <- avg_epas$punt - k * punt_sd
    fg_epa <- avg_epas$fg - k * fg_sd
    
    best_play <- c(go_for_it = go_epa, punt = punt_epa, field_goal = fg_epa) %>%
      which.max() %>%
      names()
    
    output$final_decision <- renderText({
      paste0("→ ", best_play)
    })
    output$field_plot <- renderPlot({
      draw_field(input$yardline_100, best_play,
                 home_team = input$def_team,
                 away_team = input$off_team,
                 ydstogo = input$ydstogo)
      })
    output$scoreboard_ui <- renderUI({
      tags$div(
        style = "background-color:black; color:white; padding:10px; border-radius:10px; width:300px; text-align:center;",
        tags$h4("SCOREBOARD"),
        tags$div(
          style = "display:flex; justify-content:space-between; font-size:18px;",
          tags$span(paste(input$off_team, input$off_score)),
          tags$span(paste(input$def_score, input$def_team))
        )
      )
    })
  })
}

shinyApp(ui = ui, server = server)