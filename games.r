
games <- read.csv("~/Downloads/archive/games.csv", header=TRUE)

install.packages("readr")
install.packages("car")
install.packages("dunn.test")
library(dplyr)

install.packages("ggplot2")



# Remove rows with missing values in relevant columns
games <- na.omit(games, cols = c("PTS_home", "PTS_away", "FG3_PCT_home", "FG3_PCT_away"))

# Calculate total three-point shots made per game
games$TOTAL_FG3 <- games$FG3_PCT_home * games$PTS_home + games$FG3_PCT_away * games$PTS_away

# Remove outliers
mean_fg3 <- mean(games$TOTAL_FG3, na.rm = TRUE)
sd_fg3 <- sd(games$TOTAL_FG3, na.rm = TRUE)
games <- subset(games, TOTAL_FG3 > (mean_fg3 - 3 * sd_fg3) & TOTAL_FG3 < (mean_fg3 + 3 * sd_fg3))









## Test 1
games$SEASON <- as.numeric(as.character(games$SEASON))

model <- lm(Avg_FG3 ~ SEASON, data = games)
summary(model)









library(ggplot2)

# Scatter plot with regression line
ggplot(games, aes(x = SEASON, y = Avg_FG3)) +
  geom_point() + # This adds the scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # This adds the regression line
  labs(title = "Trend in Average Three-Point Shots Made per Game by Season",
       x = "Season",
       y = "Average Three-Point Shots Made per Game") +
  theme_minimal()









#### test 2
library(dplyr)


games <- games %>%
  mutate(
    FG3_home = FG3_PCT_home * PTS_home, # Calculate the home team three-point shots
    FG3_away = FG3_PCT_away * PTS_away  # Calculate the away team three-point shots
  )
model <- lm(FG3_home ~ FG3_away, data = games)
summary(model)






# Test 3

games <- games %>%
  mutate(Total_FG3_home = FG3_PCT_home * PTS_home)
model_reb_fg3 <- lm(Total_FG3_home ~ REB_home, data = games)
summary(model_reb_fg3)





# Test 4:

games <- games %>%
  mutate(
    FG3_diff = FG3_PCT_home * PTS_home - FG3_PCT_away * PTS_away,
    more_fg3_wins = ifelse((FG3_diff > 0 & HOME_TEAM_WINS == 1) | (FG3_diff < 0 & HOME_TEAM_WINS == 0), 1, 0)
  )
# Logistic regression
model <- glm(more_fg3_wins ~ FG3_diff, data = games, family = binomial())

# Summary of the model
summary(model)
