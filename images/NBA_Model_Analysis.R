## NBA Data Science Project
## Richard Kish
## December 2024

# This project showcases the development of two predictive models
# (one finished, and one in development)
# which is showcased through several analyses of NBA player performances.
# The predictive model in progress is intended to predict the career outcome of 
# any NBA player depending on their history of previous season outcomes.
# Career outcome probabilities are modeled using historic data of the
# the distribution of season outcomes across different career outcomes of NBA
# players over the past 20+ years.
# The second predictive model is complete, and can predict the rebound success
# percentage in the n-th game of any season for any team, based on
# historical rebounding data for that season. Linear and polynomial regressions
# are used to explore the rebound behaviors of the NY Knicks and Brooklyn Nets.

# Date range for this data is between the 1991-92 and 2022-23 NBA seasons.

################################################################################

library(tidyverse)
library(ggplot2)
awards <- read_csv("awards.csv")
player_data <- read_csv("player_data.csv")
team_data <- read_csv("team_data.csv")
rebounding_data <- read_csv("rebound_data.csv")

################################################################################

  # ANALYSIS #1: Average Points Scored per Game by All-NBA and All-Star Winners
  # Calculating the average number of points scored per game for 2007-2021 
  # players who were All NBA + All-Star winners, excluding All-Defensive + 
  # All-Star Rookie winners. Results separated by each All-NBA team ranking

# First Team Winners
first_team_win <- filter(awards, awards[5] == 1) # All NBA First Team winners
first_team_win <- filter(first_team_win, first_team_win[3] == 0) # Remove All Defensive First Team
first_team_win <- filter(first_team_win, first_team_win[4] == 0) # Remove All Defensive Second Team

# Second Team Winners
second_team_win <- filter(awards, awards[6] == 1) # All NBA Second Team winners
second_team_win <- filter(second_team_win, second_team_win[3] == 0) # Remove All Defensive First Team 
second_team_win <- filter(second_team_win, second_team_win[4] == 0) # Remove All Defensive Second Team 

# Third Team Winners
third_team_win <- filter(awards, awards[7] == 1) # All NBA Third Team winners
third_team_win <- filter(third_team_win, third_team_win[3] == 0) # Remove All Defensive First Team 
third_team_win <- filter(third_team_win, third_team_win[4] == 0) # Remove All Defensive Second Team 

# All-Star Team Players
allstar_players <- filter(awards, awards[14] == TRUE) # All NBA All-Star players
allstar_players <- filter(allstar_players, allstar_players[15] == FALSE) # Remove Rookie All-Star players

# Match player_data stats with the previously filtered datasets above
first_team_stats <- right_join(player_data, first_team_win, copy = FALSE) # Only 1st Team winner stats
second_team_stats <- right_join(player_data, second_team_win, copy = FALSE) # 2nd Team winner stats
third_team_stats <- right_join(player_data, third_team_win, copy = FALSE) # 3rd Team winner stats
allstar_player_stats <- right_join(player_data, allstar_players, copy = FALSE) # All-Star player stats

## Calculating Average Points per Game
# First Team Winners
first_team_avg <- summarise(first_team_stats, points, games) %>% 
  summarise_all(funs(sum)) # Summarize point/game count for First Team winners
first_team_avg$avg <- round(first_team_avg$points/first_team_avg$games, digits = 1) # Average of points per game

# Second Team Winners
second_team_avg <- summarise(second_team_stats, points, games) %>% 
  summarise_all(funs(sum)) # Summarize point/game count for Second Team winners
second_team_avg$avg <- round(second_team_avg$points/second_team_avg$games, digits = 1) # Average of points per game

# Third Team Winners
third_team_avg <- summarise(third_team_stats, points, games) %>% 
  summarise_all(funs(sum)) # Summarize point/game count for Third Team winners
third_team_avg$avg <- round(third_team_avg$points/third_team_avg$games, digits = 1) # Average of points per game

# All-Star Team Players
allstar_player_avg <- summarise(allstar_player_stats, points, games) %>% 
  summarise_all(funs(sum)) # Summarize point/game count for All-Star players
allstar_player_avg$avg <- round(allstar_player_avg$points/allstar_player_avg$games, digits = 1) # Average of points per game

# All averages combined in this dataset
all_averages <- full_join(first_team_avg, second_team_avg, copy = FALSE)
all_averages <- full_join(all_averages, third_team_avg, copy = FALSE)
all_averages <- full_join(all_averages, allstar_player_avg, copy = FALSE)
teams = c('1st Team', '2nd Team', '3rd Team', 'All-Star Players') # Create names for each value
all_averages$team <- teams # Append names to averages dataset

# Visualization
ggplot(all_averages, aes(y = avg, x = team, fill = factor(team), label = avg))+
  geom_bar(stat="identity",position="dodge") + xlab("Specific All-NBA Team") + 
  ylab("Average Points per Game") + scale_fill_discrete(name = "All-NBA Team") + 
  ggtitle("Average Points per Game by All-NBA Teams during 2007-2021 Seasons (All-NBA)") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5))

################################################################################

  # ANALYSIS #2: Average Number of Years from Draft to All-NBA Selection
  # Calculating the average number of years it takes from a player's draft to
  # their first All-NBA selection, using players drafted in or after 1992

# These datasets include 1st, 2nd, and 3rd team stats
first_team_stats
second_team_stats
third_team_stats

# Remove all All-NBA Award winners drafted prior to 1992
first_team_stats <- first_team_stats %>% arrange(season)
first_team_stats1992 <- subset(first_team_stats, draftyear >= 1992) # First Team

second_team_stats <- second_team_stats %>% arrange(season)
second_team_stats1992 <- subset(second_team_stats, draftyear >= 1992) # Second Team

third_team_stats <- third_team_stats %>% arrange(season)
third_team_stats1992 <- subset(third_team_stats, draftyear >= 1992) # Third Team

# Combine all stats for players drafted 1992 and after
all_stats1992 <- full_join(first_team_stats1992, second_team_stats1992, copy = FALSE)
all_stats1992 <- full_join(all_stats1992, third_team_stats1992, copy = FALSE)

# Remove all but first year All-NBA selections
all_stats1992 <- all_stats1992 %>% distinct(player, .keep_all = TRUE) # 1st, 2nd, and 3rd Team

# Calculate time between draft and All-NBA selection
all_stats1992$selectiontime <- all_stats1992$season - all_stats1992$draftyear

# Average of time between draft and All-NBA selection
NBA_selection_avg <- mean(all_stats1992$selectiontime) # yields 4.6

# Visualization of draft to All-NBA selection times for all 1st, 2nd, and 3rd team winners
ggplot(all_stats1992, aes(y = selectiontime, x = player, fill = factor(player), label = selectiontime))+
  geom_bar(stat="identity",position="dodge") + 
  theme(legend.key.size = unit(0.1, 'cm'), legend.key.width = unit(0.25, 'cm')) + 
  xlab("Player") + ylab("Years from draft to All-NBA Selection") + 
  scale_fill_discrete(name = "Player") + 
  ggtitle("Number of Years from Draft to All-NBA Selection (Drafted 1992-2021)") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
  theme(axis.text.x=element_blank()) + 
  geom_hline(yintercept = mean(all_stats1992$selectiontime), color = "red") + 
  annotate("text", x = 26, y = 10.9, label="Average = 6.22 years") + 
  geom_segment(aes(x = 27, y = 6.5, xend = 27, yend = 10.5, color = 'red'), arrow = arrow(length = unit(0.5, "cm")), show.legend = FALSE)

################################################################################

  # ANALYSIS #3: Career Outcomes of Players
  # Calculating the career outcomes for all players drafted in or prior to 2015
  # Possible outcomes include Elite, All-Star, Roster, Rookie, Rotation, and Out-of-League.
  # Career outcomes are determined based on the highest outcome achieved in any 
  # season, and their status in the NBA after their fourth season.
  # (However, 3 additional seasons will be required to determine distinct career
  # outcome).

# Filter to only players drafted in or prior to 2015 (change to whichever range desired)
player_data_time <- subset(player_data, draftyear <= 2015) # Since it takes at least
                                                           # 5 seasons total to determine
                                                           # career outcome

# Join awards and player data
player_stats_time <- right_join(awards, player_data_time, copy = FALSE)
player_stats_time <- player_stats_time %>% arrange(nbapersonid, season)
player_stats_time <- player_stats_time %>% mutate(season_outcome = NA)

# Season outcome logic: assigns the six season outcome labels to each player's season
# 82/66 multiplier used for 2011 season game count due to lockout season
# 82/72 multiplier used for 2019/20 seasons game count due to COVID seasons
row_number = 1 # Initialize loop
while (row_number <= nrow(player_stats_time)) {
  if (!is.na(player_stats_time$`All NBA First Team`[row_number]) &&
      player_stats_time$`All NBA First Team`[row_number] != 0 || 
      !is.na(player_stats_time$`All NBA Second Team`[row_number]) &&
      player_stats_time$`All NBA Second Team`[row_number] != 0 || 
      !is.na(player_stats_time$`All NBA Third Team`[row_number]) &&
      player_stats_time$`All NBA Third Team`[row_number] != 0 || 
      !is.na(player_stats_time$`Defensive Player Of The Year_rk`[row_number]) &&
      player_stats_time$`Defensive Player Of The Year_rk`[row_number] != 0 || 
      !is.na(player_stats_time$`Most Valuable Player_rk`[row_number]) &&
      player_stats_time$`Most Valuable Player_rk`[row_number] != 0) {
    player_stats_time$season_outcome[row_number] <- 'Elite'
  } else if (!is.na(player_stats_time$all_star_game[row_number]) &&
             player_stats_time$all_star_game[row_number] != 0) {
    player_stats_time$season_outcome[row_number] <- 'All-Star'
  } else if (((player_stats_time$games_start[row_number] >= 41 || #Special conditions
              player_stats_time$mins[row_number] >= 2000) &&      #for these years
              player_stats_time$season[row_number] != (2011 || 2019 || 2020)) ||
              ((player_stats_time$games_start[row_number] >= (41 / (82/66)) || 
              player_stats_time$mins[row_number] >= round(2000 / (82/66), digits = 0)) && 
              player_stats_time$season[row_number] == 2011) || 
              ((player_stats_time$games_start[row_number] >= round(41 / (82/72), digits = 0) || 
              player_stats_time$mins[row_number] >= round(2000 / (82/72), digits = 0)) && 
              player_stats_time$season[row_number] == (2019 || 2020))) {
    player_stats_time$season_outcome[row_number] <- 'Starter'
  } else if ((player_stats_time$mins[row_number] >= 1000 && 
              player_stats_time$season[row_number] != (2011 || 2019 || 2020)) || 
              (player_stats_time$mins[row_number] >= round(1000 / (82/66), digits = 0) &&
              player_stats_time$season[row_number] == 2011) || 
              (player_stats_time$mins[row_number] >= round(1000 /(82/72), digits = 0) && 
              player_stats_time$season[row_number] == (2019 || 2020))) {
    player_stats_time$season_outcome[row_number] <- 'Rotation'
  } else if (player_stats_time$mins[row_number] >= 1) {
    player_stats_time$season_outcome[row_number] <- 'Roster'
  } else if (player_stats_time$mins[row_number] == 0) {
    player_stats_time$season_outcome[row_number] <- 'Out of the League'
  }
  row_number <- row_number + 1; # Go to next row in dataset
}

# Initialize a career outcome dataset for tallying with length of number of unique players
career_outcome <- data.frame("Player" = character(n_distinct(player_stats_time$player)), "Elite" = numeric(n_distinct(player_stats_time$player)), "All_Star" = numeric(n_distinct(player_stats_time$player)), "Starter" = numeric(n_distinct(player_stats_time$player)), "Rotation" = numeric(n_distinct(player_stats_time$player)), "Roster" = numeric(n_distinct(player_stats_time$player)), "Out_of_the_League" = numeric(n_distinct(player_stats_time$player)), "Outcome" = character(n_distinct(player_stats_time$player)))

# Initialize season tally starting conditions
row_number = 1 # Initialize loop condition
season_number = 1 # Initialize inner loop condition
current_id = player_stats_time$nbapersonid[1] # Initializes with first athlete in dataset
career_row = 1 # Initialize career outcome loop

# Tally up season outcomes
while (row_number <= nrow(player_stats_time)) { # Will scan entirety of player_stats_time
  career_outcome$Player[career_row] <- current_id
  while (player_stats_time$nbapersonid[row_number] == current_id) {
    while (season_number <= 4) { # Skip first four seasons of each player
      if (row_number > nrow(player_stats_time)) {
        break
      }
      if (player_stats_time$nbapersonid[row_number] == current_id) {
        season_number <- season_number + 1;
        row_number <- row_number + 1;
      } else { # If player does not exceed 4 seasons, no check to be taken
        break
      }
    } 
    if (row_number > nrow(player_stats_time)) {
      break
    }
    if (player_stats_time$season_outcome[row_number] == 'Elite' && player_stats_time$nbapersonid[row_number] == current_id) {
      career_outcome$Elite[career_row] <- career_outcome$Elite[career_row] + 1
    } else if (player_stats_time$season_outcome[row_number] == 'All-Star' && player_stats_time$nbapersonid[row_number] == current_id) {
      career_outcome$All_Star[career_row] <- career_outcome$All_Star[career_row] + 1
    } else if (player_stats_time$season_outcome[row_number] == 'Starter' && player_stats_time$nbapersonid[row_number] == current_id) {
      career_outcome$Starter[career_row] <- career_outcome$Starter[career_row] + 1
    } else if (player_stats_time$season_outcome[row_number] == 'Rotation' && player_stats_time$nbapersonid[row_number] == current_id) {
      career_outcome$Rotation[career_row] <- career_outcome$Rotation[career_row] + 1
    } else if (player_stats_time$season_outcome[row_number] == 'Roster' && player_stats_time$nbapersonid[row_number] == current_id) {
      career_outcome$Roster[career_row] <- career_outcome$Roster[career_row] + 1
    } else if (player_stats_time$season_outcome[row_number] == 'Out of the League' && player_stats_time$nbapersonid[row_number] == current_id) {
      career_outcome$Out_of_the_League[career_row] <- career_outcome$Out_of_the_League[career_row] + 1
    } else {
      career_outcome$Out_of_the_League[career_row] <- career_outcome$Out_of_the_League[career_row] + 1
      break
    }
    row_number <- row_number + 1; # Go to next row in player_stats2010
    if (row_number > nrow(player_stats_time)) {
      break
    }
  }
  if (row_number > nrow(player_stats_time)) {
    break
  }
  season_number = 1 # Resets season counter for next athlete
  current_id <- player_stats_time$nbapersonid[row_number]; # Switch to the next athlete
  career_row <- career_row + 1; # Go to next row in career_outcome dataset for writing data
}

# Calculate each player's career outcome from the season outcome data
career_row = 1 # Starting at top of career_outcome dataset for analysis
current_id <- career_outcome$Player[1] # Reset id to first player in dataset
while (career_row <= nrow(career_outcome)) {
  current_id <- career_outcome$Player[career_row]
  while (career_outcome$Player[career_row] == current_id && career_row <= nrow(career_outcome)) {
    if (career_outcome$Elite[career_row] >= 2 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'Elite'
      career_row <- career_row + 1
    } else if (career_outcome$Elite[career_row] == 1 && career_outcome$Player[career_row] == current_id) {
      career_outcome$All_Star[career_row] <- career_outcome$All_Star[career_row] + 1
    }
    
    if (career_outcome$All_Star[career_row] >= 2 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'All-Star'
      career_row <- career_row + 1
    } else if (career_outcome$All_Star[career_row] == 1 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Starter[career_row] <- career_outcome$Starter[career_row] + 1
    }
    
    if (career_outcome$Starter[career_row] >= 2 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'Starter'
      career_row <- career_row + 1
    } else if (career_outcome$Starter[career_row] == 1 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Rotation[career_row] <- career_outcome$Rotation[career_row] + 1
    }
    
    if (career_outcome$Rotation[career_row] >= 2 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'Rotation'
      career_row <- career_row + 1
    } else if (career_outcome$Rotation[career_row] == 1 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Roster[career_row] <- career_outcome$Roster[career_row] + 1
    }
    
    if (career_outcome$Roster[career_row] >= 2 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'Roster'
      career_row <- career_row + 1
    } else if ((career_outcome$Roster[career_row] == 1) || (career_outcome$Roster[career_row] == 0)) {
      career_outcome$Out_of_the_League[career_row] <- career_outcome$Out_of_the_League[career_row] + 1
    }
    
    if (career_outcome$Out_of_the_League[career_row] >= 1 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'Out of the League'
      career_row <- career_row + 1
    }
  }
}

# Sum number of players for each career outcome
career_counts <- data.frame(table(career_outcome$Outcome))

# Visualization
ggplot(career_counts, aes(y = Freq, x = Var1, fill = factor(Var1), label = Freq))+
  geom_bar(stat="identity",position="dodge") + xlab("Career Outcome") + 
  ylab("Number of Players") + scale_fill_discrete(name = "Career Outcome") + 
  ggtitle("Distribution for the Career Outcome of NBA Players Drafted in or prior to 2015") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5))

################################################################################

  # ANALYSIS #4: Distribution of Season Outcomes for Prediction Model
  # Calculating the distribution of season outcomes of players based on their 
  # distinct career outcome. Assuming four complete seasons, plus an additional 
  # three seasons are required to determine a distinct career outcome, 
  # only players drafted prior to 2015 will be considered for this analysis.
  # This data will be used in a prediction model to predict career outcomes.

# Create subset of all data from players drafted 2015 and prior
player_data2015 <- subset(player_data, draftyear <= 2015)

# Filter by specific team
#player_data2015 <- subset(player_data2015, nbateamid == 1610612752)

# Join awards and 1991-2015 player data
player_stats2015 <- right_join(awards, player_data2015, copy = FALSE)
player_stats2015 <- player_stats2015 %>% arrange(nbapersonid, season)
player_stats2015 <- player_stats2015 %>% mutate(season_outcome = NA)

# Season outcome logic for 1991-2015 data: assigns the six season outcome labels to each player's season
# 82/66 multiplier used for 2011 season game count, 82/72 multiplier used for 2019/20 seasons game count
row_number = 1 # Initialize loop
while (row_number <= nrow(player_stats2015)) {
  if (!is.na(player_stats2015$`All NBA First Team`[row_number]) &&
      player_stats2015$`All NBA First Team`[row_number] != 0 || !is.na(player_stats2015$`All NBA Second Team`[row_number]) &&
      player_stats2015$`All NBA Second Team`[row_number] != 0 || !is.na(player_stats2015$`All NBA Third Team`[row_number]) &&
      player_stats2015$`All NBA Third Team`[row_number] != 0 || !is.na(player_stats2015$`Defensive Player Of The Year_rk`[row_number]) &&
      player_stats2015$`Defensive Player Of The Year_rk`[row_number] != 0 || !is.na(player_stats2015$`Most Valuable Player_rk`[row_number]) &&
      player_stats2015$`Most Valuable Player_rk`[row_number] != 0) {
    player_stats2015$season_outcome[row_number] <- 'Elite'
  } else if (!is.na(player_stats2015$all_star_game[row_number]) &&
              player_stats2015$all_star_game[row_number] != 0) {
    player_stats2015$season_outcome[row_number] <- 'All-Star'
  } else if (((player_stats2015$games_start[row_number] >= 41 || 
              player_stats2015$mins[row_number] >= 2000) &&        
              player_stats2015$season[row_number] != (2011 || 2019 || 2020)) ||
              ((player_stats2015$games_start[row_number] >= (41 / (82/66)) || 
              player_stats2015$mins[row_number] >= round(2000 / (82/66), digits = 0)) && 
              player_stats2015$season[row_number] == 2011) ||
              ((player_stats2015$games_start[row_number] >= round(41 / (82/72), digits = 0) ||
              player_stats2015$mins[row_number] >= round(2000 / (82/72), digits = 0)) &&
              player_stats2015$season[row_number] == (2019 || 2020))) {
    player_stats2015$season_outcome[row_number] <- 'Starter'
  } else if ((player_stats2015$mins[row_number] >= 1000 && 
              player_stats2015$season[row_number] != (2011 || 2019 || 2020)) || 
              (player_stats2015$mins[row_number] >= round(1000 / (82/66), digits = 0) &&
              player_stats2015$season[row_number] == 2011) || 
              (player_stats2015$mins[row_number] >= round(1000 / (82/72), digits = 0) && 
              player_stats2015$season[row_number] == (2019 || 2020))) {
    player_stats2015$season_outcome[row_number] <- 'Rotation'
  } else if (player_stats2015$mins[row_number] >= 1) {
    player_stats2015$season_outcome[row_number] <- 'Roster'
  } else if (player_stats2015$mins[row_number] == 0) {
    player_stats2015$season_outcome[row_number] <- 'Out of the League'
  }
  row_number <- row_number + 1; # Go to next row in dataset
}

###
# Initialize a career outcome dataset for tallying with length of number of unique players
career_outcome <- data.frame("Player" = character(n_distinct(player_stats2015$nbapersonid)), "Elite" = numeric(n_distinct(player_stats2015$nbapersonid)), "All_Star" = numeric(n_distinct(player_stats2015$nbapersonid)), "Starter" = numeric(n_distinct(player_stats2015$nbapersonid)), "Rotation" = numeric(n_distinct(player_stats2015$nbapersonid)), "Roster" = numeric(n_distinct(player_stats2015$nbapersonid)), "Out_of_the_League" = numeric(n_distinct(player_stats2015$nbapersonid)), "Outcome" = character(n_distinct(player_stats2015$nbapersonid)))

# Initialize season tally starting conditions
row_number = 1 # Initialize loop condition
season_number = 1 # Initialize inner loop condition
current_id = player_stats2015$nbapersonid[1] # Initializes with first athlete in dataset
career_row = 1 # Initialize career outcome loop

# Tally up season outcomes
while (row_number <= nrow(player_stats2015)) { # Will scan entirety of player_stats2015
  career_outcome$Player[career_row] <- current_id
  while (player_stats2015$nbapersonid[row_number] == current_id) {
    while (season_number <= 4) { # Skip first four seasons of each player
      if (row_number > nrow(player_stats2015)) {
        break
      }
      if (player_stats2015$nbapersonid[row_number] == current_id) {
        season_number <- season_number + 1;
        row_number <- row_number + 1;
      } else { # If player does not exceed 4 seasons, no check to be taken
        break
      }
    } 
    if (row_number > nrow(player_stats2015)) {
      break
    }
    if (player_stats2015$season_outcome[row_number] == 'Elite' && 
        player_stats2015$nbapersonid[row_number] == current_id) {
      career_outcome$Elite[career_row] <- career_outcome$Elite[career_row] + 1
    } else if (player_stats2015$season_outcome[row_number] == 'All-Star' && 
               player_stats2015$nbapersonid[row_number] == current_id) {
      career_outcome$All_Star[career_row] <- career_outcome$All_Star[career_row] + 1
    } else if (player_stats2015$season_outcome[row_number] == 'Starter' && 
               player_stats2015$nbapersonid[row_number] == current_id) {
      career_outcome$Starter[career_row] <- career_outcome$Starter[career_row] + 1
    } else if (player_stats2015$season_outcome[row_number] == 'Rotation' && 
               player_stats2015$nbapersonid[row_number] == current_id) {
      career_outcome$Rotation[career_row] <- career_outcome$Rotation[career_row] + 1
    } else if (player_stats2015$season_outcome[row_number] == 'Roster' && 
               player_stats2015$nbapersonid[row_number] == current_id) {
      career_outcome$Roster[career_row] <- career_outcome$Roster[career_row] + 1
    } else if (player_stats2015$season_outcome[row_number] == 'Out of the League' && 
               player_stats2015$nbapersonid[row_number] == current_id) {
      career_outcome$Out_of_the_League[career_row] <- career_outcome$Out_of_the_League[career_row] + 1
    } else {
      career_outcome$Out_of_the_League[career_row] <- career_outcome$Out_of_the_League[career_row] + 1
      break
    }
    row_number <- row_number + 1; # Go to next row in player_stats2010
    if (row_number > nrow(player_stats2015)) {
      break
    }
  }
  if (row_number > nrow(player_stats2015)) {
    break
  }
  season_number = 1 # Resets season counter for next athlete
  current_id <- player_stats2015$nbapersonid[row_number]; # Switch to the next athlete
  career_row <- career_row + 1; # Go to next row in career_outcome dataset for writing data
}

# Make sure each player has actually played in 7 seasons
career_outcome$Sum = numeric(n_distinct(player_stats2015$nbapersonid))
career_row = 1
while (career_row <= nrow(career_outcome)) {
  career_outcome$Sum[career_row] <- career_outcome$Sum[career_row] + career_outcome$Elite[career_row]
  career_outcome$Sum[career_row] <- career_outcome$Sum[career_row] + career_outcome$All_Star[career_row]
  career_outcome$Sum[career_row] <- career_outcome$Sum[career_row] + career_outcome$Starter[career_row]
  career_outcome$Sum[career_row] <- career_outcome$Sum[career_row] + career_outcome$Rotation[career_row]
  career_outcome$Sum[career_row] <- career_outcome$Sum[career_row] + career_outcome$Roster[career_row]
  career_row <- career_row + 1
}

# Filter out players that haven't played 7+ seasons
career_outcome <- career_outcome %>% filter(Sum >= 3)
career_outcome <- subset(career_outcome, select = -Sum)

# Save original season stats for analysis after getting career outcomes
career_outcome_OG <- career_outcome

# Calculate each player's career outcome from the season outcome data
career_row = 1 # Starting at top of career_outcome dataset for analysis
current_id <- career_outcome$Player[1] # Reset id to first player in dataset
while (career_row <= nrow(career_outcome)) {
  current_id <- career_outcome$Player[career_row]
  while (career_outcome$Player[career_row] == current_id && career_row <= nrow(career_outcome)) {
    if (career_outcome$Elite[career_row] >= 2 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'Elite'
      career_row <- career_row + 1
    } else if (career_outcome$Elite[career_row] == 1 && career_outcome$Player[career_row] == current_id) {
      career_outcome$All_Star[career_row] <- career_outcome$All_Star[career_row] + 1
    }
    
    if (career_outcome$All_Star[career_row] >= 2 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'All-Star'
      career_row <- career_row + 1
    } else if (career_outcome$All_Star[career_row] == 1 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Starter[career_row] <- career_outcome$Starter[career_row] + 1
    }
    
    if (career_outcome$Starter[career_row] >= 2 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'Starter'
      career_row <- career_row + 1
    } else if (career_outcome$Starter[career_row] == 1 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Rotation[career_row] <- career_outcome$Rotation[career_row] + 1
    }
    
    if (career_outcome$Rotation[career_row] >= 2 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'Rotation'
      career_row <- career_row + 1
    } else if (career_outcome$Rotation[career_row] == 1 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Roster[career_row] <- career_outcome$Roster[career_row] + 1
    }
    
    if (career_outcome$Roster[career_row] >= 2 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'Roster'
      career_row <- career_row + 1
    } else if ((career_outcome$Roster[career_row] == 1) || (career_outcome$Roster[career_row] == 0)) {
      career_outcome$Out_of_the_League[career_row] <- career_outcome$Out_of_the_League[career_row] + 1
    }
    if (career_row > nrow(career_outcome)) {
      break
    }
    if (career_outcome$Out_of_the_League[career_row] >= 1 && career_outcome$Player[career_row] == current_id) {
      career_outcome$Outcome[career_row] <- 'Out of the League'
      career_row <- career_row + 1
    }
  }
}

# Append calculated career outcome to original season outcome data
career_outcome_OG$Outcome <- career_outcome$Outcome


# Predictive model building (taking a statistical approach)
# Get individual season outcome counts for each career outcome bucket
stat_count2015 <- career_outcome_OG %>% group_by(Outcome) %>% summarise(Elite = sum(Elite), All_Star = sum(All_Star), Starter = sum(Starter), Rotation = sum(Rotation), Roster = sum(Roster))

# Tidy for plotting purposes
stat_count_tidy <- stat_count2015 %>%
  pivot_longer(cols=c("Elite", "All_Star", "Starter", "Rotation", "Roster"), names_to = "Variable", values_to = "Value")

# This gives the sum of all season outcomes for players from each career outcome bucket - can be used to find percentages for each season outcome
stat_count_total <- stat_count_tidy %>% group_by(Outcome) %>% summarise(Sums = sum(Value))

# Calculate percentages of each season outcome bucket to total of season outcomes for a specific career outcome bucket
stat_count_tidy_pcts <- right_join(stat_count_tidy, stat_count_total, copy = NULL)
stat_count_tidy_pcts$Percent <- stat_count_tidy_pcts$Value / stat_count_tidy_pcts$Sums

# Visualization of the distribution of season outcomes throughout each career outcome bucket
ggplot(data = stat_count_tidy_pcts, aes(Outcome, Percent, fill = Variable)) + 
  geom_bar(stat = "identity", position = "dodge")  + xlab("Career Outcomes") + 
  ylab("Percentage of all season outcomes") + 
  scale_fill_discrete(name = "Season Outcome") + 
  ggtitle("Distribution of Season Outcomes within Each Career Outcome Bucket")

################################################################################

  # PREDICTION MODEL: Predicting Career Outcomes of New Players
  # This in-progress prediction model is being developed using the season
  # outcome to career outcomes percentages calculated in the previous analysis, 
  # and will predict the career outcome of all players who were drafted between 
  # 2018 and 2021.

# Retrieve 2018-2021 data for predictions
player_data2018 <- subset(player_data, draftyear >= 2018)

# Join awards and 1991-2015 player data
player_stats2018 <- right_join(awards, player_data2018, copy = FALSE)
player_stats2018 <- player_stats2018 %>% arrange(nbapersonid, season)
player_stats2018 <- player_stats2018 %>% mutate(season_outcome = NA)

# Season outcome logic for 2018-2021 data: assigns the six season outcome labels to each player's season
# 82/66 multiplier used for 2011 season game count, 82/72 multiplier used for 2019/20 seasons game count
row_number = 1 # Initialize loop
while (row_number <= nrow(player_stats2018)) {
  if (!is.na(player_stats2018$`All NBA First Team`[row_number]) &&
      player_stats2018$`All NBA First Team`[row_number] != 0 || 
      !is.na(player_stats2018$`All NBA Second Team`[row_number]) &&
      player_stats2018$`All NBA Second Team`[row_number] != 0 || 
      !is.na(player_stats2018$`All NBA Third Team`[row_number]) &&
      player_stats2018$`All NBA Third Team`[row_number] != 0 || 
      !is.na(player_stats2018$`Defensive Player Of The Year_rk`[row_number]) &&
      player_stats2018$`Defensive Player Of The Year_rk`[row_number] != 0 || 
      !is.na(player_stats2018$`Most Valuable Player_rk`[row_number]) &&
      player_stats2018$`Most Valuable Player_rk`[row_number] != 0) {
    player_stats2018$season_outcome[row_number] <- 'Elite'
  } else if (!is.na(player_stats2018$all_star_game[row_number]) &&
             player_stats2018$all_star_game[row_number] != 0) {
    player_stats2018$season_outcome[row_number] <- 'All-Star'
  } else if (((player_stats2018$games_start[row_number] >= 41 || 
              player_stats2018$mins[row_number] >= 2000) &&        
              player_stats2018$season[row_number] != (2011 || 2019 || 2020)) ||
              ((player_stats2018$games_start[row_number] >= (41 / (82/66)) || 
              player_stats2018$mins[row_number] >= round(2000 / (82/66), digits = 0)) && 
              player_stats2018$season[row_number] == 2011) ||                           
              ((player_stats2018$games_start[row_number] >= round(41 / (82/72), digits = 0) || 
              player_stats2018$mins[row_number] >= round(2000 / (82/72), digits = 0)) &&
              player_stats2018$season[row_number] == (2019 || 2020))) {
    player_stats2018$season_outcome[row_number] <- 'Starter'
  } else if ((player_stats2018$mins[row_number] >= 1000 && 
              player_stats2018$season[row_number] != (2011 || 2019 || 2020)) || 
              (player_stats2018$mins[row_number] >= round(1000 / (82/66), digits = 0) &&
              player_stats2018$season[row_number] == 2011) || 
              (player_stats2018$mins[row_number] >= round(1000 / (82/72), digits = 0) && 
              player_stats2018$season[row_number] == (2019 || 2020))) {
    player_stats2018$season_outcome[row_number] <- 'Rotation'
  } else if (player_stats2018$mins[row_number] >= 1) {
    player_stats2018$season_outcome[row_number] <- 'Roster'
  } else if (player_stats2018$mins[row_number] == 0) {
    player_stats2018$season_outcome[row_number] <- 'Out of the League'
  }
  row_number <- row_number + 1; # Go to next row in dataset
}

###
# Initialize a career outcome dataset for tallying with length of number of unique players
career_outcome <- data.frame("Player" = character(n_distinct(player_stats2018$nbapersonid)), "Elite" = numeric(n_distinct(player_stats2018$nbapersonid)), "All_Star" = numeric(n_distinct(player_stats2018$nbapersonid)), "Starter" = numeric(n_distinct(player_stats2018$nbapersonid)), "Rotation" = numeric(n_distinct(player_stats2018$nbapersonid)), "Roster" = numeric(n_distinct(player_stats2018$nbapersonid)), "Out_of_the_League" = numeric(n_distinct(player_stats2018$nbapersonid)), "Outcome" = character(n_distinct(player_stats2018$nbapersonid)))

# Initialize season tally starting conditions
row_number = 1 # Initialize loop condition
season_number = 1 # Initialize inner loop condition
current_id = player_stats2018$nbapersonid[1] # Initializes with first athlete in dataset
career_row = 1 # Initialize career outcome loop

# Tally up season outcomes - this is solely for providing predictions for the players mentioned in the question prompt
while (row_number <= nrow(player_stats2018)) { # Will scan entirety of player_stats2015
  career_outcome$Player[career_row] <- current_id
  while (player_stats2018$nbapersonid[row_number] == current_id) {
    if (player_stats2018$season_outcome[row_number] == 'Elite' && player_stats2018$nbapersonid[row_number] == current_id) {
      career_outcome$Elite[career_row] <- career_outcome$Elite[career_row] + 1
    } else if (player_stats2018$season_outcome[row_number] == 'All-Star' && 
               player_stats2018$nbapersonid[row_number] == current_id) {
      career_outcome$All_Star[career_row] <- career_outcome$All_Star[career_row] + 1
    } else if (player_stats2018$season_outcome[row_number] == 'Starter' && 
               player_stats2018$nbapersonid[row_number] == current_id) {
      career_outcome$Starter[career_row] <- career_outcome$Starter[career_row] + 1
    } else if (player_stats2018$season_outcome[row_number] == 'Rotation' && 
               player_stats2018$nbapersonid[row_number] == current_id) {
      career_outcome$Rotation[career_row] <- career_outcome$Rotation[career_row] + 1
    } else if (player_stats2018$season_outcome[row_number] == 'Roster' && 
               player_stats2018$nbapersonid[row_number] == current_id) {
      career_outcome$Roster[career_row] <- career_outcome$Roster[career_row] + 1
    } else if (player_stats2018$season_outcome[row_number] == 'Out of the League' && 
               player_stats2018$nbapersonid[row_number] == current_id) {
      career_outcome$Out_of_the_League[career_row] <- career_outcome$Out_of_the_League[career_row] + 1
    } else {
      career_outcome$Out_of_the_League[career_row] <- career_outcome$Out_of_the_League[career_row] + 1
      break
    }
    row_number <- row_number + 1; # Go to next row in player_stats2010
    if (row_number > nrow(player_stats2018)) {
      break
    }
  }
  if (row_number > nrow(player_stats2018)) {
    break
  }
  season_number = 1 # Resets season counter for next athlete
  current_id <- player_stats2018$nbapersonid[row_number]; # Switch to the next athlete
  career_row <- career_row + 1; # Go to next row in career_outcome dataset for writing data
}

player_names_2018 <- data.frame(Player = player_stats2018$nbapersonid, name = player_stats2018$player)
career_outcome <- merge(career_outcome, player_names_2018, by = "Player", all = TRUE)
career_outcome <- distinct(career_outcome, Player, .keep_all = TRUE)

################################################################################

  # ANALYSIS 5: Predicting Offensive Rebound Percentage using Historic Data
  # This analysis will use historic NBA to find the average rebound success rate
  # for all NBA teams in the 2022-23 season. Additionally, this model will be
  # used to statistical data to predict what the rebounding percentage will be 
  # for the New York Knicks' and Brooklyn Nets' 71st game.
  # Data used is from the 2022-23 season, but only the first 70 of 82 games are 
  # analyzed.

# Determine which game to provide prediction for
rebounding_data_NBA <- subset(rebounding_data, game_number <= 70)

# Calculate rebounding percent based on mean of percentages
rebounding_percent <- mean(rebounding_data_NBA$oreb_pct) # Yields 0.27776 or 27.8%

# Calculate rebounding percent based on the sum of rebounds / sum of chances
rebounding_sum <- summarise(rebounding_data_NBA, offensive_rebounds, off_rebound_chances) %>% 
  summarise_all(funs(sum))
rebounding_percent_multi_NBA <- rebounding_sum$offensive_rebounds / rebounding_sum$off_rebound_chances # Yields 0.28092 or 28.1%

# Visualization
ggplot(data = rebounding_data_NBA, aes(x = game_number, y = oreb_pct, group = 1, label = game_number)) +
  geom_line(arrow = arrow())+
  geom_point() + xlab("Game Number (of 1-70)") + ylab("Percentage of Successful Rebounds") + 
  ggtitle("Percentage of NBA Rebounds during Games 1-70 of the 2022-23 Season") + 
  geom_hline(yintercept = rebounding_percent_multi_NBA, color = "red") + 
  annotate("text", x = 45, y = (rebounding_percent_multi_NBA + 0.23), label = "NBA rebound percent average = 28.1% (28.092%)") + 
  theme(legend.position="none")

#Separate out only New York Knicks games 1-70 data
rebounding_data_NYK <- subset(rebounding_data, team == 'NYK')
rebounding_data_NYK <- subset(rebounding_data_NYK, game_number <= 70)

# Calculate rebounding percent based on mean of percentages
rebounding_percent <- mean(rebounding_data_NYK$oreb_pct) # Yields 0.31717 or 31.7%

# Calculate rebounding percent based on the sum of rebounds / sum of chances
rebounding_sum <- summarise(rebounding_data_NYK, offensive_rebounds, off_rebound_chances) %>% 
  summarise_all(funs(sum))
rebounding_percent_multi_NYK <- rebounding_sum$offensive_rebounds / rebounding_sum$off_rebound_chances # Yields 0.31967 or 32%

# Visualization
ggplot(data = rebounding_data_NYK, aes(x = game_number, y = oreb_pct, group = 1, label = game_number)) +
  geom_line(arrow = arrow())+
  geom_point() + xlab("Game Number (of 1-70)") + ylab("Percentage of Successful Rebounds") + 
  ggtitle("Percentage of NYK Rebounds during Games 1-70 of the 2022-23 Season") + 
  geom_hline(yintercept = rebounding_percent_multi_NYK, color = "red") + 
  annotate("text", x = 27, y = (rebounding_percent_multi_NYK + 0.15), label = "NYK rebound percent average = 32% (31.967%)") + 
  geom_segment(aes(x = 15, y = 0.325, xend = 17.5, yend = 0.46, color = 'orange'), arrow = arrow(length = unit(0.5, "cm"))) + 
  theme(legend.position="none") + geom_smooth(method='lm', formula = y ~ poly(x,8), linewidth = 1) + 
  geom_segment(aes(x = 33, y = 0.33, xend = 30.5, yend = 0.18, color = 'red'), arrow = arrow(length = unit(0.5, "cm"))) + 
  theme(legend.position="none") + 
  annotate("text", x = 24, y = (rebounding_percent_multi_NYK - 0.15), label = "Polynomial linear regression of rebound % trend")

#Separate out only Brooklyn Nets games 1-70 data
rebounding_data_BKN <- subset(rebounding_data, team == 'BKN')
rebounding_data_BKN <- subset(rebounding_data_BKN, game_number <= 70)

# Calculate rebounding percent based on mean of percentages
rebounding_percent <- mean(rebounding_data_BKN$oreb_pct) # Yields 0.21894 or 21.9%

# Calculate rebounding percent based on the sum of rebounds / sum of chances
rebounding_sum <- summarise(rebounding_data_BKN, offensive_rebounds, off_rebound_chances) %>% 
  summarise_all(funs(sum))
rebounding_percent_multi_BKN <- rebounding_sum$offensive_rebounds / rebounding_sum$off_rebound_chances # Yields 0.22295 or 22.3%

# Visualization
ggplot(data = rebounding_data_BKN, aes(x = game_number, y = oreb_pct, group = 1, label = game_number)) +
  geom_line(arrow = arrow())+
  geom_point() + xlab("Game Number (of 1-70)") + ylab("Percentage of Successful Rebounds") + 
  ggtitle("Percentage of BKN Rebounds during Games 1-70 of the 2022-23 Season") + 
  geom_hline(yintercept = rebounding_percent_multi_BKN, color = "red") + 
  annotate("text", x = 50, y = (rebounding_percent_multi_BKN + 0.17), label = "BKN rebound percent average = 22.3% (22.295%)") + 
  geom_segment(aes(x = 37, y = 0.24, xend = 39.5, yend = 0.37, color = 'blue'), arrow = arrow(length = unit(0.5, "cm"))) + 
  theme(legend.position="none") + geom_smooth(method='lm', formula = y ~ poly(x,8), linewidth = 1) + 
  geom_segment(aes(x = 27, y = 0.18, xend = 29.5, yend = 0.05, color = 'red'), arrow = arrow(length = unit(0.5, "cm"))) + 
  theme(legend.position="none") + 
  annotate("text", x = 24, y = (rebounding_percent_multi_BKN - 0.20), label = "Polynomial linear regression of rebound % trend")
