#----Open Files----

#clean environment
rm(list = ls())

#load packages
library(tidyverse)
library(StatsBombR)
library(SBpitch)

#Open Premier League event Data for 2015/16 season
match_event_data <- read_rds("prem_event_data_15-16.rds")

#Select Player and Match
player_analysed <- "Riyad Mahrez"
match_analysed <- 3754290

#----Create Passing Map----

#Filter to only include passes from correct match and player
passes_map_data <- match_event_data %>%
  filter(player.name == player_analysed & type.name == "Pass" & match_id == match_analysed) %>%
  mutate(pass.outcome = if_else(is.na(pass.outcome.name), "Successful", "Unsuccessful"),
        pass.outcome = factor(pass.outcome, levels = c("Successful", "Unsuccessful")),
        pass.goal_assist = if_else(is.na(pass.goal_assist), FALSE, pass.goal_assist))

#Plot pitch
create_Pitch(goaltype = "box",
             grass_colour = "#224C56", 
             line_colour = "#B3CED9", 
             background_colour = "#224C56", 
             goal_colour = "#15393D") +
  # Plot successful and unsuccessful passes
  geom_segment(data = passes_map_data %>% filter(!pass.goal_assist),
               aes(x = location.x, y = location.y,
                   xend = pass.end_location.x, yend = pass.end_location.y,
                   colour = pass.outcome),
               lineend = "round", linewidth = 0.5,
               arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) +
  #Change assists to gold
  geom_segment(data = passes_map_data %>% filter(pass.goal_assist),
               aes(x = location.x, y = location.y,
                   xend = pass.end_location.x, yend = pass.end_location.y,colour = "Assist"),
               lineend = "round", linewidth = 0.5,
               arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) +
  #Add Labels
  labs(title = player_analysed, subtitle = "Passing Map for Leicester City vs Man City 02/01/2016", colour = "Pass Outcome") +
  scale_y_reverse() +
  #Scale to correct size as done by Statsbomb
  coord_fixed(ratio = 105/100) +
  #Select correct colours for legend
  scale_colour_manual(values = c("Successful" = "green", "Unsuccessful" = "red", "Assist" = "gold")) +
  #Make whole plot the same colour with white text
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#224C56", color = NA),
    panel.background = element_rect(fill = "#224C56", color = NA),
    legend.background = element_rect(fill = "#224C56", color = NA),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.title = element_text(color = "white", hjust = 0.5, size = 16),
    plot.subtitle = element_text(color = "white", hjust = 0.5, size = 12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank())

