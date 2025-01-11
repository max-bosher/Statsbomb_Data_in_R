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
player_analysed <- "N'Golo Kanté"
match_analysed <- 3754343

#----Plot Defensive Action Map----

#Filter data to only include events from selected player in selected match
defensive_action_map_data <- match_event_data %>%
  filter(player.name == player_analysed & match_id == match_analysed) %>%
  #Filter data to only include defensive actions
  filter(`50_50.outcome.name` == "Won" | `50_50.outcome.name` == "Success To Team" |
           (type.name == "Ball Recovery" & is.na(ball_recovery.recovery_failure)) |
           type.name == "Block" |
           type.name == "Clearance" |
           duel.type.name == "Tackle" & (duel.outcome.name == "Won" | duel.outcome.name == "Success" |
                                           duel.outcome.name == "Success In Play" | duel.outcome.name == "Success Out") |
           type.name == "Interception" & (interception.outcome.name == "Won" | interception.outcome.name == "Success" |
                                            interception.outcome.name == "Success In Play" | interception.outcome.name == "Success Out"))

#Plot pitch
create_Pitch(goaltype = "box",
             grass_colour = "#224C56", 
             line_colour = "white", 
             background_colour = "#224C56", 
             goal_colour = "#15393D") +
  #Plot defensive actions with different colours for each action
  geom_point(data = defensive_action_map_data,
             aes(x = location.x, y = location.y, colour = type.name), 
             size = 3) +
  #Add arrow to show attacking direction
  annotate("segment", 
           x = 45, xend = 75, 
           y = -5, yend = -5, 
           colour = "white", 
           arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  #Add titles
  labs(title = player_analysed, 
       subtitle = "Defensive Actions, Premier League, Liverpool vs Leicester City 26/12/2015", 
       colour = "Defensive Action Type") +
  scale_y_reverse() +
  #Scale to correct size as done by StatsBomb
  coord_fixed(ratio = 105/100) +
  #Make whole plot the same colour with white text
  theme_minimal(base_size = 16) +
  theme(plot.background = element_rect(fill = "#224C56", color = NA),
        panel.background = element_rect(fill = "#224C56", color = NA),
        legend.background = element_rect(fill = "#224C56", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        plot.title = element_text(color = "white", hjust = 0.5, size = 22),
        plot.subtitle = element_text(color = "white", hjust = 0.5, size = 12),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
