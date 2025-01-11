#----Open Files----

#clean environment
rm(list = ls())

#load packages
library(tidyverse)
library(StatsBombR)
library(SBpitch)

#Open Premier League event Data for 2015/16 season
match_event_data <- read_rds("prem_event_data_15-16.rds")

#Select Player
player_analysed <- "Riyad Mahrez"

#----Filter Data----

#Filter to only include passes from correct player
shot_map_data <- match_event_data %>%
  filter(player.name == player_analysed & type.name == "Shot" & !is.na(shot.end_location.z))

#----Plot Goal----

#Function to plot a goal
plot_goal <- function(background_colour, line_colour, goal_colour) {
  
  #Create clean plot theme
  theme_blankGoal <- function(size = 12) { 
    theme(axis.text.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.length = unit(0, "lines"), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          legend.background = element_rect(fill = background_colour, colour = NA), 
          legend.key = element_rect(fill = background_colour), 
          legend.key.size = unit(1.2, "lines"), 
          legend.text = element_text(size = size), 
          legend.title = element_text(size = size, face = "bold", hjust = 0),
          strip.background = element_rect(fill = background_colour, linewidth = 0.5),
          panel.background = element_rect(fill = background_colour, colour = background_colour), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.background = element_blank(), 
          plot.margin = unit(c(0, 0, 0, 0), "lines"), 
          plot.title = element_text(size = size * 1.2))}
  
  #Goal dimensions
  goal_width <- c(36, 44)
  goal_height <- 2.67
  goal_depth <- 0
  
  #Plot goal
  plot <- ggplot() +
    theme_blankGoal() +
    #Plot goal box
    geom_rect(aes(xmin = goal_width[1], xmax = goal_width[2], ymin = goal_depth, ymax = goal_height), fill = background_colour, colour = line_colour) +
    #Add posts
    geom_segment(aes(x = goal_width[1], y = goal_depth, xend = goal_width[1], yend = goal_height), colour = goal_colour, linewidth = 1.5) +
    geom_segment(aes(x = goal_width[2], y = goal_depth, xend = goal_width[2], yend = goal_height), colour = goal_colour, linewidth = 1.5) +
    #Add crossbar
    geom_segment(aes(x = goal_width[1], y = goal_height, xend = goal_width[2], yend = goal_height), colour = goal_colour, linewidth = 1.5) +
    #Set limits for plot
    xlim(c(34, 46)) +
    ylim(c(-0.2, 3.5))
  
  return(plot)
}

#----Plot Shot Map----

#Use function to plot goal inputting colours
plot_blank_goal <- plot_goal("#224C56", "white", "white")

#Plot Goal with shots mapped onto it
plot_blank_goal +
  geom_point(data = shot_map_data,
             aes(x = shot.end_location.y, 
                 y = shot.end_location.z, 
                 size = shot.statsbomb_xg, 
                 color = ifelse(shot.outcome.name == "Goal", "Goal", "Unsuccessful Shot"))) +
  #Change colours according to shot outcome
  scale_color_manual(values = c("Goal" = "gold", "Unsuccessful Shot" = "red"),
                     name = "Shot Outcome",
                     guide = guide_legend(override.aes = list(size = 6))) +
  #Change size for xG of shot
  scale_size_continuous(range = c(1.5, 8), limits = c(0, 1), breaks = c(0, 0.5, 1),
                        labels = c("0.0", "0.5", "1.0"),
                        name = "xG",
                        guide = guide_legend(override.aes = list(color = "white"))) +
  coord_fixed(ratio = 105/100) +
  #Add titles
  labs(title = player_analysed, subtitle = "Shot Map, Premier League, 2015/16") +
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
