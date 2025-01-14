#----Open Files----

#clean environment
rm(list = ls())

#load packages
library(tidyverse)
library(StatsBombR)
library(SBpitch)
library(flextable)
library(officer)
library(patchwork)
library(cowplot)
library(magick)
library(gridExtra)

#Open Premier League event Data for 2015/16 season
match_event_data <- read_rds("prem_event_data_15-16.rds")

#Open season long stats calculated in other code
season_stats <- read.csv("Prem_Stats_2015-16_for_season.csv")

#Open stats for each match calculated in other code
stats_per_match <- read.csv("Prem_Stats_2015-16_per_match.csv")

#create df that contains match detials designed so you can find desired match_id
match_ids <- read_rds("prem_matches_15-16.rds") %>%
  select(match_id, match_date, home_team = home_team.home_team_name, away_team = away_team.away_team_name)

#Select Player and Match
player_analysed <- "Dele Alli"
match_analysed <- 3754179

#----Combined Plot----

#Create new variable to determine what type of event each point is
pass_data <- match_event_data %>%
  filter(player.name == player_analysed & type.name == "Pass" & match_id == match_analysed & is.na(pass.outcome.name)) %>%
  mutate(action_type = if_else(is.na(pass.goal_assist), "Pass", "Assist"))
carry_data <- match_event_data %>%
  filter(player.name == player_analysed & type.name == "Carry" & match_id == match_analysed) %>%
  mutate(carry_distance = sqrt((carry.end_location.x - location.x)^2 + (carry.end_location.y - location.y)^2)) %>%
  filter(carry_distance >= 3) %>%
  mutate(action_type = "Carry")
defensive_action_data <- match_event_data %>%
  filter(player.name == player_analysed & match_id == match_analysed) %>%
  filter(`50_50.outcome.name` == "Won" | `50_50.outcome.name` == "Success To Team" |
           (type.name == "Ball Recovery" & is.na(ball_recovery.recovery_failure)) |
           type.name == "Block" |
           type.name == "Clearance" |
           duel.type.name == "Tackle" & (duel.outcome.name == "Won" | duel.outcome.name == "Success" |
                                           duel.outcome.name == "Success In Play" | duel.outcome.name == "Success Out") |
           type.name == "Interception" & (interception.outcome.name == "Won" | interception.outcome.name == "Success" |
                                            interception.outcome.name == "Success In Play" | interception.outcome.name == "Success Out")) %>%
  mutate(action_type = "Defensive Action")
shot_data <- match_event_data %>%
  filter(player.name == player_analysed & match_id == match_analysed & type.name == "Shot") %>%
  mutate(action_type = if_else(shot.outcome.name == "Goal", "Goal", "Unsuccessful Shot"))

#Combine all data frames
combined_data <- bind_rows(
  pass_data %>% mutate(event_xend = pass.end_location.x, event_yend = pass.end_location.y),
  carry_data %>% mutate(event_xend = carry.end_location.x, event_yend = carry.end_location.y),
  defensive_action_data %>% mutate(event_xend = NA, event_yend = NA),
  shot_data %>% mutate(event_xend = NA, event_yend = NA))

#Define all possible action types
action_types <- c("Pass", "Assist", "Carry", "Defensive Action", "Goal", "Unsuccessful Shot")

#Add dummy rows for missing action types so that all items are always in legend
dummy_data <- tibble(
  location.x = NA,
  location.y = NA,
  event_xend = NA,
  event_yend = NA,
  shot.statsbomb_xg = NA,
  action_type = action_types)

#Combine real data with dummy data
combined_data <- combined_data %>%
  mutate(action_type = factor(action_type, levels = action_types)) %>%
  bind_rows(dummy_data)

#Plot the combined data
events_plot <- create_Pitch(goaltype = "box",
                              grass_colour = "#224C56", 
                              line_colour = "white", 
                              background_colour = "#224C56", 
                              goal_colour = "#15393D") +
  #Plot passes as solid arrows and carries as dashed arrows
  geom_segment(data = combined_data %>% filter(action_type %in% c("Pass", "Assist")),
               aes(x = location.x, y = location.y, xend = event_xend, yend = event_yend, colour = action_type),
               lineend = "round", linewidth = 0.5,
               arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "closed")) +
  geom_segment(data = combined_data %>% filter(action_type == "Carry"),
               aes(x = location.x, y = location.y, xend = event_xend, yend = event_yend, colour = action_type),
               lineend = "round", linewidth = 0.5, linetype = "dashed",
               arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "closed")) +
  #Plot defensive actions
  geom_point(data = combined_data %>% filter(action_type == "Defensive Action"),
             aes(x = location.x, y = location.y, colour = action_type), size = 4) +
  #Plot shots with xG determining size
  geom_point(data = combined_data %>% filter(action_type %in% c("Goal", "Unsuccessful Shot")),
             aes(x = location.x, y = location.y, size = shot.statsbomb_xg, colour = action_type)) +
  #Add arrow to show attacking direction
  annotate("segment", 
           x = 50, xend = 70, 
           y = -3, yend = -3, 
           colour = "white", 
           arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  annotate("text", 
           x = 60, y = -5, 
           label = "Attack", 
           colour = "white", 
           size = 5, 
           hjust = 0.5,
           angle = 90) +
  labs(colour = "Event Type") +
  #Change colours for each event type
  scale_colour_manual(
    values = c("Pass" = "grey", "Assist" = "gold", 
               "Carry" = "grey","Defensive Action" = "cyan", 
               "Goal" = "gold", "Unsuccessful Shot" = "red"),
    guide = guide_legend(override.aes = list(size = 5))) +
  #Change size for xG of shot
  scale_size_continuous(range = c(1.5, 8), limits = c(0, 1), breaks = c(0, 0.5, 1),
                        labels = c("0.0", "0.5", "1.0"),
                        name = "xG",
                        guide = guide_legend(override.aes = list(color = "white"))) +
  #Add theme to stay consistent with the rest of the plot
  theme_minimal(base_size = 16) +
  theme(plot.background = element_rect(fill = "#224C56", color = NA),
        panel.background = element_rect(fill = "#224C56", color = NA),
        legend.background = element_rect(fill = "#224C56", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        plot.title = element_text(color = "white", hjust = 0.5, size = 22),
        plot.subtitle = element_text(color = "white", hjust = 0.5, size = 16),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  #keep pitch scale correct as done by Statsbomb
  coord_fixed(ratio = 105/100) +
  coord_flip()

#----Shot Outcome Map----

#Filter data to only include shots from choosen player and match
shot_data <- match_event_data %>%
  filter(player.name == player_analysed & match_id == match_analysed & type.name == "Shot") %>%
  #create new variable to show if the shot resulted in a goal
  mutate(event_name = if_else(shot.outcome.name == "Goal", "Goal", "Unsuccessful Shot"))

#Filter to only include passes from correct player
shot_outcome_data <- shot_data %>%
  filter(!is.na(shot.end_location.z)) %>%
  mutate(shot.end_location.y = pmin(pmax(shot.end_location.y, 34), 46),
         shot.end_location.z = pmin(pmax(shot.end_location.z, -0.2), 3.5))

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
  
  #Plot Limits
  width <- c(34,46)
  height <- c(0, 3.5)
  
  #Gridline dimensions
  gridlines_horizontal <- data.frame(y = seq(goal_depth, goal_height, length.out = 5), 
                                     x_start = goal_width[1], x_end = goal_width[2])
  gridlines_vertical <- data.frame(x = seq(goal_width[1], goal_width[2], length.out = 7), 
                                   y_start = goal_depth, y_end = goal_height)
  
  #Plot goal
  plot <- ggplot() +
    theme_blankGoal() +
    #Plot goal box
    geom_rect(aes(xmin = goal_width[1], xmax = goal_width[2], ymin = goal_depth, ymax = goal_height), 
              fill = background_colour, colour = line_colour) +
    #Add horizontal gridlines
    geom_segment(data = gridlines_horizontal, 
                 aes(x = x_start, xend = x_end, y = y, yend = y), 
                 colour = "grey", linetype = "dashed") +
    #Add vertical gridlines
    geom_segment(data = gridlines_vertical, 
                 aes(x = x, xend = x, y = y_start, yend = y_end), 
                 colour = "grey", linetype = "dashed") +
    #Add grey border around plot
    geom_rect(aes(xmin = width[1], xmax = width[2], ymin = height[1], ymax = height[2]), 
              fill = NA, colour = "grey", linewidth = 0.5) +
    #Add posts
    geom_segment(aes(x = goal_width[1], y = goal_depth, xend = goal_width[1], yend = goal_height), 
                 colour = goal_colour, linewidth = 1.5) +
    geom_segment(aes(x = goal_width[2], y = goal_depth, xend = goal_width[2], yend = goal_height), 
                 colour = goal_colour, linewidth = 1.5) +
    #Add crossbar
    geom_segment(aes(x = goal_width[1], y = goal_height, xend = goal_width[2], yend = goal_height), 
                 colour = goal_colour, linewidth = 1.5) +
    #Add white line along the bottom
    geom_segment(aes(x = width[1], y = goal_depth, xend = width[2], yend = goal_depth), 
                 colour = "white", linewidth = 1) +
    # Set limits for plot
    xlim(c(34, 46)) +
    ylim(c(-0.2, 3.5))
  
  return(plot)
}

#Use function to plot goal inputting colours
plot_blank_goal <- plot_goal("#224C56", "white", "white")

#Plot Goal with shots mapped onto it
shot_outcome_plot <- plot_blank_goal +
  geom_point(data = shot_outcome_data,
             aes(x = shot.end_location.y, 
                 y = shot.end_location.z, 
                 size = shot.statsbomb_xg, 
                 color = event_name)) +
  labs(title = "Shot Outcome Map") +
  #Change colours according to shot outcome
  scale_color_manual(values = c("Goal" = "gold", "Unsuccessful Shot" = "red"),
                     name = "Shot Outcome",
                     guide = guide_legend(override.aes = list(size = 6))) +
  #Change size for xG of shot
  scale_size_continuous(range = c(1.5, 8), limits = c(0, 1), breaks = c(0, 0.5, 1),
                        labels = c("0.0", "0.5", "1.0"),
                        name = "xG",
                        guide = guide_legend(override.aes = list(color = "white"))) +
  #keep pitch scale correct as done by Statsbomb
  coord_fixed(ratio = 105/100) +
  #Make whole plot the same colour with white text
  theme_minimal(base_size = 16) +
  theme(plot.background = element_rect(fill = "#224C56", color = NA),
        panel.background = element_rect(fill = "#224C56", color = NA),
        legend.background = element_rect(fill = "#224C56", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        plot.title = element_text(color = "white", hjust = 0.5, size = 16),
        plot.subtitle = element_text(color = "white", hjust = 0.5, size = 12),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

#----Heatmap----

#Filter to only include passes from correct player
heatmap_data <- match_event_data %>%
  filter(player.name == player_analysed & match_id == match_analysed) %>%
  filter(location.x >=0 & location.x <= 120) %>%
  filter(location.y >=0 & location.y <= 80)

#function to plot pitch so it can be added after heatmap is created
#needed as create_pitch function can only be used before any data is put on top
plot_pitch_markings <- function(line_colour) {
  
  #Pitch dimensions
  ymin <- 0
  ymax <- 80
  xmin <- 0
  xmax <- 120
  
  #Position of pitch features
  boxEdgeDef <- 18
  boxEdgeOff <- 102
  halfwayline <- 60
  sixYardDef <- 6
  sixYardOff <- 114
  penSpotDef <- 12
  penSpotOff <- 108
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  sixYardLeft <- 30
  sixYardRight <- 50
  goalPostLeft <- 36
  goalPostRight <- 44
  CentreSpot <- 40
  centreCirle_d <- 20
  
  #Function to plot circles
  circleFun <- function(center = c(0, 0), diameter = 1, npoints = 100) {
    r <- diameter / 2
    tt <- seq(0, 2 * pi, length.out = npoints)
    data.frame(x = center[1] + r * cos(tt), y = center[2] + r * sin(tt))
  }
  
  #plot centre circle
  centre_circle <- circleFun(c(halfwayline, CentreSpot), 20, npoints = 100)
  
  #Plot pitch marking
  pitch_markings <- list(geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "transparent", colour = line_colour),
                         geom_rect(aes(xmin = xmin, xmax = boxEdgeDef, ymin = boxEdgeLeft, ymax = boxEdgeRight), fill = "transparent", colour = line_colour),
                         geom_rect(aes(xmin = boxEdgeOff, xmax = xmax, ymin = boxEdgeLeft, ymax = boxEdgeRight), fill = "transparent", colour = line_colour),
                         geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight), fill = "transparent", colour = line_colour),
                         geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight), fill = "transparent", colour = line_colour),
                         geom_point(aes(x = penSpotDef , y = CentreSpot), colour = line_colour, size = 0.75),
                         geom_point(aes(x = penSpotOff , y = CentreSpot), colour = line_colour, size = 0.75),
                         geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax), colour = line_colour),
                         geom_path(data = centre_circle, aes(x = x, y = y), colour = line_colour))
  
  #Add circles and semicircles
  pitch_markings <- c(pitch_markings,list(annotate("path", x = 12 + 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
                                                   y = 40 + 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
                                                   col = line_colour),
                                          annotate("path", x = (120 - 12) - 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
                                                   y = 40 + 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
                                                   col = line_colour)))
  
  #Add goal markings
  pitch_markings <- c(pitch_markings,list(
    geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight), colour = "white", size = 1),
    geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight), colour = "white", size = 1)))
  
  return(pitch_markings)
}

#Create the heatmap
heatmap_colours <- ggplot() +
  geom_bin2d(data = heatmap_data, aes(x = location.x, y = location.y), bins = 10) +
  scale_fill_gradient(low = "#224C56", high = "red", name = "Action Density") +
  scale_y_reverse() +
  #keep pitch scale correct as done by Statsbomb
  coord_fixed(ratio = 105/100) +
  labs(title = "Heatmap") +
  #Keep theme consistent with other plots
  theme_minimal(base_size = 16) +
  theme(plot.background = element_rect(fill = "#224C56", color = NA),
        panel.background = element_rect(fill = "#224C56", color = NA),
        plot.title = element_text(color = "white", hjust = 0.5, size = 16),
        plot.subtitle = element_text(color = "white", hjust = 0.5, size = 16),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

#Add the pitch markings to the heatmap
heatmap_plot <- heatmap_colours +
  plot_pitch_markings(line_colour = "white")

#----Average Stats for Position----

#Function change positions into more general so they are not seperate by right or left
#For example a right or left winger would be grouped together
normalise_positions <- function(df) {
  df <- df %>%
    mutate(most_played_position = case_when(
      most_played_position %in% c("Right Wing", "Left Wing") ~ "Winger",
      most_played_position %in% c("Right Back", "Left Back") ~ "Full Back",
      most_played_position %in% c("Right Center Back", "Left Center Back") ~ "Center Back",
      most_played_position %in% c("Right Center Forward", "Left Center Forward") ~ "Center Forward",
      most_played_position %in% c("Right Defensive Midfield", "Left Defensive Midfield") ~ "Defensive Midfield",
      most_played_position %in% c("Right Center Midfield", "Left Center Midfield") ~ "Center Midfield",
      most_played_position %in% c("Right Midfield", "Left Midfield") ~ "Wide Midfield",
      most_played_position %in% c("Right Attacking Midfield", "Left Attacking Midfield") ~ "Center Attacking Midfield",
      most_played_position %in% c("Right Wing Back", "Left Wing Back") ~ "Wing Back",
      TRUE ~ most_played_position
    ))
  return(df)
}

#Apply function to both stats dfs
season_stats <- normalise_positions(season_stats)
stats_per_match <- normalise_positions(stats_per_match)

#Get the position played most in selected game by selected player
position_played <- stats_per_match %>%
  filter(player.name == player_analysed & match_id == match_analysed) %>%
  pull(most_played_position)

#Calculate weighted averages for all stats based on minutes_played for position player played most in game
average_for_position <- season_stats %>%
  filter(most_played_position == position_played) %>%
  #create row for average for position with NA for player id and team id
  summarise(player.id = NA,
            player.name = paste("Average for", position_played, "(EPL 15/16)"),
            team.name = NA,
            across(where(is.numeric) & !matches("minutes_played"),
                   ~ round(sum(.x * minutes_played, na.rm = TRUE) / sum(minutes_played, na.rm = TRUE), 2)),
            minutes_played = sum(minutes_played, na.rm = TRUE))

#Define exceptions that do not have per90 but need to be kept 
exceptions <- c("player.name")

#Filter the df to only include player.name and columns ending in per90
average_for_position <- average_for_position[, grepl("(rate|per90)$", colnames(average_for_position)) | colnames(average_for_position) %in% exceptions]
  
#Remove _per90 from the end of columns
colnames(average_for_position) <- gsub("_per90$", "", colnames(average_for_position))

#Filter to get only stats from match and player selected
match_stats <- stats_per_match %>%
  filter(player.name == player_analysed & match_id == match_analysed)

#Join average row for position with match stats
data_for_table <- bind_rows(match_stats, average_for_position)

#Remove .00 from all data
data_for_table <- data_for_table %>%
  mutate(across(where(is.numeric), ~ ifelse(. %% 1 == 0, as.character(as.integer(.)), as.character(.))))

#Choose the stats wanted to in table
data_for_table <- data_for_table %>%
  select(player.name, 
         #Shooting Stats
         goals, xG, shots, conversion_rate, 
         #Passing Stats
         assists, xGA,
         passes_completed, passes_completion_rate,
         final_3rd_passes_completed, key_passes, 
         #Dribbling Stats
         carries,
         #General Stats
         progressive_actions, distanced_progressed)

#Pivot data
data_flipped <- data_for_table %>%
  pivot_longer(cols = -player.name, 
               names_to = "stat_type", 
               values_to = "value")

#Transform data so there is column for player analysed and average postion
data_transformed <- data_flipped %>%
  pivot_wider(names_from = player.name,
              values_from = value) %>%
  mutate(stat_type = ifelse(stat_type %in% c("xG", "xGA"), stat_type, 
                            gsub("_", " ", stat_type) %>% str_to_title()), 
    stat_type = ifelse(str_detect(stat_type, "Rate"), 
                       paste0(stat_type, " (%)"), 
                       stat_type)) 

#Change the name of Progressive Actions to add more clarity
data_transformed <- data_transformed %>%
  mutate(stat_type = if_else(stat_type == "Progressive Actions", 
                             "Progressive Actions (Passes + Carries)", 
                             stat_type))

#----Table for Stats----

#Create the table
stats_table <- flextable(data_transformed)

#Remove column name for stat_type
stats_table <- set_header_labels(stats_table, stat_type = "")

#Autofit the width of columns
stats_table <- set_table_properties(stats_table, layout = "autofit")

#Align text to centre
stats_table <- align(stats_table, align = "center", part = "all")

#Set all text in table to white
stats_table <- color(stats_table, part = "all", color = "white")

#Change background colour to keep with other plots
stats_table <- bg(stats_table, bg = "#224C56", part = "body")
stats_table <- bg(stats_table, bg = "#2A6373", part = "header")

#Change all text in table to size 12 and make header bold
stats_table <- fontsize(stats_table, size = 12, part = "all")
stats_table <- bold(stats_table, part = "header")

#Remove all default borders
stats_table <- border_remove(stats_table)

#Add thick borders between columns
stats_table <- border_inner_v(stats_table, border = fp_border(color = "white", width = 2))

#Add thin borders between rows
stats_table <- border_inner_h(stats_table, border = fp_border(color = "white", width = 1))

#Add thick border between header and body
stats_table <- border(stats_table, part = "header", border.bottom = fp_border(color = "white", width = 2))

#----Combine Plots and Table----

#Save the table as an image to be able to combine it with other plots
table_img <- save_as_image(stats_table, path = "stats_table.png")

#Convert the table image to a ggplot object
table_plot <- ggdraw() + draw_image(table_img)

#Extract the legend from events_plot
legend_plot <- cowplot::get_legend(events_plot + theme(legend.position = "right"))

#Use ggdraw to keep consistnet background colour
legend_plot <- ggdraw(legend_plot) +
  theme(plot.margin = margin(0, 0, 0, 0), 
        aspect.ratio = 1)

#Combine plots into one
#One line for each column of plots
final_plot <- (events_plot | 
                   #row is divided into 3
                   (shot_outcome_plot / heatmap_plot / legend_plot) | 
                   table_plot) +
  #change the dimensons of columns for plots
  plot_layout(widths = c(1.4, 1.2, 1),
              heights = c(1)) +
  #Add title to whole plot and change background colour
  plot_annotation(title = paste(player_analysed, "- Crystal Palace vs. Tottenham - 23/01/2016"),
                  theme = theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "white"),
                                plot.background = element_rect(fill = "#224C56", color = NA)))

#Display the final output
final_plot
