#----Open Files----

#clean environment
rm(list = ls())

#load packages
library(shiny)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(readr)
library(cowplot)
library(patchwork)
library(flextable)
library(officer)
library(stringr)
library(magick)

#Open Premier League event Data for 2015/16 season
match_event_data <- read_rds("prem_event_data_15-16.rds")

#Open season-long stats calculated in other code
season_stats <- read.csv("Prem_Stats_2015-16_for_season.csv")

#Open stats for each match calculated in other code
stats_per_match <- read.csv("Prem_Stats_2015-16_per_match.csv")

#Create df that contains match details (to locate the desired match_id)
match_data <- read_rds("prem_matches_15-16.rds")

#----Create Data for Dropdown Menus----

#format match_date to day month year format
match_data <- match_data %>%
  mutate(match_date = as.Date(match_date, format = "%Y-%m-%d")) %>%  # Convert to Date type
  mutate(match_date = format(match_date, "%d-%m-%Y"))  # Keep formatted as "DD-MM-YYYY"

#create list of matches sorted by date for dropdown menu
list_of_matches <- match_data %>%
  select(match_id, match_date, 
         home_team = home_team.home_team_name, 
         away_team = away_team.away_team_name) %>%
  mutate(match_date = as.Date(match_date, format = "%d-%m-%Y")) %>%  # Convert back to Date for sorting
  arrange(match_date) %>%
  mutate(match_date = format(match_date, "%d-%m-%Y")) %>%  # Reformat after sorting
  mutate(match_summary = paste0(home_team, " vs ", away_team, " (", match_date, ")"))  # Ensure correct format

#----Custom Theme----

#Custom theme for plots to maintain consistent styling
custom_theme <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "#224C56", color = NA),
      panel.background = element_rect(fill = "#224C56", color = NA),
      legend.background = element_rect(fill = "#224C56", color = NA),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      plot.title = element_text(color = "white", hjust = 0.5),
      plot.subtitle = element_text(color = "white", hjust = 0.5),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank())}

#----Combined Plot----

create_events_plot <- function(player_analysed, match_analysed, match_event_data) {
  
  #filter data to only include passes
  pass_data <- match_event_data %>%
    filter(player.name == player_analysed, 
           type.name == "Pass", 
           match_id == match_analysed, 
           is.na(pass.outcome.name)) %>%
    mutate(action_type = if_else(is.na(pass.goal_assist), "Pass", "Assist"))
  
  #filter data to only include carries
  carry_data <- match_event_data %>%
    filter(player.name == player_analysed, 
           type.name == "Carry", 
           match_id == match_analysed) %>%
    mutate(carry_distance = sqrt((carry.end_location.x - location.x)^2 + 
                                   (carry.end_location.y - location.y)^2)) %>%
    filter(carry_distance >= 3) %>%
    mutate(action_type = "Carry")
  
  #filter data to only include defensive actions
  defensive_action_data <- match_event_data %>%
    filter(player.name == player_analysed, match_id == match_analysed) %>%
    filter(`50_50.outcome.name` == "Won" | `50_50.outcome.name` == "Success To Team" |
             (type.name == "Ball Recovery" & is.na(ball_recovery.recovery_failure)) |
             type.name == "Block" |
             type.name == "Clearance" |
             (duel.type.name == "Tackle" & (duel.outcome.name %in% c("Won", "Success", "Success In Play", "Success Out"))) |
             (type.name == "Interception" & (interception.outcome.name %in% c("Won", "Success", "Success In Play", "Success Out")))) %>%
    mutate(action_type = "Defensive Action")
  
  #filter data to only include shots
  shot_data <- match_event_data %>%
    filter(player.name == player_analysed, 
           match_id == match_analysed, 
           type.name == "Shot") %>%
    mutate(action_type = if_else(shot.outcome.name == "Goal", "Goal", "Unsuccessful Shot"))
  
  #combine all action types needed for plot
  combined_data <- bind_rows(
    pass_data %>% mutate(event_xend = pass.end_location.x, event_yend = pass.end_location.y),
    carry_data %>% mutate(event_xend = carry.end_location.x, event_yend = carry.end_location.y),
    defensive_action_data %>% mutate(event_xend = NA, event_yend = NA),
    shot_data %>% mutate(event_xend = NA, event_yend = NA))
  
  #Define order for legend
  action_types <- c("Goal", "Unsuccessful Shot", "Pass", "Assist", "Carry", "Defensive Action")
  
  #Create dummy_data to help reorder legend
  dummy_data <- tibble(
    location.x = NA,
    location.y = NA,
    event_xend = NA,
    event_yend = NA,
    shot.statsbomb_xg = NA,
    action_type = factor(action_types, levels = action_types))
  
  #Combine dummy data with existing data to order
  combined_data <- combined_data %>%
    mutate(action_type = factor(action_type, levels = action_types)) %>%
    bind_rows(dummy_data)
  
  #write function taken from sbpitch package as it does not work in shiny apps
  create_Pitch <- function(grass_colour = "#F9F9F9", line_colour = "#8F8F8F", background_colour = "#F9F9F9", goal_colour = "#000000", goaltype = "line", middlethird = FALSE, BasicFeatures = FALSE, JdeP = FALSE, arcs = TRUE, padding = 5){
    
    ## set theme for blank pitch
    theme_blankPitch = function(size=12) {
      theme(
        #axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        #axis.ticks.y=element_text(size=size),
        #   axis.ticks=element_blank(),
        axis.ticks.length=unit(0, "lines"),
        #axis.ticks.margin=unit(0, "lines"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.background=element_rect(fill=background_colour, colour=NA),
        legend.key=element_rect(colour=background_colour,fill=background_colour),
        legend.key.size=unit(1.2, "lines"),
        legend.text=element_text(size=size),
        legend.title=element_text(size=size, face="bold",hjust=0),
        strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
        panel.background=element_rect(fill=background_colour,colour=background_colour),
        #       panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing=element_blank(),
        plot.background=element_blank(),
        plot.margin=unit(c(0, 0, 0, 0), "lines"),
        plot.title=element_text(size=size*1.2),
        strip.text.y=element_text(colour=background_colour,size=size,angle=270),
        strip.text.x=element_text(size=size*1))}
    
    ymin <- 0 # minimum width
    ymax <- 80 # maximum width
    xmin <- 0 # minimum length
    xmax <- 120 # maximum length
    
    # Defining features along the length
    boxEdgeDef <- 18
    boxEdgeOff <- 102
    halfwayline <- 60
    sixYardDef <- 6
    sixYardOff <- 114
    penSpotDef <- 12
    penSpotOff <- 108
    
    # Defining features along the width
    boxEdgeLeft <- 18
    boxEdgeRight <- 62
    sixYardLeft <- 30
    sixYardRight <- 50
    goalPostLeft <- 36
    goalPostRight <- 44
    CentreSpot <- 40
    
    # other dimensions
    centreCirle_d <- 20
    
    ## define the circle function
    circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
      r = diameter / 2
      tt <- seq(0,2*pi,length.out = npoints)
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }
    
    #### create center circle ####
    center_circle <- circleFun(c(halfwayline,CentreSpot),centreCirle_d,npoints = 100)
    
    ### FIRST STAGE
    ## initiate the plot, set some boundries to the plot
    p <- ggplot() + xlim(c(xmin-padding,xmax+padding)) + ylim(c(ymin-padding,ymax+padding)) +
      # add the theme
      theme_blankPitch()
    
    ### ADD MIDDLE THIRD SHADING
    if(middlethird == TRUE){
      p <- p + geom_rect(aes(xmin=(xmax/3*1), xmax=(xmax/3*2), ymin=ymin, ymax=ymax), colour = NA, fill = "black", alpha = 0.10)
    }else{}
    
    if(BasicFeatures == TRUE){
      p <- p +
        # add the base rectangle of the pitch
        geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = NA, colour = line_colour) +
        # add the 18 yard box defensive
        geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) +
        # add the 18 yard box offensive
        geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) +
        # add halway line
        geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),colour = line_colour)
      arcs = FALSE
    }else{
      ## initiate the plot, set some boundries to the plot
      p <- p +
        # add the base rectangle of the pitch
        geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = NA, colour = line_colour) +
        # add the 18 yard box defensive
        geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) +
        # add the 18 yard box offensive
        geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) +
        # add halway line
        geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),colour = line_colour) +
        # add the six yard box Defensive
        geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_colour, colour = line_colour)  +
        # add the six yard box offensive
        geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_colour, colour = line_colour) +
        # add centre circle
        geom_path(data=center_circle, aes(x=x,y=y), colour = line_colour) +
        # add penalty spot left
        geom_point(aes(x = penSpotDef , y = CentreSpot), colour = line_colour, size = 0.75) +
        # add penalty spot right
        geom_point(aes(x = penSpotOff , y = CentreSpot), colour = line_colour, size = 0.75) +
        # add centre spot
        geom_point(aes(x = halfwayline , y = CentreSpot), colour = line_colour, size = 0.75) }
    
    #### add goals depending on type
    
    ## LINE TYPE
    if(goaltype == "line"){
      p <- p +
        # add the goal Defensive
        geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),colour = goal_colour, size = 1) +
        # add the goal offensive
        geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_colour, size = 1)
      
    }else{}
    
    ## Barca Numbers TYPE
    if(goaltype == "barcanumbers"){
      p <- p +
        # add the goal Defensive
        geom_segment(aes(x = xmin - 0.75, y = goalPostLeft, xend = xmin - 0.75, yend = goalPostRight),colour = line_colour, size = 0.75) +
        # add the goal offensive
        geom_segment(aes(x = xmax + 0.75, y = goalPostLeft, xend = xmax + 0.75, yend = goalPostRight),colour = line_colour, size = 0.75)
      
    }else{}
    
    ## BOX TYPE
    if(goaltype == "box"){
      p <- p +
        # add the goal Defensive
        geom_rect(aes(xmin = xmin - 2 , ymin = goalPostLeft, xmax = xmin, ymax = goalPostRight), fill = grass_colour, colour = line_colour) +
        # add the goal offensive
        geom_rect(aes(xmin = xmax, ymin = goalPostLeft, xmax = xmax + 2, ymax = goalPostRight), fill = grass_colour, colour = line_colour)
    }else{}
    
    
    ## add J de P
    if(JdeP == TRUE){
      p <- p +
        # vertical tram lines
        geom_segment(aes(x = boxEdgeDef, y = boxEdgeLeft, xend = boxEdgeOff, yend = boxEdgeLeft), colour = "#941C07", alpha = 0.3) +
        geom_segment(aes(x = boxEdgeDef, y = boxEdgeRight, xend = boxEdgeOff, yend = boxEdgeRight), colour = "#941C07", alpha = 0.3) +
        geom_segment(aes(x = boxEdgeDef, y = CentreSpot - 10, xend = boxEdgeOff, yend = CentreSpot - 10), colour = "#941C07", alpha = 0.3) +
        geom_segment(aes(x = boxEdgeDef, y = CentreSpot + 10, xend = boxEdgeOff, yend = CentreSpot + 10), colour = "#941C07", alpha = 0.3) +
        # horizontal tram lines
        geom_segment(aes(x = boxEdgeDef, y = ymin, xend = boxEdgeDef, yend = ymax), colour = "#941C07", alpha = 0.3) +
        geom_segment(aes(x = boxEdgeOff, y = ymin, xend = boxEdgeOff, yend = ymax), colour = "#941C07", alpha = 0.3) +
        geom_segment(aes(x = (xmax/3*1), y = boxEdgeRight, xend = (xmax/3*1), yend = ymax), colour = "#941C07", alpha = 0.3) +
        geom_segment(aes(x = (xmax/3*1), y = boxEdgeLeft, xend = (xmax/3*1), yend = ymin), colour = "#941C07", alpha = 0.3) +
        geom_segment(aes(x = (xmax/3*2), y = boxEdgeRight, xend = (xmax/3*2), yend = ymax), colour = "#941C07", alpha = 0.3) +
        geom_segment(aes(x = (xmax/3*2), y = boxEdgeLeft, xend = (xmax/3*2), yend = ymin), colour = "#941C07", alpha = 0.3)
      # add the 18 yard box defensive
      #geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = NA, colour = line_colour) +
      # add the 18 yard box offensive
      #geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = NA, colour = line_colour)
    }else{}
    
    ## add J de P
    if(arcs == TRUE){
      p <- p +
        # vertical tram lines
        annotate("path",
                 x = 12 + 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
                 y = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
                 col = line_colour) +
        annotate("path",
                 x = (120-12) - 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
                 y = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
                 col = line_colour)
    }else{}
    
    return(p)
    
  }
  
  #Create a base events plot with legend visible.
  base_events_plot <- create_Pitch(goaltype = "box",
                                   grass_colour = "#224C56", 
                                   line_colour = "white", 
                                   background_colour = "#224C56", 
                                   goal_colour = "#15393D") +
    #Plot passes
    geom_segment(data = combined_data %>% filter(action_type %in% c("Pass", "Assist")),
                 aes(x = location.x, y = location.y, 
                     xend = event_xend, yend = event_yend, 
                     colour = action_type),
                 lineend = "round", linewidth = 0.5,
                 arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "closed")) +
    #Plot Carries
    geom_segment(data = combined_data %>% filter(action_type == "Carry"),
                 aes(x = location.x, y = location.y, 
                     xend = event_xend, yend = event_yend, 
                     colour = action_type),
                 lineend = "round", linewidth = 0.5, linetype = "dashed",
                 arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "closed")) +
    #Plot defensive actions
    geom_point(data = combined_data %>% filter(action_type == "Defensive Action"),
               aes(x = location.x, y = location.y, colour = action_type), size = 4) +
    #Plot shots
    geom_point(data = combined_data %>% filter(action_type %in% c("Goal", "Unsuccessful Shot")),
               aes(x = location.x, y = location.y, size = shot.statsbomb_xg, colour = action_type)) +
    #Add arrows and text to show attacking directio
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
    #Colours for legend
    scale_colour_manual(
      values = c("Pass" = "grey", "Assist" = "gold", 
                 "Carry" = "grey", "Defensive Action" = "cyan", 
                 "Goal" = "gold", "Unsuccessful Shot" = "red"),
      breaks = action_types,
      guide = guide_legend(override.aes = list(size = 8))) +
    scale_size_continuous(range = c(1.5, 8), limits = c(0, 1),
                          breaks = c(0, 0.5, 1),
                          labels = c("0.0", "0.5", "1.0"),
                          name = "xG",
                          guide = guide_legend(override.aes = list(color = "white"))) +
    #add theme to plot
    custom_theme(16) +
    theme(plot.title = element_text(size = 22),
          plot.subtitle = element_text(size = 16),
          legend.position = "right",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16)) +
    #Make sure the ratio correct and pitch is flipped
    coord_fixed(ratio = 105/100) +
    coord_flip()
  
  #Extract the legend from the base plot.
  legend_extracted <- cowplot::get_legend(base_events_plot)
  #Remove the legend from the plot version.
  events_plot_no_legend <- base_events_plot + theme(legend.position = "none")
  
  return(list(plot = events_plot_no_legend, legend = legend_extracted))
}

#----Shot Outcome Map----

create_shot_outcome_plot <- function(player_analysed, match_analysed, match_event_data) {
  
  #Filter to only get shot data
  shot_data <- match_event_data %>%
    filter(player.name == player_analysed, 
           match_id == match_analysed, 
           type.name == "Shot") %>%
    mutate(event_name = if_else(shot.outcome.name == "Goal", "Goal", "Unsuccessful Shot"))
  
  #filter to get location of shot end locations
  shot_outcome_data <- shot_data %>%
    filter(!is.na(shot.end_location.z)) %>%
    mutate(shot.end_location.y = pmin(pmax(shot.end_location.y, 34), 46),
           shot.end_location.z = pmin(pmax(shot.end_location.z, -0.2), 3.5))
  
  # Function to create the goal background
  plot_goal <- function(background_colour, line_colour, goal_colour) {
    #creates blank plot
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
            plot.title = element_text(size = size * 1.2))
    }
    
    #sets detials for goal size
    goal_width <- c(36, 44)
    goal_height <- 2.67
    goal_depth <- 0
    width <- c(34,46)
    height <- c(0, 3.5)
    
    gridlines_horizontal <- data.frame(y = seq(goal_depth, goal_height, length.out = 5),
                                       x_start = goal_width[1], x_end = goal_width[2])
    gridlines_vertical <- data.frame(x = seq(goal_width[1], goal_width[2], length.out = 7),
                                     y_start = goal_depth, y_end = goal_height)
    #Plot goal post on blank plot
    plot <- ggplot() +
      theme_blankGoal() +
      geom_rect(aes(xmin = goal_width[1], xmax = goal_width[2], ymin = goal_depth, ymax = goal_height),
                fill = background_colour, colour = line_colour) +
      geom_segment(data = gridlines_horizontal,
                   aes(x = x_start, xend = x_end, y = y, yend = y),
                   colour = "grey", linetype = "dashed") +
      geom_segment(data = gridlines_vertical,
                   aes(x = x, xend = x, y = y_start, yend = y_end),
                   colour = "grey", linetype = "dashed") +
      geom_rect(aes(xmin = width[1], xmax = width[2], ymin = height[1], ymax = height[2]),
                fill = NA, colour = "grey", linewidth = 0.5) +
      geom_segment(aes(x = goal_width[1], y = goal_depth, xend = goal_width[1], yend = goal_height),
                   colour = goal_colour, linewidth = 1.5) +
      geom_segment(aes(x = goal_width[2], y = goal_depth, xend = goal_width[2], yend = goal_height),
                   colour = goal_colour, linewidth = 1.5) +
      geom_segment(aes(x = goal_width[1], y = goal_height, xend = goal_width[2], yend = goal_height),
                   colour = goal_colour, linewidth = 1.5) +
      geom_segment(aes(x = width[1], y = goal_depth, xend = width[2], yend = goal_depth),
                   colour = "white", linewidth = 1) +
      xlim(c(34, 46)) +
      ylim(c(-0.2, 3.5))
    
    return(plot)
  }
  
  #Plot outcome locations
  shot_outcome_plot <- plot_goal("#224C56", "white", "white") +
    geom_point(data = shot_outcome_data,
               aes(x = shot.end_location.y,
                   y = shot.end_location.z,
                   size = shot.statsbomb_xg,
                   color = event_name)) +
    labs(title = "Shot Outcome Map") +
    scale_color_manual(values = c("Goal" = "gold", "Unsuccessful Shot" = "red"),
                       name = "Shot Outcome",
                       guide = guide_legend(override.aes = list(size = 8))) +
    scale_size_continuous(range = c(1.5, 8), limits = c(0, 1),
                          breaks = c(0, 0.5, 1),
                          labels = c("0.0", "0.5", "1.0"),
                          name = "xG",
                          guide = guide_legend(override.aes = list(color = "white", size = 8))
    ) +
    #Make sure ratio of goal stays correct
    coord_fixed(ratio = 105/100) +
    #Make sure plot is correct theme
    custom_theme(16) +
    theme(legend.position = "none",
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12))
  
  return(shot_outcome_plot)
}

#----Heatmap----

create_heatmap <- function(player_analysed, match_analysed, match_event_data) {
  
  #Filter for data needed for heatmap
  heatmap_data <- match_event_data %>%
    filter(player.name == player_analysed, match_id == match_analysed) %>%
    filter(location.x >= 0, location.x <= 120,
           location.y >= 0, location.y <= 80)
  
  #function to plot pitch as needs to be plot after heatmap which doesnt work with createPitch()
  plot_pitch_markings <- function(line_colour) {
    ymin <- 0; ymax <- 80; xmin <- 0; xmax <- 120
    boxEdgeDef <- 18; boxEdgeOff <- 102; halfwayline <- 60
    sixYardDef <- 6; sixYardOff <- 114; penSpotDef <- 12; penSpotOff <- 108
    goalPostLeft <- 36; goalPostRight <- 44; CentreSpot <- 40
    
    circleFun <- function(center = c(0, 0), diameter = 1, npoints = 100) {
      r <- diameter / 2
      tt <- seq(0, 2 * pi, length.out = npoints)
      data.frame(x = center[1] + r * cos(tt), y = center[2] + r * sin(tt))
    }
    
    centre_circle <- circleFun(c(halfwayline, CentreSpot), 20, npoints = 100)
    
    pitch_markings <- list(
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = "transparent", colour = line_colour),
      geom_rect(aes(xmin = xmin, xmax = boxEdgeDef, ymin = 18, ymax = 62),
                fill = "transparent", colour = line_colour),
      geom_rect(aes(xmin = boxEdgeOff, xmax = xmax, ymin = 18, ymax = 62),
                fill = "transparent", colour = line_colour),
      geom_rect(aes(xmin = xmin, xmax = sixYardDef, ymin = 30, ymax = 50),
                fill = "transparent", colour = line_colour),
      geom_rect(aes(xmin = sixYardOff, xmax = xmax, ymin = 30, ymax = 50),
                fill = "transparent", colour = line_colour),
      geom_point(aes(x = penSpotDef, y = 40), colour = line_colour, size = 0.75),
      geom_point(aes(x = penSpotOff, y = 40), colour = line_colour, size = 0.75),
      geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),
                   colour = line_colour),
      geom_path(data = centre_circle, aes(x = x, y = y), colour = line_colour)
    )
    
    pitch_markings <- c(pitch_markings, list(
      annotate("path", x = 12 + 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
               y = 40 + 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
               col = line_colour),
      annotate("path", x = (120 - 12) - 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
               y = 40 + 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
               col = line_colour)
    ))
    
    pitch_markings <- c(pitch_markings, list(
      geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),
                   colour = "white", size = 1),
      geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),
                   colour = "white", size = 1)
    ))
    
    return(pitch_markings)
  }
  
  #Plot heatmpa
  heatmap_colours <- ggplot() +
    geom_bin2d(data = heatmap_data, aes(x = location.x, y = location.y), bins = 10) +
    scale_fill_gradient(low = "#224C56", high = "red", name = "Action Density") +
    scale_y_reverse() +
    coord_fixed(ratio = 105/100) +
    labs(title = "Heatmap") +
    custom_theme(16) +
    theme(legend.position = "none")
  
  #Add pitch markings to heatmap plot
  heatmap_plot <- heatmap_colours + plot_pitch_markings(line_colour = "white")
  
  return(heatmap_plot)
}

#----Stats Table----

create_stats_table_grob <- function(player_analysed, match_analysed, stats_per_match, season_stats) {
  
  #Function to normalise positions meaning there is not right/left etc
  normalise_positions <- function(df) {
    df %>%
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
        TRUE ~ most_played_position))}
  
  #Use function to normalise positions
  season_stats <- normalise_positions(season_stats)
  stats_per_match <- normalise_positions(stats_per_match)
  
  #Pull the position played by chosen player in chosen match
  position_played <- stats_per_match %>%
    filter(player.name == player_analysed, match_id == match_analysed) %>%
    pull(most_played_position)
  
  #Calucalte the average position for position played across the season
  average_for_position <- season_stats %>%
    filter(most_played_position == position_played) %>%
    summarise(player.id = NA,
              player.name = paste("Average for", position_played, "per 90 Minutes (EPL 15/16)"),
              team.name = NA,
              across(where(is.numeric) & !matches("minutes_played"),
                     ~ round((sum(.x * minutes_played, na.rm = TRUE) / sum(minutes_played, na.rm = TRUE)), 2)),
              minutes_played = "NA")
  
  #Select needed stats and remove per 90
  average_for_position <- average_for_position %>%
    select(player.name, minutes_played, 
           goals = goals_per90, 
           xG = xG_per90, 
           shots = shots_per90, 
           conversion_rate, 
           assists = assists_per90, 
           xGA = xGA_per90, 
           passes_completed = passes_completed_per90, 
           passes_completion_rate,
           final_3rd_passes_completed = final_3rd_passes_completed_per90, 
           key_passes = key_passes_per90, 
           carries = carries_per90, 
           progressive_actions = progressive_actions_per90, 
           distanced_progressed = distanced_progressed_per90)
  
  match_stats <- stats_per_match %>%
    filter(player.name == player_analysed, match_id == match_analysed) %>%
    mutate(minutes_played = as.character(minutes_played))
  
  #Combine match stats for player and average position stats
  data_for_table <- bind_rows(match_stats, average_for_position)
  
  data_for_table <- data_for_table %>%
    mutate(across(where(is.numeric),
                  ~ ifelse(. %% 1 == 0, as.character(as.integer(.)), as.character(.))))
  
  data_for_table <- data_for_table %>%
    select(player.name, minutes_played, 
           goals, 
           xG, 
           shots, 
           conversion_rate, 
           assists, 
           xGA, 
           passes_completed, 
           passes_completion_rate,
           final_3rd_passes_completed, 
           key_passes, carries, 
           progressive_actions, 
           distanced_progressed)
  
  #Flip data for better table layout
  data_flipped <- data_for_table %>%
    pivot_longer(cols = -player.name, names_to = "stat_type", values_to = "value")
  
  #Make stats names look  better for table
  data_transformed <- data_flipped %>%
    pivot_wider(names_from = player.name, values_from = value) %>%
    mutate(stat_type = ifelse(stat_type %in% c("xG", "xGA"), stat_type, 
                              gsub("_", " ", stat_type) %>% str_to_title()),
           stat_type = ifelse(str_detect(stat_type, "Rate"), paste0(stat_type, " (%)"), stat_type)) %>%
    mutate(stat_type = if_else(stat_type == "Progressive Actions", "Progressive Actions (Passes + Carries)", stat_type))
  
  #Create flextable and format it
  stats_table <- flextable::flextable(data_transformed)
  stats_table <- flextable::set_header_labels(stats_table, stat_type = "")
  stats_table <- flextable::set_table_properties(stats_table, layout = "autofit")
  stats_table <- flextable::align(stats_table, align = "center", part = "all")
  stats_table <- flextable::color(stats_table, part = "all", color = "white")
  stats_table <- flextable::bg(stats_table, bg = "#224C56", part = "body")
  stats_table <- flextable::bg(stats_table, bg = "#2A6373", part = "header")
  stats_table <- flextable::fontsize(stats_table, size = 12, part = "all")
  stats_table <- flextable::bold(stats_table, part = "header")
  stats_table <- flextable::border_remove(stats_table)
  stats_table <- flextable::border_inner_v(stats_table, border = officer::fp_border(color = "white", width = 2))
  stats_table <- flextable::border_inner_h(stats_table, border = officer::fp_border(color = "white", width = 1))
  stats_table <- flextable::border(stats_table, part = "header", border.bottom = officer::fp_border(color = "white", width = 2))
  
  #Convert flextable to raster
  table_raster <- as_raster(stats_table)
  
  #Change ratio of plot for optimal shape
  fixed_width <- unit(0.9, "npc")
  fixed_height <- unit(1, "npc")
  
  #Create grob with fixed dimensions
  stats_grob <- grid::grobTree(
    grid::rasterGrob(table_raster, width = fixed_width, height = fixed_height))
  
  #Create a ggplot object with the table
  table_plot <- cowplot::ggdraw() + cowplot::draw_grob(stats_grob)
  
  return(table_plot)
}

#----Shiny UI----

ui <- fluidPage(
  #Change background to keep consistent
  tags$head(tags$style(HTML("
      html, body {
          background-color: #224C56 !important;  /* Dark teal background for entire page */
      }
      .well { background-color: #224C56; }  /* Ensuring consistency in panel styling */
  "))),
  
  #Set background colour for ui and text colour
  style = "background-color: #224C56; color: white;",
  
  #Set title for app
  titlePanel("Match Analysis Dashboard (Attacking-Focused)"),
  
  #Create dropdown menus for team match and player
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #224C56; color: white;",
      selectInput("team", "Select Team", 
                  choices = c("Select a team" = "",  # Default empty selection
                              setNames(sort(unique(c(list_of_matches$home_team, list_of_matches$away_team))),
                                       sort(unique(c(list_of_matches$home_team, list_of_matches$away_team)))))),
      selectInput("match_id", "Select Match", choices = c("Select a match" = "")),
      selectInput("player", "Select Player", choices = c("Select a player" = "")),
      
      #Add text to give instructions
      helpText("Please select a team, then choose a match, then select a player.", style = "color: white;"),
      div(style = "text-align: center; margin-top: 20px; color: white; font-size: 12px;",
          HTML("Developed by <a href='https://www.linkedin.com/in/max-bosher-459a802b1/' 
               style='color:white; text-decoration:underline;'>Max Bosher</a> using Statsbomb data")),
      br(),
      downloadButton("download_plot", "Download Plot as PNG", class = "btn btn-primary")),
    
    #Add the main panel containing the combined plot
    mainPanel(
      #Add spinner to show that plot is loading
      withSpinner(plotOutput("final_plot", width = "100%"), type = 8, color = "white"))))

#----Shiny Server----

server <- function(input, output, session) {
  
  # Observe the inputted team and update match dropdown menu
  observe({
    req(input$team)
    team_matches <- list_of_matches %>% 
      filter(home_team == input$team | away_team == input$team)
    updateSelectInput(session, "match_id", 
                      choices = c("Select a match" = "", 
                                  setNames(team_matches$match_id, team_matches$match_summary)), 
                      selected = "")
  })
  
  # Observe the inputted match and update player dropdown menu
  observe({
    req(input$match_id, input$team)
    players <- match_event_data %>% 
      filter(match_id == as.numeric(input$match_id),
             team.name == input$team,
             !is.na(player.name)) %>% 
      distinct(player.name) %>% 
      pull(player.name)
    # Sort player names alphabetically for better readability
    players <- sort(players)
    updateSelectInput(session, "player", 
                      choices = c("Select a player" = "", 
                                  setNames(players, players)), 
                      selected = "")
  })
  
  # *** NEW: Create a reactive scaling factor for the legend ***
  legend_scale <- reactive({
    req(session$clientData$output_final_plot_width)
    # Get the current width of the final_plot output area
    plot_width <- session$clientData$output_final_plot_width
    # Define a "base" width (e.g., 1200 pixels gives scale 1)
    scale <- plot_width / 1200
    # Clamp the scale between 0.5 and 1 so it doesn’t get too small or too big
    scale <- plot_width / 1200 * 0.8  # Slightly reduce legend size
    max(min(scale, 1), 0.5)
    #scale
  })
  
  # Construct final plot
  final_plot_reactive <- reactive({
    req(input$match_id != "", input$player != "")
    
    # Set variables from selection from dropdown menu
    player_analysed <- input$player
    match_analysed <- as.numeric(input$match_id)
    
    # Generate individual plot using functions
    events_data <- create_events_plot(player_analysed, match_analysed, match_event_data)
    events_plot <- events_data$plot 
    legend_extracted <- events_data$legend
    
    # *** UPDATED: Create the legend using the dynamic scale ***
    legend_plot <- ggdraw() +
      cowplot::draw_grob(legend_extracted, scale = legend_scale())
    
    # Use functions to create additional plots and table
    shot_outcome_plot <- create_shot_outcome_plot(player_analysed, match_analysed, match_event_data)
    heatmap_plot <- create_heatmap(player_analysed, match_analysed, match_event_data)
    stats_table_plot <- create_stats_table_grob(player_analysed, match_analysed, stats_per_match, season_stats)
    
    # Combine all plots and legend into a single plot
    final_plot <- (events_plot | 
                     (shot_outcome_plot / heatmap_plot / legend_plot) | 
                     stats_table_plot) +
      plot_layout(widths = c(1.4, 1.2, 1),
                  heights = c(1)) +
      # Add title annotation with player name, match details, and date
      plot_annotation(title = paste(
        player_analysed, "-",
        (match_data %>% filter(match_id == match_analysed) %>% pull(home_team.home_team_name)),
        "vs.",
        (match_data %>% filter(match_id == match_analysed) %>% pull(away_team.away_team_name)),
        "-",
        (match_data %>% filter(match_id == match_analysed) %>% pull(match_date))),
        theme = theme(plot.title = element_text(size = 28, face = "bold", 
                                                hjust = 0.5, color = "white"),
                      plot.background = element_rect(fill = "#224C56", color = NA)))
    
    return(final_plot)
  })
  
  # Render the final plot
  output$final_plot <- renderPlot({
    final_plot_reactive() 
  },
  # Make sure aspect ratio is 3:2
  width = function() { session$clientData$output_final_plot_width },
  height = function() { session$clientData$output_final_plot_width * 0.667 })
  
  # Download handler remains unchanged
  output$download_plot <- downloadHandler(
    filename = function() { paste("Match_Analysis_", input$player, "_", input$match_id, ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = final_plot_reactive(), width = 12, height = 8, dpi = 300)
    }
  )
  
}

#----Shiny App Launch----

shinyApp(ui = ui, server = server)
