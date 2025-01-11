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
player_analysed <- "Roberto Firmino"

#----Filter Data----

#Filter to only include passes from correct player
shot_map_data <- match_event_data %>%
  filter(player.name == player_analysed & type.name == "Shot")

#----Plot Half Pitch----

#Create function to plot half a pitch
plot_half_pitch <- function(grass_colour, line_colour, background_colour, goal_colour) {
  
  #Create clean plot
  theme_blankPitch = function(size=12) { 
    theme(
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(), 
      axis.ticks.length=unit(0, "lines"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(), 
      legend.background=element_rect(fill=background_colour, colour=NA), 
      legend.key=element_rect(fill=background_colour), 
      legend.key.size=unit(1.2, "lines"), 
      legend.text=element_text(size=size), 
      legend.title=element_text(size=size, face="bold", hjust=0),
      strip.background=element_rect(fill=background_colour, linewidth=0.5),
      panel.background=element_rect(fill=background_colour, colour=background_colour), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2))
  }
  
  #Set Pitch boundaries
  ymin <- 0
  ymax <- 80
  xmin <- 60
  xmax <- 120
  
  #Set dimenions for box and features
  boxEdgeOff <- 102
  sixYardOff <- 114
  penSpotOff <- 108
  halfwayline <- 60
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  sixYardLeft <- 30 
  sixYardRight <- 50
  goalPostLeft <- 36
  goalPostRight <- 44
  CentreSpot <- 40   
  centreCirle_d <- 20   
  
  #Function to generate a circle
  circleFun <- function(center = c(0,0), diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0, 2 * pi, length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))}
  
  #Set out the arc of the penalty area
  dArc <- circleFun(c(40, penSpotOff), centreCirle_d, npoints = 1000)
  dArc <- dArc[which(dArc$y <= boxEdgeOff),]
  
  #Plot pitch using dimensions set out above
  plot <- ggplot() + xlim(c(ymin, ymax)) + ylim(c(xmin, xmax)) +
    theme_blankPitch() +
    geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill=grass_colour, colour=line_colour) +
    geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill=grass_colour, colour=line_colour) +
    geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=xmax), fill=grass_colour, colour=line_colour) +
    geom_path(data=dArc, aes(x=x, y=y), colour=line_colour) +
    geom_point(aes(x=CentreSpot, y=penSpotOff), colour=line_colour) +
    geom_segment(aes(x=goalPostLeft, y=xmax, xend=goalPostRight, yend=xmax), colour=goal_colour, linewidth=1)
  
  return(plot)
}

#----Plot Shot Map----

#Set function to variable with colours
plot_blank_shotmap <- plot_half_pitch("#224C56", "white", "#224C56", "#15393D")

#Use function to plot map with data
plot_blank_shotmap +
  geom_point(data = shot_map_data,
             aes(x = location.y, y = location.x, size = shot.statsbomb_xg, 
                 shape = shot.body_part.name,
                 fill = ifelse(shot.outcome.name == "Goal", "Goal", "Unsuccessful Shot")),
             stroke = 1,
             alpha = 0.8) +
  #change colours according to shot outcome
  scale_fill_manual(values = c("Goal" = "gold", "Unsuccessful Shot" = "red"),
                    name = "Shot Outcome",
                    #Change size and shape for legend
                    guide = guide_legend(override.aes = list(size = 6, shape = 23))) +
  #change shape for body part used for shot
  scale_shape_manual(values = c("Other" = 21, "Right Foot" = 22, "Left Foot" = 23, "Head" = 24),
                     name = "Body Part",
                     #Change size and colour for legend
                     guide = guide_legend(override.aes = list(size = 6, fill = "white"))) +
  #change size for xG of shot
  scale_size_continuous(range = c(1.5, 8), limits = c(0, 1), breaks = c(0, 0.5, 1),
                        labels = c("0.0", "0.5", "1.0"),
                        name = "xG",
                        #Change size, colour and shape for legend
                        guide = guide_legend(override.aes = list(fill = "white", shape = 23, size = 6))) +
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
        plot.subtitle = element_text(color = "white", hjust = 0.5, size = 16),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
