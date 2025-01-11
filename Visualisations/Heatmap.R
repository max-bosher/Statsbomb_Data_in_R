#----Open Files----

#clean environment
rm(list = ls())

#load packages
library(tidyverse)
library(StatsBombR)

#Open Premier League event Data for 2015/16 season
match_event_data <- read_rds("prem_event_data_15-16.rds")

#Select Player
player_analysed <- "Yannick Bolasie"

#----Filter Data----

#Filter to only include passes from correct player
heatmap_data <- match_event_data %>%
  filter(player.name == player_analysed) %>%
  filter(location.x >=0 & location.x <= 120) %>%
  filter(location.y >=0 & location.y <= 80)

#----Function to Plot Pitch Markings----

plot_pitch_markings <- function(line_colour) {
  #Pitch dimensions
  ymin <- 0
  ymax <- 80
  xmin <- 0
  xmax <- 120
  
  #Position of pitch features
  halfwayline <- 60
  boxEdgeDef <- 18
  boxEdgeOff <- 102
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  CentreSpot <- 40
  goalPostLeft <- 36
  goalPostRight <- 44
  
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

#----Plot Heatmap----

#Create the heatmap
heatmap_plot <- ggplot() +
  geom_bin2d(data = heatmap_data, aes(x = location.x, y = location.y), bins = 16) +
  scale_fill_gradient(low = "#224C56", high = "red", name = "Action Density") +
  labs(title = player_analysed,
       subtitle = "Heat Map, Premier League, 2015/16") +
  scale_y_reverse() +
  coord_fixed(ratio = 105/100) +
  #Keep theme consistent with other plots
  theme_minimal(base_size = 16) +
  theme(plot.background = element_rect(fill = "#224C56", color = NA),
        panel.background = element_rect(fill = "#224C56", color = NA),
        plot.title = element_text(color = "white", hjust = 0.5, size = 22),
        plot.subtitle = element_text(color = "white", hjust = 0.5, size = 16),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

#Add the pitch markings to the heatmap
heatmap_plot +
  plot_pitch_markings(line_colour = "white") +
  #Add arrow to show attacking direction
  annotate("segment", 
           x = 45, xend = 75, 
           y = -5, yend = -5, 
           colour = "white", 
           arrow = arrow(length = unit(0.3, "cm"), type = "closed"))
