# Statsbomb Data in R
The repository uses free StatsBomb event data (from the Premier League 2015/16) in R to calculate stats and create visualisations. These are then used to create a shiny dashboard that provides insights into a player’s performance in a chosen match.

An interactive version of the Shiny dashboard is available on my <ins>[Shinyapp.io]([https://your-shinyapp-url](https://max-bosher.shinyapps.io/performance_dashboard/))</ins> page. However, due to RAM limitations on the platform, only data for Leicester City is accessible.

![image](https://github.com/user-attachments/assets/c2830589-bb91-45e4-a4a4-2783a23f9123)


## Folder Structure
### Shiny_Dashboards:

This Shiny App is designed to be dynamic, allowing users  to analyse any player in any game. It includes:

 - A plot showing the location of all shots, passes, carries and defensive actions.
 - Heatmap
 - Shot Outcome Plot
 - Table showing the chosen player’s statistics compared to the average performance of their position across the season

### Calculating Stats

The code calculates advanced statistics for all players across the entire season or for individual games. The average for each team is also calculated alongside 90-minute stats and the success rate for key metrics. An example of some of the stats calculated is xG Prevented (GK), time spent pressuring, and the net impact on xG a player has while they are on the pitch.

### Scrapping

Includes the code that scraps all event data and matches information for the Premier League season of 2015/16 and saves it for use in the other sections of this project.

### Visualisations

This repository contains code to create visualisations, such as shot, pass and heatmaps. These visualisations provide insights into player behaviour, movement patterns, and effectiveness on the pitch, making them useful for performance analysis and scouting.

## License
This project uses free StatsBomb data pulled using the Statsbomb API, as shown in the code.

