#clean environment
rm(list = ls()) 

#----load packages----

library(StatsBombR)
library(tidyverse)

#----Scrapping----

#pull all free competitions from Statsbomb
free_comp <- (FreeCompetitions())

#get all free prem matches from 2015/16 season
prem_comp <- free_comp %>%
  filter(competition_id == 2 & season_id == 27)
matches_analysed <- FreeMatches(prem_comp)
#get all event data
uncleaned_data <- free_allevents(matches_analysed)
match_event_data <- allclean(uncleaned_data)

#----Changing to Nicknames----

#Empty list to store the results
lineups_list <- list()

#Loop to get lineups for all needed matches
for (i in 1:nrow(matches_analysed)) {lineups_list[[i]] <- get.lineupsFree(matches_analysed[i, ])}

#Combine the lineup data from all matches into one df
lineups <- as_tibble(do.call(rbind, lineups_list))

#Remove the nests in the lineups df
unnested_lineups <- lineups %>%
  unnest(lineup) %>%
  unnest(positions) %>%
  select(match_id, team.name = team_name, team.id = team_id, player.name = player_name,
         player.id = player_id, player.nickname = player_nickname, position,
         position.id = position_id, from, to)

#Create df containing nicknames of all players 
player_nicknames <- unnested_lineups %>%
  mutate(player.name = if_else(!is.na(player.nickname), player.nickname, player.name)) %>%
  select(player.name, player.id) %>%
  distinct()

#Change player name to nickname for all relevant players for lineups df
unnested_lineups <- unnested_lineups %>%
  left_join(player_nicknames, by = "player.id") %>%
  mutate(player.name = coalesce(player.name.y, player.name.x)) %>%
  select(-player.name.x, -player.name.y)

#Change player name to nickname for all relevant players for event df
match_event_data <- match_event_data %>%
  left_join(player_nicknames, by = "player.id") %>%
  mutate(player.name = coalesce(player.name.y, player.name.x)) %>%
  select(-player.name.x, -player.name.y) %>%
  left_join(player_nicknames, by = c("pass.recipient.id" = "player.id")) %>%
  mutate(pass.recipient.name = player.name.y) %>%
  select(-player.name.y) %>%
  rename(player.name = player.name.x)

#----Save as RDS----

write_rds(match_event_data, file = "prem_event_data_15-16.rds")
write_rds(matches_analysed, file = "prem_matches_15-16.rds")
