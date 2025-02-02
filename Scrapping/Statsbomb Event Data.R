#clean environment
rm(list = ls())

#----load packages----

library(StatsBombR)
library(tidyverse)

#----Scrapping----

#pull all free competitions from Statsbomb
free_comp <- (FreeCompetitions())

#get all free prem games from 2015/16
prem_comp <- free_comp %>%
  filter(competition_id == 2 & season_id == 27)
prem_matches <- FreeMatches(prem_comp)
#get all event data
prem_data <- free_allevents(prem_matches)
prem_clean <- allclean(prem_data)
prem_lineups <- StatsBombFreeLineups(prem_matches)

#Save matches as rds
saveRDS(prem_matches, file = "prem_matches_15-16.rds")

#----Change to Nicknames----

#Remove the nests in the lineups df
unnested_lineups <- prem_lineups %>%
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

#Update the names
prem_clean <- prem_clean %>%
  # First join to update player.name using player.id
  left_join(player_nicknames, by = "player.id") %>%
  mutate(player.name = coalesce(player.name.y, player.name.x)) %>%
  select(-player.name.x, -player.name.y) %>%
  left_join(player_nicknames, by = c("pass.recipient.id" = "player.id")) %>%
  mutate(pass.recipient.name = coalesce(player.name.y, pass.recipient.name)) %>%
  select(-player.name.y) %>%
  rename(player.name = player.name.x)

#Save event data as rds
saveRDS(prem_clean, file = "prem_event_data_15-16.rds")
