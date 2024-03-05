library(rvest)
library(tidyverse)
library(dplyr)

# Read the HTML from the URL
page1 <- read_html("https://en.wikipedia.org/wiki/Norman,_Oklahoma")
page2 <- read_html("https://en.wikipedia.org/wiki/Logan,_Utah")
page3 <- read_html("https://en.wikipedia.org/wiki/Tucson,_Arizona")


# Select the table and convert it to a data frame
table1 <- page1 %>% html_table(header = FALSE)
ClimateNOK <- table1[[2]] %>% slice(-1:-2) %>% select(1, 14)
table2 <- page2 %>% html_table(header = FALSE)
ClimateLUT <- table2[[2]] %>% slice(-1:-2) %>% select(1, 14)
table3 <- page3 %>% html_table(header = FALSE)
ClimateTAZ <- table3[[2]] %>% slice(-1:-2) %>% select(1, 14)


# Add location information and bind the rows
mergedClimate <- bind_rows(
  mutate(ClimateNOK, Location = "Norman"),
  mutate(ClimateLUT, Location = "Logan"),
  mutate(ClimateTAZ, Location = "Tucson")
  )
colnames(mergedClimate) <- c("Metric", "Average", "Location")

head(mergedClimate,50)



#API setup
library(baseballr)

schedule <-mlb_schedule(season = 2024, level_ids = "1")
astros_schedule <- schedule %>% filter(teams_home_team_name == "Houston Astros" | teams_away_team_name == "Houston Astros") %>% select(date, game_pk, series_description, teams_away_team_name, teams_home_team_name, teams_away_score, teams_home_score)
astros_schedule 

