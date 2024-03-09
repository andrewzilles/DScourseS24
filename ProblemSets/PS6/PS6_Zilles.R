library(httr)
library(jsonlite)
library(tibble)
library(tools)
library(maps)
library(ggplot2)

site <- read_html("https://www.espn.com/mlb/playbyplay/_/gameId/401576366")
livetable <- site %>% html_table(header = TRUE)
livescore <- cbind(livetable[[1]], livetable[[2]])
print(livescore)

# Read the HTML from the URL
spring <- read_html("https://www.espn.com/mlb/team/schedule/_/name/hou/season/2024/seasontype/1")
half1 <- read_html("https://www.espn.com/mlb/team/schedule/_/name/hou/season/2024/seasontype/2/half/1")
half2 <- read_html("https://www.espn.com/mlb/team/schedule/_/name/hou/season/2024/seasontype/2/half/2")
post <- read_html("https://www.espn.com/mlb/team/schedule/_/name/hou/season/2024/seasontype/3")

# Extract the tables
table_sp <- spring %>% html_table(header = TRUE)
table_h1 <- half1 %>% html_table(header = TRUE)
table_h2 <- half2 %>% html_table(header = TRUE)
#table1_post <- post %>% html_table(header = TRUE)


#Extract the ESPN hyperlinks 
hyper_sp <- data.frame(link = spring %>% html_nodes("a") %>% html_attr("href") %>% grep("gameId", ., value = TRUE))
hyper_h1 <- data.frame(link = half1 %>% html_nodes("a") %>% html_attr("href") %>% grep("gameId", ., value = TRUE))
hyper_h2 <- data.frame(link = half2 %>% html_nodes("a") %>% html_attr("href") %>% grep("gameId", ., value = TRUE))
#hyper_post <- data.frame(link = post %>% html_nodes("a") %>% html_attr("href")[grep("gameId", post %>% html_nodes("a") %>% html_attr("href"))])


#...and Game IDs from the hyperlinks
GameId_sp <- data.frame(GameId = regmatches(hyper_sp$link, regexpr("\\d+", hyper_sp$link)))
GameId_h1 <- data.frame(GameId = regmatches(hyper_h1$link, regexpr("\\d+", hyper_h1$link)))
GameId_h2 <- data.frame(GameId = regmatches(hyper_h2$link, regexpr("\\d+", hyper_h2$link)))
#GameId_post <- data.frame(GameId = regmatches(hyper_post$link, regexpr("\\d+", hyper_post$link)))


#Format into a usable, mergeable dataframe
cleaned_sp <- table_sp[[1]][table_sp[[1]]$DATE != 'DATE' & !table_sp[[1]]$RESULT %in% c('Postponed', 'LIVE'), c("DATE", "OPPONENT", "RESULT")]
mergeable_sp <- data.frame(DATE = cleaned_sp$DATE, OPPONENT = cleaned_sp$OPPONENT, TIME = cleaned_sp$RESULT)
cleaned_h1 <- table_h1[[1]][table_h1[[1]]$DATE != 'DATE', c("DATE", "OPPONENT", "TIME")]
mergeable_h1 <- data.frame(DATE = cleaned_h1$DATE, OPPONENT = cleaned_h1$OPPONENT, TIME = cleaned_h1$TIME)
cleaned_h2 <- table_h2[[1]][table_h2[[1]]$DATE != 'DATE', c("DATE", "OPPONENT", "TIME")]
mergeable_h2 <- data.frame(DATE = cleaned_h2$DATE, OPPONENT = cleaned_h2$OPPONENT, TIME = cleaned_h2$TIME)
#cleaned_post <- table1_post[[1]][table1_post[[1]]$DATE != 'DATE', c("DATE", "OPPONENT", "TIME")]
#mergeable_post <- data.frame(DATE = cleaned_post$DATE, OPPONENT = cleaned_post$OPPONENT, TIME = cleaned_post$TIME)


#Merge the tables
combined_sp <- cbind(mergeable_sp, hyper_sp, GameId_sp)
combined_h1 <- cbind(mergeable_h1, hyper_h1, GameId_h1)
combined_h2 <- cbind(mergeable_h2, hyper_h2, GameId_h2)
#combined_post <- cbind(mergeable_post, hyper_post, GameId_post)


#Final schedule
Schedule <- rbind(combined_sp, combined_h1, combined_h2)#, combined_post)


#Add home/away column
Schedule$Home= toTitleCase(sapply(strsplit(sapply(strsplit(Schedule$link, "/"), function(x) tail(x, n = 1)), "-"), function(x) tail(x, n = 1)))
Schedule$Away= toTitleCase(sapply(strsplit(sapply(strsplit(Schedule$link, "/"), function(x) tail(x, n = 1)), "-"), function(x) head(x, n = 1)))
Schedule$Home <- ifelse(grepl("\\bChicago\\b", Schedule$OPPONENT), 
                        gsub("\\bSox\\b", "White Sox", Schedule$Home), 
                        ifelse(grepl("\\bBoston\\b", Schedule$OPPONENT), 
                               gsub("\\bSox\\b", "Red Sox", Schedule$Home), 
                               gsub("\\bSox\\b", "", Schedule$Home)))
Schedule$Home <- gsub("\\bJays\\b", "Blue Jays", Schedule$Home)
Schedule$Away <- gsub("\\bRed\\b", "Red Sox", Schedule$Away)
Schedule$Away <- gsub("\\bWhite\\b", "White Sox", Schedule$Away)
Schedule$Away <- gsub("\\bBlue\\b", "Blue Jays", Schedule$Away)


#Add stadium addresses
stadiums <- data.frame(
  Home = c("Diamondbacks", "Braves", "Orioles", "Red Sox", "Cubs", "White Sox", "Reds", "Guardians", "Rockies", "Tigers", "Astros", "Royals", "Angels", "Dodgers", "Marlins", "Brewers", "Twins", "Yankees", "Mets", "Athletics", "Phillies", "Pirates", "Padres", "Giants", "Mariners", "Cardinals", "Rays", "Rangers", "Blue Jays", "Nationals"),
  Stadium_Address = c("Chase Field, 401 E Jefferson St, Phoenix, AZ 85004",
                      "Truist Park, 755 Battery Ave SE, Atlanta, GA 30339",
                      "Oriole Park at Camden Yards, 333 W Camden St, Baltimore, MD 21201",
                      "Fenway Park, 4 Jersey St, Boston, MA 02215",
                      "Wrigley Field, 1060 W Addison St, Chicago, IL 60613",
                      "Guaranteed Rate Field, 333 W 35th St, Chicago, IL 60616",
                      "Great American Ball Park, 100 Joe Nuxhall Way, Cincinnati, OH 45202",
                      "Progressive Field, 2401 Ontario St, Cleveland, OH 44115",
                      "Coors Field, 2001 Blake St, Denver, CO 80205",
                      "Comerica Park, 2100 Woodward Ave, Detroit, MI 48201",
                      "Minute Maid Park, 501 Crawford St, Houston, TX 77002",
                      "Kauffman Stadium, 1 Royal Way, Kansas City, MO 64129",
                      "Angel Stadium of Anaheim, 2000 Gene Autry Way, Anaheim, CA 92806",
                      "Dodger Stadium, 1000 Vin Scully Ave, Los Angeles, CA 90012",
                      "loanDepot park, 501 Marlins Way, Miami, FL 33125",
                      "American Family Field, 1 Brewers Way, Milwaukee, WI 53214",
                      "Target Field, 1 Twins Way, Minneapolis, MN 55403",
                      "Yankee Stadium, 1 E 161st St, Bronx, NY 10451",
                      "Citi Field, 41 Seaver Way, Queens, NY 11368",
                      "Oakland Coliseum, 7000 Coliseum Way, Oakland, CA 94621",
                      "Citizens Bank Park, 1 Citizens Bank Way, Philadelphia, PA 19148",
                      "PNC Park, 115 Federal St, Pittsburgh, PA 15212",
                      "Petco Park, 100 Park Blvd, San Diego, CA 92101",
                      "Oracle Park, 24 Willie Mays Plaza, San Francisco, CA 94107",
                      "T-Mobile Park, 1250 1st Ave S, Seattle, WA 98134",
                      "Busch Stadium, 700 Clark Ave, St. Louis, MO 63102",
                      "Tropicana Field, 1 Tropicana Dr, St. Petersburg, FL 33705",
                      "Globe Life Field, 734 Stadium Drive, Arlington, TX 76011",
                      "Rogers Centre, 1 Blue Jays Way, Toronto, ON M5V 1J1",
                      "Nationals Park, 1500 S Capitol St SE, Washington, DC 20003")
)
Schedule <- merge(Schedule, stadiums, by.x = "Home", by.y = "Home", all.x = TRUE)





#create three visualizations
#1. A bar chart of the number of games played at each stadium, except for the Astros' home stadium
Colors <- data.frame(
  Team = c("Diamondbacks", "Braves", "Orioles", "Red Sox", "Cubs", "White Sox", "Reds", "Guardians", "Rockies", "Tigers", "Astros", "Royals", "Angels", "Dodgers", "Marlins", "Brewers", "Twins", "Yankees", "Mets", "Athletics", "Phillies", "Pirates", "Padres", "Giants", "Mariners", "Cardinals", "Rays", "Rangers", "Blue Jays", "Nationals"),
  Color = c("#A71930", "#CE1141", "#DF4601", "#BD3039", "#0E3386", "#27251F", "#C6011F", "#0C2340", "#33006F", "#0C2C56", "#002D62", "#004687", "#BA0021", "#005A9C", "#00A3E0", "#FFC52F", "#002B5C", "#003087", "#FFD300", "#003831", "#E81828", "#27251F", "#2F241D", "#FA4616", "#005C5C", "#C41E3A", "#092C5C", "#8FBCE6", "#134A8E", "#AB0003")
)
away_games <- Schedule %>% filter(Home != "Astros")
away_games_with_colors <- merge(away_games, Colors, by.x = "Home", by.y = "Team", all.x = TRUE)


bar_chart_stadium_with_colors <- ggplot(away_games_with_colors, aes(x = Stadium_Address, fill = Color)) +
  geom_bar() +
  scale_fill_identity() +  # Use identity scale for custom colors
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of Games Played at Each Stadium", x = "Stadium", y = "Number of Games Played")

print(bar_chart_stadium_with_colors)
ggsave("PS6a_Zilles.png", bar_chart_stadium_with_colors)



#2. A bar chart of the number of games played against each opponent
games_played = as.data.frame(subset(c(Schedule$Home, Schedule$Away), !grepl("Astros", c(Schedule$Home, Schedule$Away))))
names(games_played) <- "Team"


games_played_freq <- table(games_played$Team)
games_played_freq_df <- as.data.frame(games_played_freq)
names(games_played_freq_df) <- c("Team", "Freq")
games_played_with_colors <- merge(games_played_freq_df, Colors, by = "Team", all.x = TRUE)


bar_chart_opponent_with_colors <- ggplot(games_played_with_colors, aes(x = reorder(Team, Freq), y = Freq, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Use identity scale for custom colors
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  coord_flip() +
  labs(title = "Number of Games Played Against Each Opponent", x = "Opponent", y = "Number of Games Played")

print(bar_chart_opponent_with_colors)

ggsave("PS6b_Zilles.png", bar_chart_opponent_with_colors)



#3. A map plot of the stadiums, with the number of games played at each stadium as the size of the points
install.packages("tmap")
install.packages("tmaptools")
install.packages("leaflet")
install.packages("leaflet.extras")

library(tmap)
library(tmaptools)
library(leaflet)
library(leaflet.extras)

# Filter stadium addresses excluding those related to Astros
stadium_addresses <- ((Schedule$Stadium_Address))
stadium_addresses <- gsub("Comerica Park, 2100 Woodward Ave, Detroit, MI 48201","2100 Woodward Ave, Detroit, MI 48201", stadium_addresses)
stadium_addresses <- gsub("T-Mobile Park, 1250 1st Ave S, Seattle, WA 98134","1250 1st Ave S, Seattle, WA 98134", stadium_addresses)
stadium_addresses <- gsub("loanDepot park, 501 Marlins Way, Miami, FL 33125","Miami, FL 33125", stadium_addresses)
stadium_addresses <- gsub("Oriole Park at Camden Yards, 333 W Camden St, Baltimore, MD 21201","333 W Camden St, Baltimore, MD 21201", stadium_addresses)
stadium_addresses <- gsub("Citizens Bank Park, 1 Citizens Bank Way, Philadelphia, PA 19148","Philadelphia, PA 19148", stadium_addresses)
stadium_addresses <- gsub("Tropicana Field, 1 Tropicana Dr, St. Petersburg, FL 33705","1 Tropicana Dr, St. Petersburg, FL 33705", stadium_addresses)
stadium_addresses <- gsub("Great American Ball Park, 100 Joe Nuxhall Way, Cincinnati, OH 45202","100 Joe Nuxhall Way, Cincinnati, OH 45202", stadium_addresses)

# Geocode addresses using Nominatim from OpenStreetMap
geocoded <- tmaptools::geocode_OSM(stadium_addresses)

# Combine geocoded locations with original addresses
stadium_df <- data.frame(addresses = stadium_addresses, lon = geocoded$lon, lat = geocoded$lat)

# Create a map using leaflet
leaflet(data = stadium_df) %>%
  addTiles() %>%
  addHeatmap(lng = ~lon, lat = ~lat, blur = 20, max = 0.5)