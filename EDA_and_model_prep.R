#### Libraries I need and load data ####
library(tidyverse)
load('basketball_players.Rdata')

#### Merge NBA and International/Choose Variables ####

#Get common names for merge
variables <- intersect(names(international_players_clean), names(nba_players_clean)) %>% 
  intersect(names(gleague_players_clean)) # Get common variable names
vars <- variables[!variables %in% c("MP", "G", "Player_href", "Tm")] # Remove other variables that aren't needed

## Match college names
# Utah State = Utah State University
usu_index <- which(nba_players_clean$College == "Utah State University")
nba_players_clean$College[usu_index] <- "Utah State"

## These shouldn't matter since they will all be in other conference category
# UTEP = Texas-El Paso
# Iona = Iona College
# Long Beach State = Cal State Long Beach
# UCSB = UC Santa Barbara

## Merge 
all_players <- rbind(select(nba_players_clean, vars),
                     select(international_players_clean, vars),  
                     select(gleague_players_clean, vars)) %>%
  filter(!is.na(Weight), # Removed rows with empty weights or heights
         !is.na(Height),
         College != "None") # The point is to predict college players, removed professional players who weren't in NCAA D1

## Add conferences
colleges <- all_players$College %>% unique() %>% sort() # create list of colleges
sec <- c("Kentucky", "Florida", "Georgia", "Missouri", "South Carolina", "Tennessee", "Vanderbilt",
         "Alabama", "Arkansas", "Auburn", "LSU", "Ole Miss", "Mississippi State", "Texas A&M")
big10 <- c("Indiana", "Maryland", "Michigan", "Michigan State", "Ohio State", "Penn State", "Rutgers",
           "Illinois", "Iowa", "Minnesota", "Nebraska", "Northwestern", "Purdue", "Wisconsin")
big12 <- c("Baylor", "Iowa State", "Kansas", "Kansas State", "Oklahoma", "Oklahoma State", "TCU", "Texas Tech",
           "West Virginia", "Texas")
acc <- c("Boston College", "Clemson", "Florida State", "NC State", "Notre Dame", "Syracuse", "Wake Forest",
         "Duke", "Georgia Tech", "Miami (FL)", "UNC", "Pitt", "Virginia", "Virginia Tech", "Louisville")
pac12 <- c("Arizona", "Arizona State", "California", "UCLA", "Oregon", "Oregon State", "USC", "Stanford", "Utah",
           "Washington", "Washington State", "Colorado")
bigeast <- c("Butler", "UConn", "Creighton", "DePaul", "Georgetown", "Marquette", "Providence", "St. John's",
             "Seton Hall", "Villanova", "Xavier")
wcc <- c("BYU", "Gonzaga", "Loyola Marymount", "Pacific", "Pepperdine", "Portland", "Saint Mary's", "San Diego",
         "San Francisco", "Santa Clara")
american <- c("Central Florida", "Cincinnati", "East Carolina University", "Houston", "Memphis", "USF", "Temple",
              "Tulane", "Tulsa", "Wichita State", "SMU")
a10 <- c("St. Bonaventure", "Davidson", "Dayton", "Duquesne", "Fordham", "George Mason", "George Washington",
         "La Salle", "UMass", "Rhode Island", "Richmond", "Saint Joseph's", "Saint Louis", "VCU")
mw <- c("Air Force", "Boise State", "Fresno State", "Colorado State", "Nevada", "UNLV", "New Mexico",
        "San Diego State", "San Jose State", "Utah State", "Wyoming")
## Create list of colleges in specified conferences
colleges_list <- c(sec, big10, big12, acc, pac12, bigeast, wcc, american, a10, mw) 
## Create df. One column is college name. The other is conference variable assigned
colleges_df <- rbind(data.frame(school = sec, conference = "SEC"),
                     data.frame(school = big10, conference = "BIG10"),
                     data.frame(school = big12, conference = "BIG12"),
                     data.frame(school = acc, conference = "ACC"),
                     data.frame(school = pac12, conference = "PAC12"),
                     data.frame(school = bigeast, conference = "BIG_EAST"),
                     data.frame(school = wcc, conference = "WCC"),
                     data.frame(school = american, conference = "American"),
                     data.frame(school = a10, conference = "A10"),
                     data.frame(school = mw, conference = "MW")) 

## Replace College variable with conference name or none
all_players$Conference <- lapply(X = all_players$College, 
                                 function(x) {ifelse(x %in% colleges_list, 
                                                     colleges_df$conference[which(colleges_df$school == x)], 
                                                     "Other")}) %>% 
  as.character() # convert each list to character

players_model <- select(all_players, -College)

#### Save ####
save(players_model, file = "C:/Users/18592/Documents/BYU/STATS/STAT 495R Sports/Project/players_model.Rdata")

#### Get international NBA draftee data ####
url <- "https://en.wikipedia.org/wiki/List_of_foreign_NBA_drafted_players"
dat <- url %>% 
  rvest::read_html() %>% 
  rvest::html_table()

tab <- dat[[5]] %>% # 5th table is the one I want
  dplyr::group_by(Draft) %>% 
  dplyr::summarise(Count = n())

ggplot(data = tab, mapping = aes(x = Draft, y = Count)) + # Create plot 
  geom_col() +
  theme_bw() + 
  ggtitle("Foreign NBA Drafted Players") + 
  ylab("Number of Foreign Players Drafted")
