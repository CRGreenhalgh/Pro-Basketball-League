#### Libraries I need ####
library(tidyverse)
library(rvest)
library(xml2)
library(rjson)
library(lubridate)

#### Scrape NBA Data ####
url <- "https://www.basketball-reference.com/players/"
url2 <- "https://www.basketball-reference.com"
nba_players_df <- data.frame()
nba_players_error <- data.frame()

tictoc::tic() #Start stopwatch
for(i in c(1:23,25:26)) {
  ## Outer loop to collect hrefs for nba players per first letter of last name 
  nba_player_list <- paste0(url, letters[i], "/") %>% 
    rvest::read_html()
  
  nba_player_df <-  nba_player_list %>%
    rvest::html_table()
  
  nba_player_href <-  nba_player_list %>% 
    rvest::html_nodes("th") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  
  ## Include data since 2016-2017 season
  nba_recent_href <- nba_player_href[nba_player_df[[1]]$To >= 2016]
  
  ## Collect name, height, weight, and college vectors
  nba_recent_name <- nba_player_df[[1]] %>%
    dplyr::filter(To >= 2016) %>%
    pull(Player)
  
  nba_recent_heights <- nba_player_df[[1]] %>%
    dplyr::filter(To >= 2016) %>%
    pull(Ht)
  
  nba_recent_weights <- nba_player_df[[1]] %>%
    dplyr::filter(To >= 2016) %>%
    pull(Wt)
  
  nba_recent_colleges <- nba_player_df[[1]] %>%
    dplyr::filter(To >= 2016) %>%
    pull(Colleges)
  
  ## Inner loop for getting basketball data per player for each letter
  for(j in 1:length(nba_recent_href)) {
    ## The per 36 minutes table was embedded in the comments for some reason
    player_page <- try(paste0(url2, nba_recent_href[j], "/") %>% 
                         rvest::read_html() %>% 
                         rvest::html_nodes(xpath = '//comment()') %>%    # select comments
                         rvest::html_text() %>%    # extract comment text
                         paste(collapse = '') %>%    # collapse to single string
                         rvest::read_html() %>% 
                         rvest::html_nodes(xpath = '//*[@id="per_minute"]') %>% 
                         rvest::html_table())
    
    player_table <- try(player_page[[1]] %>%
                          dplyr::slice(1:which(Season == "Career") - 1) %>% # Remove career row
                          group_by(Season) %>% # Players that were traded have multiple rows for same season. 
                          dplyr::slice(1) %>%  # Just need the first one
                          ungroup() %>%
                          ## Create variables for each players name, href, weight, height, college
                          mutate(Player = nba_recent_name[j],
                                 Player_href = nba_recent_href[j],
                                 Weight = nba_recent_weights[j],
                                 Height = nba_recent_heights[j],
                                 College = ifelse(nba_recent_colleges[j] == "", # Players with no college are coded as empty characters
                                                  "None",
                                                  str_extract(nba_recent_colleges[j], '\\b[^,]+$'))))
    
    ## If statement for separating successful scrapes
    if (class(player_table)[1] == "try-error") { # If no error there are 3 classes
      # Create df for players that had an error scraping. I got some errors at first so I could record who it was 
      error_df <- data.frame("Player" = nba_recent_name[j],
                             "href" = nba_recent_href[j],
                             "i" = i,
                             "j" = j)
      nba_players_error <- rbind(nba_players_error, error_df)
    } else {
      ## If statement within the else statement includes players that were scraped successfully
      ## If statement to match column names
      if (any(names(player_table) != names(nba_players_df))) {
        names(player_table)[which(names(player_table) != names(nba_players_df))] <- names(nba_players_df)[which(names(player_table) != names(nba_players_df))]
      }
      nba_players_df <- rbind(nba_players_df, player_table) # Bind df to create all the players data together
    }
    # print(j)
  }
  print(letters[i])
}
tictoc::toc() ## End Stopwatch
beepr::beep()

#### Clean NBA Players ####
names(which(colSums(is.na(nba_players_df)) > 0))
nba_players_clean <- nba_players_df %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% # Replace NAs with 0s
  ## Remove seasons before 2016-2017. 
  ## Removed players who never played since 2016-2017 but now remove current players seasons before 2016-2017
  filter(as.numeric(str_sub(nba_players_df$Season, - 2)) %in% 17:21) %>% 
  separate(Height, c("Feet", "Inches"), "-") %>% 
  ## Separate height into feet and inches then recreate height in total inches
  mutate(Feet = as.numeric(Feet),
         Inches = as.numeric(Inches),
         Height = Feet*12 + Inches) %>% 
  select(-c(Feet,Inches)) # Drop feet and inches

#### Scrape International Data ####
url <- "https://www.basketball-reference.com/international/players/"
url2 <- "https://www.basketball-reference.com"
international_player_href <- list()
international_players_df <- data.frame()
player_dfs <- list()
leagues <- c("Liga ACB", "LNB Pro A", "Lega Serie A", "Greek Basket League", 
             "Israeli Super League", "Turkish Super League", "ABA Adriatic", 
             "VTB United", "NBL Australia", "CBA China")#, "EuroLeague", "EuroCup") Leagues I will include

tictoc::tic()
for(i in 1:26) {
  ## Outer loop to collect hrefs for each international player per first letter of last name
  international_player_list <- paste0(url, letters[i], "/") %>% 
    rvest::read_html()
  
  players_href <- international_player_list %>%
    rvest::html_nodes(xpath='//*[@id="content"]/p/a[@href]') %>% 
    rvest::html_attr("href")
  
  for(j in 1:length(players_href)) {
    ## Inner loop to collect data for each basketball player per letter
    
    ## Different players have different ids for per 36 minutes data
    ## Probably better way to do this but this was the simplest for me ##
    player_page <- try(paste0(url2, players_href[j], "/") %>% 
                         rvest::read_html() %>% 
                         rvest::html_nodes(xpath = '//*[@id="player-stats-per_minute-all-"]') %>% 
                         rvest::html_table())
    
    if(length(player_page) == 0) { # If previous attempt didn't return anything
      player_page <- try(paste0(url2, players_href[j], "/") %>% 
                           rvest::read_html() %>% 
                           rvest::html_nodes(xpath = '//*[@id="player-stats-per_minute-league-"]') %>% 
                           rvest::html_table())
    }
    
    if(length(player_page) == 0) { # If previous attempts didn't return anything
      player_page <- try(paste0(url2, players_href[j], "/") %>% 
                           rvest::read_html() %>% 
                           rvest::html_nodes(xpath = '//*[@id="player-stats-per_minute-league-_p"]') %>% 
                           rvest::html_table())
    }
    
    ## Only use player_pages that successfully collected data
    if(length(player_page) != 0 & class(player_page) != "try-error") {  
      player_df <- player_page[[1]]
      
      ## Empty variable name which is a flag
      names(player_df)[which(names(player_df) == "")] <- "Flag"
      
      international_player_df <- player_df %>%
        filter(!grepl("Season", player_df$Season)) # Remove season rows for each player
      
      ## Nested loop to only include players who have played at least one season since 2016-2017
      if(any(as.numeric(str_sub(international_player_df$Season, -2)) %in% 17:21)) {
        
        ## Collect height, weight, and college data
        height <- try(paste0(url2, players_href[j], "/") %>%
                        rvest::read_html() %>%
                        rvest::html_nodes(xpath = '//*[@itemprop="height"]') %>% 
                        html_text(), silent = TRUE)
        
        weight <- try(paste0(url2, players_href[j], "/") %>%
                        rvest::read_html() %>%
                        rvest::html_nodes(xpath = '//*[@itemprop="weight"]') %>% 
                        html_text(), silent = TRUE)
        
        college <- try(paste0(url2, players_href[j], '/') %>%
                         rvest::read_html() %>%
                         rvest::html_nodes(xpath = '//*[@id="meta"]') %>%
                         rvest::html_nodes("a") %>% 
                         rvest::html_attr("href") %>% 
                         .[grepl("cbb",.)] %>% # Get the player's college stats site
                         rvest::read_html() %>% 
                         rvest::html_nodes(xpath = '//*[@id="meta"]') %>%
                         rvest::html_nodes("a") %>% 
                         rvest::html_text(), silent = TRUE) # Extract college name
        
        temp <- international_player_df %>% 
          filter(as.numeric(str_sub(international_player_df$Season, -2)) %in% 17:21,
                 League %in% leagues) %>% # filter out players not in selected leagues
          mutate(Player_href = players_href[j],
                 Height = ifelse(class(height) == "character", height, NA),
                 Weight = ifelse(class(weight) == "character", weight, NA),
                 College = ifelse(is.vector(college), tail(college,1), "None")) # get the most recent college
        
        international_players_df <- rbind(international_players_df, temp) # Bind df to get all players data together
      }
    }
  }
  print(letters[i])
  
  international_player_href <- append(international_player_href, players_href) # Get list of all hrefs
}  
tictoc::toc()
# beepr::beep()
#### Clean International Data ####
international_players_clean <- international_players_df %>% 
  filter(!is.na(FG)) %>% #Players that had NA FG also had NAs or 0s for every variable
  mutate_at(vars(`FG%`, `2P%`, `3P%`, `FT%`), ~replace_na(., 0)) %>% # Convert these NAs to 0s
  separate(Height, c("Feet", "Inches"), "-") %>% # Recreate height like before
  mutate(Feet = as.numeric(Feet),
         Inches = as.numeric(Inches),
         Height = Feet*12 + Inches,
         Weight = as.numeric(gsub("lb", "", Weight))) %>% 
  select(-c(Feet,Inches)) %>% 
  mutate(College = str_extract(College, '\\b[^,]+$')) # Get the last college; removes previous colleges if they exist

names(which(colSums(is.na(international_players_clean)) > 0)) # Check if NAs are still present

## Keep predictor names consistent
names(international_players_clean)[which(names(international_players_clean) == "League")] <- "Lg"
names(international_players_clean)[which(names(international_players_clean) == "Team")] <- "Tm"
#### Scrape G-League Data ####
url <- "https://www.basketball-reference.com/gleague/players/"
url2 <- "https://www.basketball-reference.com"
gleague_player_href <- list()
gleague_players_df <- data.frame()
empty_list <- list()
player_dfs <- list()

tictoc::tic()
for(i in c(1:23,25,26)) {
  ## Outer loop to get hrefs for each letter of the alphabet
  gleague_player_list <- paste0(url, letters[i], "/") %>%
    rvest::read_html()
  
  players_href <- gleague_player_list %>%
    rvest::html_nodes(xpath='//*[@id="content"]/p/a[@href]') %>%
    rvest::html_attr("href")
  
  for(j in 1:length(players_href)) {
    ## Inner loop to get data for each player per letter
    ## Similar to NBA need to go through comments
    player_page <- try(paste0(url2, players_href[j], "/") %>% 
                         rvest::read_html() %>% 
                         rvest::html_nodes(xpath = '//comment()') %>%    # select comments
                         rvest::html_text() %>%    # extract comment text
                         paste(collapse = '') %>%    # collapse to single string
                         rvest::read_html() %>% 
                         rvest::html_nodes(xpath = '//*[@id="nbdl_per_minute"]') %>% 
                         rvest::html_table())
    
    ## Look through successful scrapes again
    if(length(player_page) != 0 & class(player_page) != "try-error") {  
      player_df <- player_page[[1]]
      
      gleague_player_df <- player_df %>%
        filter(!grepl("Career", player_df$Season)) %>% # Remove career rows for each player
        mutate(Season = substr(Season, 1, 7)) # Some players have star symbol so subset the Season variable
      
      ## Get the current data for the time frame again
      if(any(as.numeric(str_sub(gleague_player_df$Season, -2)) %in% 17:21)) {
        
        ## Get height, weight, and college variables
        height <- try(paste0(url2, players_href[j], "/") %>%
                        rvest::read_html() %>%
                        rvest::html_nodes(xpath = '//*[@itemprop="height"]') %>% 
                        rvest::html_text(), silent = TRUE)
        
        weight <- try(paste0(url2, players_href[j], "/") %>%
                        rvest::read_html() %>%
                        rvest::html_nodes(xpath = '//*[@itemprop="weight"]') %>% 
                        rvest::html_text(), silent = TRUE)
        
        college <- try(paste0(url2, players_href[j], '/') %>%
                         rvest::read_html() %>%
                         rvest::html_nodes(xpath = '//*[@id="meta"]') %>%
                         rvest::html_nodes("a") %>% 
                         rvest::html_attr("href") %>% # get college href
                         .[grepl("cbb",.)] %>%  
                         rvest::read_html() %>% 
                         rvest::html_nodes(xpath = '//*[@id="meta"]') %>%
                         rvest::html_nodes("a") %>% 
                         rvest::html_text(), silent = TRUE) # extract college name
        
        temp <- gleague_player_df %>%
          group_by(Season) %>% ## Players that were traded have multiple rows for same season. Just need the first one
          dplyr::slice(1) %>%
          ungroup() %>% 
          filter(as.numeric(str_sub(Season, -2)) %in% 17:21) %>% # remove players who had some previous season not in timeframe
          mutate(Player_href = players_href[j],
                 Height = ifelse(length(height) != 0, height, NA),
                 Weight = ifelse(length(weight) != 0, weight, NA),
                 College = ifelse(is.vector(college), tail(college,1), "None")) # use the most recent college
        
        gleague_players_df <- rbind(gleague_players_df, temp) # bind all data together
      }
    }
    # if(j %% 25 == 0) {
    #   print(j)
    # }
  }
  print(letters[i])
  
  gleague_player_href <- append(gleague_player_href, players_href) # create list of hrefs
}  
tictoc::toc()
beepr::beep()
#### Clean G-League Data ####
gleague_players_clean <- gleague_players_df %>% 
  filter(!is.na(FG)) %>% # Similar to international data with NAs
  mutate_at(vars(`FG%`, `2P%`, `3P%`, `FT%`), ~replace_na(., 0)) %>% 
  separate(Height, c("Feet", "Inches"), "-") %>% # Separate height and weight again
  mutate(Feet = as.numeric(Feet),
         Inches = as.numeric(Inches),
         Height = Feet*12 + Inches,
         Weight = as.numeric(gsub("lb", "", Weight)), # Same as international data: remove lb and turn to numeric
         Lg = "G-League") %>% # Create league variable
  select(-c(Feet,Inches)) 

names(which(colSums(is.na(gleague_players_clean)) > 0)) # Check if any variables have NAs

## Get consistent variable names
names(gleague_players_clean)[which(names(gleague_players_clean) == "League")] <- "Lg"
names(gleague_players_clean)[which(names(gleague_players_clean) == "Team")] <- "Tm"
#### Save Data ####
save(nba_players_clean, international_players_clean, gleague_players_clean, 
     file = "C:/Users/18592/Documents/BYU/STATS/STAT 495R Sports/Project/basketball_players.Rdata")

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

#### Libraries and Load ####
library(tidyverse)
library(caret)
library(xgboost)
library(ranger)
library(e1071)
library(corrplot)
library(fastDummies)
library(randomForest)
library(naivebayes)
library(kknn)
library(gbm)
load('players_model.Rdata')

#### Split ####

## Temporarily Remove NBA
# players_model <- players_model %>%
#   filter(Lg != "NBA")

basketball <- players_model %>% 
  filter(as.numeric(str_sub(Season, -2)) <= 21) %>% 
  select(-Season) 

# set.seed(495)
test_index <- sample(1:nrow(basketball), round(.1*nrow(basketball))) # Create test index
basketball_test <- basketball %>%  # create test
  dplyr::slice(test_index)
basketball_train <- basketball %>% # create train
  dplyr::slice(-test_index) %>%
  mutate(Lg = as.factor(Lg))

#### Pre-Processing ####

## Check Multi-Collinearity
correlation_matrix <- cor(select(basketball_train, -c(Lg,Conference))) # create matrix
corrplot::corrplot.mixed(correlation_matrix, 
                         tl.col = "black",
                         tl.cex = .50,
                         number.cex=0.65) # View plot
summary(correlation_matrix[upper.tri(correlation_matrix)])
highlyCorDescr <- findCorrelation(correlation_matrix, cutoff = .75) # get variables that are highly correlated
## Remove variables that have correlation
keep <- names(basketball_train)[!(names(basketball_train) %in% colnames(correlation_matrix)[highlyCorDescr])]
basketball_train_nocorr <- select(basketball_train, all_of(keep))
correlation_matrix <- cor(select(basketball_train_nocorr, -c(Lg,Conference)) %>% # Create new matrix
                            rename(#Ht = Height,
                              Wt = Weight))
highlyCorDescr <- findCorrelation(correlation_matrix, cutoff = .75) # Should be empty at it is
corrplot::corrplot.mixed(correlation_matrix, 
                         tl.col = "black",
                         tl.cex = .50,
                         number.cex=0.65) # View plot
### Center and Scale
preProcValues <- caret::preProcess(basketball_train_nocorr) # get preprocess function
basketball_train_nocorr_cs <- predict(preProcValues, basketball_train_nocorr) # apply preprocessing to data

### Create Dummy Variables
basketball_train_nocorr_cs_dummy <- fastDummies::dummy_cols(basketball_train_nocorr_cs,
                                                            select_columns = 'Conference',
                                                            remove_selected_columns = TRUE) # Create dummies for confs

### Fit Preprocessing for Test Set
basketball_test_preprocessed <- select(basketball_test, all_of(keep)) %>% # remove correlated vars from test
  predict(preProcValues,.) %>% # apply preprocessing 
  fastDummies::dummy_cols('Conference',
                          remove_selected_columns = TRUE) # create dummies for test

#### Set Seed ####
# set.seed(1512)
#### XGBoost ####
### Ran this originally and it did slightly better than RF but then got some errors and warnings I've never seen before
### One was saying caret has no xgboost method and the warning was something with amalgamation
# tictoc::tic()
# tc <- trainControl(method = "cv", number = 10)
# # #
# tg <- expand.grid(nrounds = c(100,200),
#                   max_depth = c(10, 15, 20, 25),
#                   colsample_bytree = seq(0.5, 0.9, length.out = 5),
#                   ## The values below are default values in the sklearn-api. 
#                   eta = 0.1,
#                   gamma=0,
#                   min_child_weight = 1,
#                   subsample = 1
# )
# 
# basketball_xgb <- caret::train(Lg ~.,
#                      data = basketball_train_nocorr_cs_dummy,
#                      method = "xgboost",
#                      trControl = tc,
#                      tuneGrid = tg,
#                      tuneLength = 6)
# tictoc::toc()
# beepr::beep()

#### Results
# plot(basketball_xgb)
# varImp(basketball_xgb)
# basketball_xgb$results
# basketball_xgb$bestTune
# 
# #### Predicts
# xgb_preds1 <- predict(basketball_xgb, newdata = basketball_test_preprocessed)
# xgb_preds_df1 <- data.frame(Lg = basketball_test_preprocessed$Lg, target = xgb_preds1)

# xgb_preds_oot <- predict(basketball_xgb, newdata = basketball_oot)
# xgb_preds_df_oot <- data.frame(Lg = basketball_test$Lg, target = xgb_preds_oot)

# accuracy_xgb <- xgb_preds_df1 %>% 
#   group_by(Lg) %>% 
#   summarise(mean(target == Lg))

#### RandomForest ####
tictoc::tic()
basketball_rf <- train(form = Lg~.,
                       data = basketball_train_nocorr_cs_dummy,
                       method = "rf",
                       tuneGrid = expand.grid(
                         mtry = 14
                       ),
                       trControl = trainControl(method="repeatedcv",
                                                number=10, #Number of pieces of your data
                                                repeats=1) #repeats=1 = "cv"
)
tictoc::toc()
beepr::beep()

plot(basketball_rf) # view plot to see accuracy
varImp(basketball_rf) 
basketball_rf$results %>% arrange(desc(Accuracy))
rf_preds1 <- predict(basketball_rf, newdata = basketball_test_preprocessed)#, type = "prob") # uncomment to see probs
rf_preds_df1 <- data.frame(Lg = basketball_test_preprocessed$Lg, target = rf_preds1)

accuracy_rf <- rf_preds_df1 %>% # build accuracy df per 
  group_by(Lg) %>%
  summarise(Accuracy = mean(target == Lg),
            Count = n())

confusionMatrix(basketball_rf) # build confusion matrix

#### Naive Bayes ####
### Same template as RF
tictoc::tic()
tc <- trainControl(method = "cv", number = 10)

tg <- expand.grid(laplace = TRUE,
                  usekernel = 1,
                  adjust = seq(0.25,2,.25)) 

basketball_nb <- train(form = Lg ~.,
                       data = basketball_train_nocorr_cs_dummy, 
                       method = "naive_bayes",
                       trControl = tc,
                       tuneGrid = tg,
                       tuneLength = 6)  
tictoc::toc()
beepr::beep()

plot(basketball_nb)
varImp(basketball_nb)
basketball_nb$results %>% arrange(desc(Accuracy))
nb_preds1 <- predict(basketball_nb, newdata = basketball_test_preprocessed)
nb_preds_df1 <- data.frame(Lg = basketball_test_preprocessed$Lg, target = nb_preds1)

accuracy_nb <- nb_preds_df1 %>%
  group_by(Lg) %>%
  summarise(Accuracy = mean(target == Lg))

#### K-Nearest Neighbors ####
### Same template as RF
tictoc::tic()
tc <- trainControl(method = "cv", number = 10)

tg <- expand.grid(kmax = 30,
                  distance = 2,
                  kernel = c('optimal', 'gaussian', 'rectangular', 'triangular','epanechnikov',
                             'biweight','triweight','cos','inv')) 

basketball_knn <- train(form = Lg ~.,
                        data = basketball_train_nocorr_cs_dummy, 
                        method = "kknn",
                        trControl = tc,
                        tuneGrid = tg,
                        tuneLength = 4)  
tictoc::toc()
beepr::beep()

plot(basketball_knn)
varImp(basketball_knn)
basketball_knn$results %>% arrange(desc(Accuracy))
knn_preds1 <- predict(basketball_knn, newdata = basketball_test_preprocessed)
knn_preds_df1 <- data.frame(Lg = basketball_test_preprocessed$Lg, target = knn_preds1)

accuracy_knn <- knn_preds_df1 %>%
  group_by(Lg) %>%
  summarise(Accuracy = mean(target == Lg),
            Count = n())
#### Save Models ####
save(basketball_xgb, basketball_rf, basketball_nb, basketball_knn,
     file = "C:/Users/18592/Documents/BYU/STATS/STAT 495R Sports/Project/models.Rdata")

