#### Libraries and Load ####
library(tidyverse)
library(caret)
library(xgboost)
library(ranger)
library(e1071)
#load('players_dfs.Rdata')
load('basketball_players.Rdata')

#### Merge NBA and International/Choose Variables ####

#Get common names for merge
variables <- intersect(names(international_players_clean), names(nba_players_clean))
vars <- variables[!variables %in% c("G", "MP", "Player_href", "Tm")]

#Merge and split
basketball <- rbind(nba_players_clean %>% 
                      select(vars), 
                    international_players_clean %>% 
                      select(vars)) %>% 
  filter(as.numeric(str_sub(Season, -2)) < 21) %>% 
  select(-Season)

basketball_oot <- rbind(nba_players_clean %>% 
                            select(vars), 
                          international_players_clean %>% 
                            select(vars)) %>% 
  filter(as.numeric(str_sub(Season, -2)) == 21) %>% 
  select(-Season)

test_index <- sample(1:nrow(basketball), round(.1*nrow(basketball)))
basketball_test <- basketball[test_index,] 
basketball_test_dropped <- basketball_test %>% 
  select(-Lg) #%>% view()
basketball_train <- basketball[-test_index,]
basketball_train$Lg <- as.factor(basketball_train$Lg)

#### Set Seed ####
set.seed(0511)
#### XGBoost ####
tictoc::tic()
tc <- trainControl(method = "cv", number = 10)

tg <- expand.grid(nrounds = seq(90,110, by = 5),
                  max_depth = 3:8,
                  eta = seq(.1, .3, by = .05),
                  gamma = 0,
                  subsample = 1,
                  colsample_bytree = 1,
                  min_child_weight = 1) 

basketball_xgb <- train(form = Lg ~.,
                     data = basketball_train, #%>% select(-id),
                     method = "xgbTree",
                     trControl = tc,
                     tuneGrid = tg,
                     tuneLength = 6)  
tictoc::toc()
beepr::beep()

#### Results
plot(basketball_xgb)
varImp(basketball_xgb)
basketball_xgb$results
basketball_xgb$bestTune

#### Predicts
xgb_preds1 <- predict(basketball_xgb, newdata = basketball_test_dropped)
xgb_preds_df1 <- data.frame(Lg = basketball_test$Lg, target = xgb_preds1)

#### Save
save(nba_players_clean, international_players_df, basketball, basketball_xgb, file = "basketball_players.Rdata")
  
#### RandomForest ####
tictoc::tic()
basketball_rf <- train(form = Lg~.,
                    data = basketball_train,
                    method = "ranger",
                    tuneGrid = expand.grid(
                      min.node.size = 1:5,
                      mtry = 3:6,
                      splitrule = "gini"
                    ),
                    trControl = trainControl(method="repeatedcv",
                                           number=10, #Number of pieces of your data
                                           repeats=3) #repeats=1 = "cv"
)
tictoc::toc()
beepr::beep()

plot(basketball_rf)
basketball_rf$results %>% as.data.frame() %>% arrange(desc(Accuracy))
rf_preds1 <- predict(basketball_rf, newdata = basketball_oot)
rf_preds_df1 <- data.frame(id = basketball_train$Lg, target = rf_preds1)

# write_csv(x = rf_preds_df1, path = "./RFPreds1.csv")