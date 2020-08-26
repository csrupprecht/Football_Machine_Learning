library(readr)
library(parallel)
library(dplyr)
library(tidyverse)
library(rvest)
library(purrr)
library(stringr)
library(stringi)
library(tidymodels)
library(corrplot)

FF_WR_Data_Filled_In <- read_csv("https://docs.google.com/spreadsheets/d/1WjoBaEZs5ZFV3eiBB22jWMtJ9gZ9hGQAcDyjBjwTBpU/edit?usp=sharing")
FF_RB_Data_Filled_In <- read_csv("https://docs.google.com/spreadsheets/d/1Oev8zF1YV6yWOMq_5LKEKLY4F6efBKO2RxCz-uKsXXo/edit?usp=sharing")
FF_TE_Data_Filled_In <- read_csv("https://docs.google.com/spreadsheets/d/1M0bZb3b-ZPWTLXZtL5vBmbff-LypimENtukcU15DSnU/edit?usp=sharing")
FF_QB_Data_Filled_In <- read_csv("https://docs.google.com/spreadsheets/d/1vCI8hfCk7yZWRCgxyA7FegNwWKfZH5DhRLzF8Jj9ZgQ/edit?usp=sharing")

FF_WR_Data_Filled_In_No_NAs <- FF_WR_Data_Filled_In[!is.na(FF_WR_Data_Filled_In$Height),]

FF_RB_Data_Filled_In_No_NAs <- FF_RB_Data_Filled_In[!is.na(FF_RB_Data_Filled_In$Height),]

FF_TE_Data_Filled_In_No_NAs <- FF_TE_Data_Filled_In[!is.na(FF_TE_Data_Filled_In$Height),]

FF_QB_Data_Filled_In_No_NAs <- FF_QB_Data_Filled_In[!is.na(FF_QB_Data_Filled_In$Height),]

FF_WR_Data_Filled_In_No_NAs <- select(FF_WR_Data_Filled_In_No_NAs, -X1, -Team, -Position, -'2PM', -'2PP', -FantPt, -DKPt, -FDPt, -VBD, -'Yearly PosRank', -'Yearly OvRank', -'Draft Year', -Pos)

FF_RB_Data_Filled_In_No_NAs <- select(FF_RB_Data_Filled_In_No_NAs, -X1, -Team, -Position, -'2PM', -'2PP', -FantPt, -DKPt, -FDPt, -VBD, -'Yearly PosRank', -'Yearly OvRank', -'Draft Year', -Pos)

FF_TE_Data_Filled_In_No_NAs <- select(FF_TE_Data_Filled_In_No_NAs, -X1, -Team, -Position, -'2PM', -'2PP', -FantPt, -DKPt, -FDPt, -VBD, -'Yearly PosRank', -'Yearly OvRank', -'Draft Year', -Pos)

FF_QB_Data_Filled_In_No_NAs <- select(FF_QB_Data_Filled_In_No_NAs, -X1, -Team, -Position, -'2PM', -'2PP', -FantPt, -DKPt, -FDPt, -VBD, -'Yearly PosRank', -'Yearly OvRank', -'Draft Year', -Pos)

FF_WR_Data_2020 <- FF_WR_Data_Filled_In_No_NAs %>% filter(`Fantasy Year` == 2019)
FF_WR_Data_2020$`Fantasy Year` <- FF_WR_Data_2020$`Fantasy Year` + 1
FF_WR_Data_2020$Age <- FF_WR_Data_2020$Age + 1
FF_WR_Data_2020[,4:23] <- NA
FF_WR_Data_Filled_In_No_NAs <- bind_rows(FF_WR_Data_Filled_In_No_NAs, FF_WR_Data_2020)

FF_RB_Data_2020 <- FF_RB_Data_Filled_In_No_NAs %>% filter(`Fantasy Year` == 2019)
FF_RB_Data_2020$`Fantasy Year` <- FF_RB_Data_2020$`Fantasy Year` + 1
FF_RB_Data_2020$Age <- FF_RB_Data_2020$Age + 1
FF_RB_Data_2020[,4:23] <- NA
FF_RB_Data_Filled_In_No_NAs <- bind_rows(FF_RB_Data_Filled_In_No_NAs, FF_RB_Data_2020)

FF_TE_Data_2020 <- FF_TE_Data_Filled_In_No_NAs %>% filter(`Fantasy Year` == 2019)
FF_TE_Data_2020$`Fantasy Year` <- FF_TE_Data_2020$`Fantasy Year` + 1
FF_TE_Data_2020$Age <- FF_TE_Data_2020$Age + 1
FF_TE_Data_2020[,4:23] <- NA
FF_TE_Data_Filled_In_No_NAs <- bind_rows(FF_TE_Data_Filled_In_No_NAs, FF_TE_Data_2020)

FF_QB_Data_2020 <- FF_QB_Data_Filled_In_No_NAs %>% filter(`Fantasy Year` == 2019)
FF_QB_Data_2020$`Fantasy Year` <- FF_QB_Data_2020$`Fantasy Year` + 1
FF_QB_Data_2020$Age <- FF_QB_Data_2020$Age + 1
FF_QB_Data_2020[,4:23] <- NA
FF_QB_Data_Filled_In_No_NAs <- bind_rows(FF_QB_Data_Filled_In_No_NAs, FF_QB_Data_2020)

FF_WR_Data_Filled_In_No_NAs <- FF_WR_Data_Filled_In_No_NAs %>% group_by(Player)
FF_RB_Data_Filled_In_No_NAs <- FF_RB_Data_Filled_In_No_NAs %>% group_by(Player)
FF_TE_Data_Filled_In_No_NAs <- FF_TE_Data_Filled_In_No_NAs %>% group_by(Player)
FF_QB_Data_Filled_In_No_NAs <- FF_QB_Data_Filled_In_No_NAs %>% group_by(Player)

FF_WR_Data_Filled_In_No_NAs <- FF_WR_Data_Filled_In_No_NAs[order(FF_WR_Data_Filled_In_No_NAs$Player, FF_WR_Data_Filled_In_No_NAs$Age),]

FF_RB_Data_Filled_In_No_NAs <- FF_RB_Data_Filled_In_No_NAs[order(FF_RB_Data_Filled_In_No_NAs$Player, FF_RB_Data_Filled_In_No_NAs$Age),]

FF_TE_Data_Filled_In_No_NAs <- FF_TE_Data_Filled_In_No_NAs[order(FF_TE_Data_Filled_In_No_NAs$Player, FF_TE_Data_Filled_In_No_NAs$Age),]

FF_QB_Data_Filled_In_No_NAs <- FF_QB_Data_Filled_In_No_NAs[order(FF_QB_Data_Filled_In_No_NAs$Player, FF_QB_Data_Filled_In_No_NAs$Age),]

FF_WR_Data_Filled_In_No_NAs$PPG <- FF_WR_Data_Filled_In_No_NAs$PPR/FF_WR_Data_Filled_In_No_NAs$`Games Played`

FF_RB_Data_Filled_In_No_NAs$PPG <- FF_RB_Data_Filled_In_No_NAs$PPR/FF_RB_Data_Filled_In_No_NAs$`Games Played`

FF_TE_Data_Filled_In_No_NAs$PPG <- FF_TE_Data_Filled_In_No_NAs$PPR/FF_TE_Data_Filled_In_No_NAs$`Games Played`

FF_QB_Data_Filled_In_No_NAs$PPG <- FF_QB_Data_Filled_In_No_NAs$PPR/FF_QB_Data_Filled_In_No_NAs$`Games Played`

cor_WR_data <- cor(FF_WR_Data_Filled_In_No_NAs[,3:length(FF_WR_Data_Filled_In_No_NAs)], use = 'pairwise.complete.obs')
FF_WR_ML_Data <- FF_WR_Data_Filled_In_No_NAs %>% select(Player, `Fantasy Year`, Age, `Games Played`, `Games Started`, Targets, Receptions, `Receiving Yards`, `Receiving TDs`, `Total TDs`, PPR, Rnd, Pick, PPG)
FF_WR_ML_Data <- FF_WR_ML_Data %>% group_by(Player) %>% mutate(PPR.lag = lag(PPR, n = 1)) %>% mutate(PPR.lag2 = lag(PPR, n=2)) %>% mutate(PPR.lag3 = lag(PPR, n=3)) %>% mutate(GP.lag = lag(`Games Played`, n=1)) %>% mutate(GP.lag2 = lag(`Games Played`, n=2)) %>% mutate(GP.lag3 = lag(`Games Played`, n=3)) %>% mutate(GS.lag = lag(`Games Started`, n=1)) %>% mutate(GS.lag2 = lag(`Games Started`, n=2)) %>% mutate(GS.lag3 = lag(`Games Started`, n=3)) %>% mutate(PPG.lag = lag(PPG, n=1)) %>% mutate(PPG.lag2 = lag(PPG, n=2)) %>% mutate(PPG.lag3 = lag(PPG, n=3))
cor_WR_ML_Data <- cor(FF_WR_ML_Data[,4:length(FF_WR_ML_Data)], use = 'pairwise.complete.obs')
WR_recipe <- recipe(PPG ~ PPR.lag + PPR.lag2 + PPR.lag3 + GP.lag + GP.lag2 + GP.lag3 + GS.lag + GS.lag2 + GS.lag3 + PPG.lag + PPG.lag2 + PPG.lag3 + Age + Rnd + Pick, data = FF_WR_ML_Data) %>% step_knnimpute(all_predictors())
train_WR <- FF_WR_ML_Data %>% group_by(Player) %>% filter(between(`Fantasy Year`, 1999, 2016))
test_WR <- FF_WR_ML_Data %>% group_by(Player) %>% filter(between(`Fantasy Year`, 2017, 2019))
WR_cv <- vfold_cv(train_WR)
WR_train_prep <- WR_recipe %>% prep(train_WR) %>% juice()
WR_test_prep <- WR_recipe %>% prep(test_WR) %>% juice()
WR_model <- linear_reg() %>% set_mode('regression') %>% set_engine('lm')
WR_fit <- fit(WR_model, PPG ~ ., WR_train_prep)
WR_predict <- WR_fit %>% predict(new_data = WR_test_prep) %>% bind_cols(test_WR %>% select(Player, `Games Played`)) %>% bind_cols(WR_test_prep %>% select(Age, PPG, GP.lag, PPR.lag, PPG.lag, GP.lag2, PPR.lag2, PPG.lag2, PPR.lag3, PPG.lag3, GP.lag3, GS.lag3, Rnd, Pick))
rmse_WR <- rmse(WR_predict, PPG, .pred)
mae_WR <- mae(WR_predict, PPG, .pred)
cor_WR_train <- cor(WR_train_prep)
predict_data_WR <- FF_WR_ML_Data %>% group_by(Player) %>% filter(`Fantasy Year` == 2020)
WR_predict_prep <- WR_recipe %>% prep(predict_data_WR) %>% juice()
WR_predict_2020 <- WR_fit %>% predict(new_data = WR_predict_prep) %>% mutate(pred.16 = .pred*16) %>% bind_cols(predict_data_WR %>% select(Player, `Games Played`)) %>% bind_cols(WR_predict_prep %>% select(Age, PPG, GP.lag, PPR.lag, PPG.lag, GP.lag2, PPR.lag2, PPG.lag2, PPR.lag3, PPG.lag3, GP.lag3, GS.lag3, Rnd, Pick))

cor_RB_data <- cor(FF_RB_Data_Filled_In_No_NAs[,3:length(FF_RB_Data_Filled_In_No_NAs)], use = 'pairwise.complete.obs')
FF_RB_ML_Data <- FF_RB_Data_Filled_In_No_NAs %>% select(Player, `Fantasy Year`, Age, `Games Played`, `Games Started`, `Rush Attempts`, `Rush Yards`, `Rush TDs`, Targets, Receptions, `Receiving Yards`, `Receiving TDs`, `Total TDs`, Fumbles, `Fumbles Lost`, PPR, Rnd, Pick, `40 Yard Dash`, `60 Yard Shuttle`, `10 Yard Split`, `20 Yard Split`, PPG)
FF_RB_ML_Data <- FF_RB_ML_Data %>% group_by(Player) %>% mutate(PPR.lag = lag(PPR, n = 1)) %>% mutate(PPR.lag2 = lag(PPR, n=2)) %>% mutate(PPR.lag3 = lag(PPR, n=3)) %>% mutate(GP.lag = lag(`Games Played`, n=1)) %>% mutate(GP.lag2 = lag(`Games Played`, n=2)) %>% mutate(GP.lag3 = lag(`Games Played`, n=3)) %>% mutate(GS.lag = lag(`Games Started`, n=1)) %>% mutate(GS.lag2 = lag(`Games Started`, n=2)) %>% mutate(GS.lag3 = lag(`Games Started`, n=3)) %>% mutate(PPG.lag = lag(PPG, n=1)) %>% mutate(PPG.lag2 = lag(PPG, n=2)) %>% mutate(PPG.lag3 = lag(PPG, n=3))
cor_RB_ML_Data <- cor(FF_RB_ML_Data[,4:length(FF_RB_ML_Data)], use = 'pairwise.complete.obs')
RB_recipe <- recipe(PPG ~ PPR.lag + PPR.lag2 + PPR.lag3 + GP.lag + GP.lag2 + GP.lag3 + GS.lag + GS.lag2 + GS.lag3 + PPG.lag + PPG.lag2 + PPG.lag3 + Rnd + Pick + `40 Yard Dash` + `60 Yard Shuttle` + `10 Yard Split` + `20 Yard Split`, data = FF_RB_ML_Data) %>% step_knnimpute(all_predictors())
train_RB <- FF_RB_ML_Data %>% group_by(Player) %>% filter(between(`Fantasy Year`, 1999, 2016))
test_RB <- FF_RB_ML_Data %>% group_by(Player) %>% filter(between(`Fantasy Year`, 2017, 2019))
RB_cv <- vfold_cv(train_RB)
RB_train_prep <- RB_recipe %>% prep(train_RB) %>% juice()
RB_test_prep <- RB_recipe %>% prep(test_RB) %>% juice()
RB_model <- linear_reg() %>% set_mode('regression') %>% set_engine('lm')
RB_fit <- fit(RB_model, PPG ~ ., RB_train_prep)
RB_predict <- RB_fit %>% predict(new_data = RB_test_prep) %>% bind_cols(test_RB %>% select(Player, Age, `Games Played`)) %>% bind_cols(RB_test_prep %>% select(PPG, GP.lag, PPR.lag, PPG.lag, GP.lag2, PPR.lag2, PPG.lag2, PPR.lag3, PPG.lag3, GP.lag3, GS.lag3, Rnd, Pick, `40 Yard Dash`, `60 Yard Shuttle`, `10 Yard Split`, `20 Yard Split`))
rmse_RB <- rmse(RB_predict, PPG, .pred)
mae_RB <- mae(RB_predict, PPG, .pred)
cor_RB_train <- cor(RB_train_prep)
predict_data_RB <- FF_RB_ML_Data %>% group_by(Player) %>% filter(`Fantasy Year` == 2020)
RB_predict_prep <- RB_recipe %>% prep(predict_data_RB) %>% juice()
RB_predict_2020 <- RB_fit %>% predict(new_data = RB_predict_prep) %>% mutate(pred.16 = .pred*16) %>% bind_cols(predict_data_RB %>% select(Player, Age, `Games Played`)) %>% bind_cols(RB_predict_prep %>% select(PPG, GP.lag, PPR.lag, PPG.lag, GP.lag2, PPR.lag2, PPG.lag2, PPR.lag3, PPG.lag3, GP.lag3, GS.lag3, Rnd, Pick, `40 Yard Dash`, `60 Yard Shuttle`, `10 Yard Split`, `20 Yard Split`))

cor_TE_data <- cor(FF_TE_Data_Filled_In_No_NAs[,3:length(FF_TE_Data_Filled_In_No_NAs)], use = 'pairwise.complete.obs')
FF_TE_ML_Data <- FF_TE_Data_Filled_In_No_NAs %>% select(Player, `Fantasy Year`, Age, `Games Played`, `Games Started`, Targets, Receptions, `Receiving Yards`, `Receiving TDs`, `Total TDs`, Fumbles, `Fumbles Lost`, PPR, Rnd, Pick, `40 Yard Dash`, `Broad Jump`, `10 Yard Split`, PPG)
FF_TE_ML_Data <- FF_TE_ML_Data %>% group_by(Player) %>% mutate(PPR.lag = lag(PPR, n = 1)) %>% mutate(PPR.lag2 = lag(PPR, n=2)) %>% mutate(PPR.lag3 = lag(PPR, n=3)) %>% mutate(GP.lag = lag(`Games Played`, n=1)) %>% mutate(GP.lag2 = lag(`Games Played`, n=2)) %>% mutate(GP.lag3 = lag(`Games Played`, n=3)) %>% mutate(GS.lag = lag(`Games Started`, n=1)) %>% mutate(GS.lag2 = lag(`Games Started`, n=2)) %>% mutate(GS.lag3 = lag(`Games Started`, n=3)) %>% mutate(PPG.lag = lag(PPG, n=1)) %>% mutate(PPG.lag2 = lag(PPG, n=2)) %>% mutate(PPG.lag3 = lag(PPG, n=3))
cor_TE_ML_Data <- cor(FF_TE_ML_Data[,4:length(FF_TE_ML_Data)], use = 'pairwise.complete.obs')
TE_recipe <- recipe(PPG ~ PPR.lag + PPR.lag2 + PPR.lag3 + GP.lag + GP.lag2 + GP.lag3 + GS.lag + GS.lag2 + GS.lag3 + PPG.lag + PPG.lag2 + PPG.lag3 + Rnd + Pick + `40 Yard Dash` + `Broad Jump` + `10 Yard Split`, data = FF_TE_ML_Data) %>% step_knnimpute(all_predictors())
train_TE <- FF_TE_ML_Data %>% group_by(Player) %>% filter(between(`Fantasy Year`, 1999, 2016))
test_TE <- FF_TE_ML_Data %>% group_by(Player) %>% filter(between(`Fantasy Year`, 2017, 2019))
TE_cv <- vfold_cv(train_TE)
TE_train_prep <- TE_recipe %>% prep(train_TE) %>% juice()
TE_test_prep <- TE_recipe %>% prep(test_TE) %>% juice()
TE_model <- linear_reg() %>% set_mode('regression') %>% set_engine('lm')
TE_fit <- fit(TE_model, PPG ~ ., TE_train_prep)
TE_predict <- TE_fit %>% predict(new_data = TE_test_prep) %>% bind_cols(test_TE %>% select(Player, Age, `Games Played`)) %>% bind_cols(TE_test_prep %>% select(PPG, GP.lag, PPR.lag, PPG.lag, GP.lag2, PPR.lag2, PPG.lag2, PPR.lag3, PPG.lag3, GP.lag3, GS.lag3, Rnd, Pick, `40 Yard Dash`, `Broad Jump`, `10 Yard Split`))
rmse_TE <- rmse(TE_predict, PPG, .pred)
mae_TE <- mae(TE_predict, PPG, .pred)
cor_TE_train <- cor(TE_train_prep)
predict_data_TE <- FF_TE_ML_Data %>% group_by(Player) %>% filter(`Fantasy Year` == 2020)
TE_predict_prep <- TE_recipe %>% prep(predict_data_TE) %>% juice()
TE_predict_2020 <- TE_fit %>% predict(new_data = TE_predict_prep) %>% mutate(pred.16 = .pred*16) %>% bind_cols(predict_data_TE %>% select(Player, Age, `Games Played`)) %>% bind_cols(TE_predict_prep %>% select(PPG, GP.lag, PPR.lag, PPG.lag, GP.lag2, PPR.lag2, PPG.lag2, PPR.lag3, PPG.lag3, GP.lag3, GS.lag3, Rnd, Pick, `40 Yard Dash`, `Broad Jump`, `10 Yard Split`))

cor_QB_data <- cor(FF_QB_Data_Filled_In_No_NAs[,3:length(FF_QB_Data_Filled_In_No_NAs)], use = 'pairwise.complete.obs')
FF_QB_ML_Data <- FF_QB_Data_Filled_In_No_NAs %>% select(Player, `Fantasy Year`, Age, `Games Played`, `Games Started`, Completions, `Pass Attempts`, `Pass Yards`, `Pass TDs`, Interceptions, `Rush Attempts`, `Rush Yards`, `Rush TDs`, `Total TDs`, Fumbles, `Fumbles Lost`, PPR, Rnd, Pick, `20 Yard Split`, PPG)
FF_QB_ML_Data <- FF_QB_ML_Data %>% group_by(Player) %>% mutate(PPR.lag = lag(PPR, n = 1)) %>% mutate(PPR.lag2 = lag(PPR, n=2)) %>% mutate(PPR.lag3 = lag(PPR, n=3)) %>% mutate(GP.lag = lag(`Games Played`, n=1)) %>% mutate(GP.lag2 = lag(`Games Played`, n=2)) %>% mutate(GP.lag3 = lag(`Games Played`, n=3)) %>% mutate(GS.lag = lag(`Games Started`, n=1)) %>% mutate(GS.lag2 = lag(`Games Started`, n=2)) %>% mutate(GS.lag3 = lag(`Games Started`, n=3)) %>% mutate(PPG.lag = lag(PPG, n=1)) %>% mutate(PPG.lag2 = lag(PPG, n=2)) %>% mutate(PPG.lag3 = lag(PPG, n=3))
cor_QB_ML_Data <- cor(FF_QB_ML_Data[,4:length(FF_QB_ML_Data)], use = 'pairwise.complete.obs')
QB_recipe <- recipe(PPG ~ PPR.lag + PPR.lag2 + PPR.lag3 + GP.lag + GP.lag2 + GP.lag3 + GS.lag + GS.lag2 + GS.lag3 + PPG.lag + PPG.lag2 + PPG.lag3 + Rnd + Pick + `20 Yard Split`, data = FF_QB_ML_Data) %>% step_knnimpute(all_predictors())
train_QB <- FF_QB_ML_Data %>% group_by(Player) %>% filter(between(`Fantasy Year`, 1999, 2016))
test_QB <- FF_QB_ML_Data %>% group_by(Player) %>% filter(between(`Fantasy Year`, 2017, 2019))
QB_cv <- vfold_cv(train_QB)
QB_train_prep <- QB_recipe %>% prep(train_QB) %>% juice()
QB_test_prep <- QB_recipe %>% prep(test_QB) %>% juice()
QB_model <- linear_reg() %>% set_mode('regression') %>% set_engine('lm')
QB_fit <- fit(QB_model, PPG ~ ., QB_train_prep)
QB_predict <- QB_fit %>% predict(new_data = QB_test_prep) %>% bind_cols(test_QB %>% select(Player, Age, `Games Played`)) %>% bind_cols(QB_test_prep %>% select(PPG, GP.lag, PPR.lag, PPG.lag, GP.lag2, PPR.lag2, PPG.lag2, PPR.lag3, PPG.lag3, GP.lag3, GS.lag3, Rnd, Pick, `20 Yard Split`))
rmse_QB <- rmse(QB_predict, PPG, .pred)
mae_QB <- mae(QB_predict, PPG, .pred)
cor_QB_train <- cor(QB_train_prep)
predict_data_QB <- FF_QB_ML_Data %>% group_by(Player) %>% filter(`Fantasy Year` == 2020)
QB_predict_prep <- QB_recipe %>% prep(predict_data_QB) %>% juice()
QB_predict_2020 <- QB_fit %>% predict(new_data = QB_predict_prep) %>% mutate(pred.16 = .pred*16) %>% bind_cols(predict_data_QB %>% select(Player, Age, `Games Played`)) %>% bind_cols(QB_predict_prep %>% select(PPG, GP.lag, PPR.lag, PPG.lag, GP.lag2, PPR.lag2, PPG.lag2, PPR.lag3, PPG.lag3, GP.lag3, GS.lag3, Rnd, Pick, `20 Yard Split`))

WR_Final_2020 <- WR_predict_2020 %>% ungroup() %>% select(Player, pred.16)
RB_Final_2020 <- RB_predict_2020 %>% ungroup() %>% select(Player, pred.16)
TE_Final_2020 <- TE_predict_2020 %>% ungroup() %>% select(Player, pred.16)
QB_Final_2020 <- QB_predict_2020 %>% ungroup() %>% select(Player, pred.16)
Total_Final_2020 <- bind_rows(WR_Final_2020, RB_Final_2020, TE_Final_2020, QB_Final_2020)
Total_Final_2020 <- Total_Final_2020[order(Total_Final_2020$pred.16, decreasing = TRUE),]