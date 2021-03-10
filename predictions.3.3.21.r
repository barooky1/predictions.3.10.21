
# load libraries, load data ---------------------------------------
library(tidyverse)
library(randomForest)
library(scales)
library(formattable)
library(states)
library(anytime)
rawdata <- read.csv("data/2017topresent.csv")

# usa only, automobile only -----------------------------------------------

rawdata2 <- rawdata %>%
  filter(PickUpState %in% state.abb| PickUpState =="DC",    Type=="Automobile") %>%  
  select(-Type)


# clean data so simiar terms match ----------------------------------------

rawdata2$Runs[rawdata2$Runs=="no"] <- "No"
rawdata2$Runs[rawdata2$Runs=="yes"] <- "Yes"
rawdata2$Runs[rawdata2$Runs==""] <- "Yes"

rawdata2$Runs <- as.character(rawdata2$Runs)
rawdata2$Runs <-  as.factor(rawdata2$Runs)
rawdata2$Make <- as.factor(rawdata2$Make)

rawdata2$Make[rawdata2$Make=="chevy"] <- "Chevrolet"
rawdata2$Make[rawdata2$Make=="chevy "] <- "Chevrolet"
rawdata2$Make[rawdata2$Make=="Chevy"] <- "Chevrolet"
rawdata2$Make[rawdata2$Make=="Chevy "] <- "Chevrolet"
rawdata2$Make[rawdata2$Make=="Chevrolet "] <- "Chevrolet"
# parse, clean , and impute weights ------------------------------------

rawdata2$Weight <- parse_number(rawdata2$Weight)
weights <- rawdata2 %>% 
  drop_na(Weight) %>% 
  group_by(Make, Model) %>% 
  summarise(mean(Weight))
rawdata2 <- left_join(rawdata2, weights, by= c("Model"="Model", "Make"="Make"))
rawdata2$weight <- ifelse(is.na(rawdata2$Weight), rawdata2$`mean(Weight)`, rawdata2$Weight )
rawdata2 <- rawdata2 %>% 
  select(-Weight)
# impute missing mileage --------------------------------------------------

rawdata2$Mileage <- 
  ifelse(is.na(rawdata2$Mileage), (2020-rawdata2$YearOfCar)*10000,rawdata2$Mileage)

# remove NA ---------------------------------------------------------------

rawdata2 <- rawdata2 %>% drop_na("HighestPrice")
rawdata2$ExteriorCondition <- NULL
rawdata2$InteriorCondition <- NULL
rawdata2$X <- NULL

rawdata2 <- rawdata2 %>% 
  filter(!is.na(rawdata2$Model))
rawdata2 <- rawdata2 %>% 
  filter(!is.na(rawdata2$YearOfCar))
rawdata2 <- rawdata2 %>% 
  filter(rawdata2$Make!="")
rawdata2 <- rawdata2 %>% 
  filter(rawdata2$Model!="")
rawdata2 <- na.omit(rawdata2)


# clean dates, get don.month and don.year -------------------------------------------------------------

rawdata2$DonationDate <- sapply(strsplit(as.character(rawdata2$DonationDate), " "), "[", 1)
rawdata2$DonationDate <- as.Date(rawdata2$DonationDate,"%m/%d/%Y")
rawdata2$don.month <- sapply(strsplit(as.character(rawdata2$DonationDate), "-"),"[",2)
rawdata2$don.month <- as.numeric(rawdata2$don.month)
rawdata2$don.year <- sapply(strsplit(as.character(rawdata2$DonationDate), "-"),"[",1)
rawdata2$don.year <- as.numeric(rawdata2$don.year)

# add auction benefits,  "shooda", shooda results, and whether it was auctioned####

rawdata2$AuctionBenefit <-rawdata2$Income-rawdata2$HighestPrice
rawdata2$shooda <- ifelse(rawdata2$AuctionBenefit>0,1,0)
rawdata2$shooda <- as.factor(rawdata2$shooda)
rawdata2$shoodaresult <- ifelse(rawdata2$shooda==1,rawdata2$Income, rawdata2$HighestPrice)
rawdata2$auctioned <- grepl("Auction",rawdata2$Seller )
rawdata2$auctioned <- ifelse(rawdata2$auctioned=="TRUE", 1,0)

# RERUN FROM HERE!!!split data into train, validation, and test sets ------------------------

# train <- rawdata2[1:(nrow(rawdata2)*.75),]
# val <- rawdata2[nrow(train):(nrow(rawdata2)*.95),]
# test <- rawdata2[(nrow(rawdata2)*.95):nrow(rawdata2),]

# new version of split:
set.seed(111)
randomizer <- sample(3, nrow(rawdata2), replace = T, prob = c(0.75, 0.2,.05))
train <- rawdata2[randomizer==1,]
val <- rawdata2[randomizer==2,]
test <- rawdata2[randomizer==3,]


# create yardmeans from train set, used for  all 3 data sets -----------------------------------------

yard_ratios <- train %>%
  filter(PriceSoldFor>0, Seller=="Copart Auto Auction", don.year>2018) %>%
  group_by(Make,Model,YearOfCar, YardNumber, don.year) %>%
  summarise(sum_paid = sum(PriceSoldFor),
            count = n(),
            YardNumber_mean = mean(PriceSoldFor)) %>%
  group_by(Make,Model,YearOfCar, don.year) %>%
  mutate(National_mean = sum(sum_paid) / sum(count),
         yard_ratio = YardNumber_mean/National_mean) %>%
  group_by(YardNumber) %>%
  summarise(yardmean= mean(yard_ratio),
            yard.count=n())
train<- left_join(train,yard_ratios, "YardNumber" )
val <- left_join(val,yard_ratios, "YardNumber" )
test <- left_join(test,yard_ratios, "YardNumber" )


# assign make and year to be used in predication -----------------------------------

make <- "Toyota"
year <- 2019
train <- train %>% 
  filter(Make %in% make , don.year %in% year)
val <- val %>% 
  filter(Make %in% make , don.year %in% year)
test <- test %>% 
  filter(Make %in% make , don.year %in% year)

unique_train <- unique(train$Model)
val <- val[val$Model %in% unique_train,]
test <-  test[test$Model %in% unique_train,]

#  glm model ---------------------------------------------------------
glm_model <- glm(shooda ~Model  +Runs+Mileage+ YearOfCar+HighestPrice+Charges+don.month+yardmean+weight+don.year , data = train, family = 'binomial')

# RF model ----------------------------------------------------------------

rfmodel <- randomForest(shooda ~ Model+Runs + Mileage+ YearOfCar+HighestPrice+Charges+don.month+yardmean+weight , data = train, proximity=TRUE, na.action = na.exclude)

# predictions -------------------------------------------------------------

glm_predict_val <- predict(glm_model, val, type = 'response')
val$rf.classifier <- predict(rfmodel, val, type = 'response')
val$glm.classifier <-  if_else(glm_predict_val>.501,1,0)
 

# confusion matrices ------------------------------------------------------


table(Predicted =val$glm.classifier , Actual = val$auctioned)
table(Predicted =val$glm.classifier , Best = val$shooda)
table(Actual = val$auctioned,Best =val$shooda)
table(best=val$shooda, rf=val$rf.classifier)

table(val$shooda)

# bottom line results -----------------------------------------------------
val$glm_result <- ifelse(val$glm.classifier==1,train$Income, train$HighestPrice)
val$rf_result <- ifelse(val$rf.classifier==1,train$Income, train$HighestPrice)
val$both_result <- ifelse(val$rf.classifier==0&val$glm.classifier==0, val$HighestPrice,val$Income)
val$best_results <- ifelse(val$shooda==1, val$Income, val$HighestPrice)
val <- val[!is.na(val$both_result),]
results <- data.frame( Make=val[3,"Make"],
                          best_results=comma(sum(val$best_results)),
                          Actual_Income= comma(sum(val$Income)),
                          glm_result=sum(val$glm_result),
                          rf.results=comma(sum(val$rf_result)),
                          potential_improvement=percent(sum(val$best_results)/sum(val$Income)-1),
                          both=comma(sum(val$both_result)) ,
                       both_improvement=comma(sum(val$both_result))-comma(sum(val$Income)),
                       both_improvement_percent=percent((sum(val$both_result))/sum(val$Income)-1)
)
results



# challenges: must deal with how to predict success on junk cars; must remove zeros from both yarddmean(done) predictors, as well as from train , val, and test sets... however,  can and should  include a penalty to allow for duds c) now that including junk in overall data set, not just cpo cars, ,  shoulod create 3rd binary, as what actually DID happen, vs shooda, vs my model 



# predict on final test ---------------------------------------------------
glm_predict_test <- predict(glm_model, test, type = 'response')
test$rf.classifier <- predict(rfmodel, test, type = 'response')
test$glm.classifier <-  if_else(glm_predict_test>.501,1,0)

test$glm_result <- ifelse(test$glm.classifier==1,train$Income, train$HighestPrice)
test$rf_result <- ifelse(test$rf.classifier==1,train$Income, train$HighestPrice)
test$both_result <- ifelse(test$rf.classifier==0&test$glm.classifier==0, test$HighestPrice,test$Income)
test$best_results <- ifelse(test$shooda==1, test$Income, test$HighestPrice)
test.results <- data.frame( Make=test[3,"Make"],
                       best_results=comma(sum(test$best_results)),
                       Actual_Income= comma(sum(test$Income)),
                       glm_result=sum(test$glm_result),
                       rf.results=comma(sum(test$rf_result)),
                       potential_improvement=percent(sum(test$best_results)/sum(test$Income)-1),
                       both=comma(sum(test$both_result)) ,
                       both_improvement=comma(sum(test$both_result))-comma(sum(test$Income))
)
test.results

