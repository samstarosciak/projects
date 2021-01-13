#MKT 451 Final Project: 2019 Hitter Season Comparisons
#Sam Starosciak

#Importing libraries
library(base)
library(dplyr)
library(ggplot2)
library(class)
library(philentropy)
library(neighbr)

#Import data
hitterdata <- read_csv("MKT 451 PROJECT FanGraphs Leaderboard.csv",
                        header=TRUE, sep=",")

#Remove unnecessary columns
hitterdata <- hitterdata[,1:26]

#Turn factored columns into percents
hitterdata$BB. <- as.numeric(sub("%","",hitterdata[,13]))
hitterdata$K. <- as.numeric(sub("%","",hitterdata[,14]))
hitterdata$set_ID <- c(1:length(hitterdata$First))

#Create normalization function
normalize <- function(x) {
  num <- x - mean(x)
  denom <- sd(x)
  return (num/denom)
}

#Create and normalize values
names <- paste(hitterdata$First, hitterdata$Last)
team <- as.character(hitterdata$Team)
games <- normalize(as.numeric(hitterdata$G))
plateapp <- normalize(as.numeric(hitterdata$PA))
hr <- normalize(as.numeric(hitterdata$HR))
runs <- normalize(as.numeric(hitterdata$R))
rbi <- normalize(as.numeric(hitterdata$RBI))
sb <- normalize(as.numeric(hitterdata$SB))
k_rate <- normalize(as.numeric(hitterdata$K.))
bb_rate <- normalize(as.numeric(hitterdata$BB.))
iso <- normalize(as.numeric(hitterdata$ISO))
babip <- normalize(as.numeric(hitterdata$BABIP))
avg <- normalize(as.numeric(hitterdata$AVG))
obp <- normalize(as.numeric(hitterdata$OBP))
slg <- normalize(as.numeric(hitterdata$SLG))
ops <- normalize(obp + slg)
wOBA <- normalize(as.numeric(hitterdata$wOBA))
wRC_plus <- normalize(as.numeric(hitterdata$wRC.))
BsR <- normalize(as.numeric(hitterdata$BsR))
war <- normalize(as.numeric(hitterdata$WAR))
player_id <- factor(hitterdata$playerid)
set_norm_ID <- as.numeric(hitterdata$set_ID)

#Create data fame
norm_df <- data.frame(player_id,team, set_norm_ID, names, war, games, plateapp, 
                      hr, runs, rbi,sb,k_rate,bb_rate, iso, babip, avg, obp, slg, 
                      ops, wOBA, wRC_plus,BsR)

#Each model should take 5-10 minutes to run
#Model 1 
train <- select(norm_df, 4:22)
test <- select(norm_df, 5:22)

hitterknn <- knn(train_set = train, test_set = test, k = 30,
                 comparison_measure = "euclidean",
                 continuous_scoring_method =  "average",
                 return_ranked_neighbors = 4,
                 id = "names")

library(dplyr)
results <- hitterknn$test_set_scores

names(results)[1] <- "Player"
names(results)[2] <- "Neighbor 1"
names(results)[3] <- "Neighbor 2"
names(results)[4] <- "Neighbor 3"

#Model 2 (no WAR)
norm_df2 <- data.frame(player_id,team, set_norm_ID, names, games, plateapp, hr, 
                       runs, rbi,sb,k_rate, bb_rate, iso, babip, avg, obp, slg, 
                       ops, wOBA, wRC_plus,BsR)

train2 <- select(norm_df2, 4:21)
test2 <- select(norm_df2, 5:21)
hitterknn2 <- knn(train_set = train2, test_set = test2, k = 30,
                  comparison_measure = "euclidean",
                  continuous_scoring_method =  "average",
                  return_ranked_neighbors = 4,
                  id = "names")

library(dplyr)
results2 <- hitterknn$test_set_scores

names(results2)[1] <- "Player"
names(results2)[2] <- "Neighbor 1"
names(results2)[3] <- "Neighbor 2"
names(results2)[4] <- "Neighbor 3"

#Add a BMI column to hitterdata
hitterWeight <- as.numeric(hitterdata$Weight)
hitterHeight <- as.numeric(hitterdata$Height)
hitterdata$BMI <- round((703*hitterdata$Weight/(hitterdata$Height^2)),3)

# Examine the histogram distribution of BMI
summary(hitterdata$BMI)
hist(hitterdata$BMI, main = "BMI of Hitters (min 150 PA)", col = "#00990099",
     xlab = "BMI", breaks = 10)

#Add BMI to normalized dataframe
norm_bmi <- normalize(as.numeric(hitterdata$BMI))
norm_df3 <- data.frame(player_id,team, set_norm_ID, names, norm_bmi, games, 
                       plateapp, hr, runs, rbi,sb,k_rate,bb_rate, iso, babip, 
                       avg, obp, slg, ops, wOBA, wRC_plus,BsR)

#Model 3 (add BMI)
train3 <- select(norm_df3, 4:22)
test3 <- select(norm_df3, 5:22)
hitterknn3 <- knn(train_set = train3, test_set = test3, k = 30,
                  comparison_measure = "euclidean",
                  continuous_scoring_method =  "average",
                  return_ranked_neighbors = 4,
                  id = "names")

library(dplyr)
results3 <- hitterknn3$test_set_scores

names(results3)[1] <- "Player"
names(results3)[2] <- "Neighbor 1"
names(results3)[3] <- "Neighbor 2"
names(results3)[4] <- "Neighbor 3"
