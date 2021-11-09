#The goal is to cluster the pitch metrics in order to identify the pitch-type that we are not given.
#Import the dataset
library(readr)
pitch_data <- read_csv("candex_pitch_data.csv")

#Check the types on each of the variables. 
str(pitch_data)
class(pitch_data)

#Since all the variables are in their proper data types for analysis, we'll move on to the next step.
# We want to filter out the columns we won't be using for analysis. 
#We only want to keep columns that will help us identify the pitch type. 
#Therefore, we don't need any context regarding the count, who the pitcher or the hitter is, or the result of that pitch.
# The only non-numeric column we will need to keep is the PitcherThrowing column because it will give us a chance
# to separate the data into movement profiles for lefties and righties. 
#Otherwise, I've chosen to utilize column 6 through column 14 for the clustering analysis.

#Create a new data frame containing only these variables called pd1 (pitch data 1).
pd1 <- pitch_data[,c(2,6:14)]

#Now that we have our data for analysis, we should eliminate any rows with NA values.
pd1 <- na.omit(pd1)

#We find that there were 46 rows with NA values. 
#692,809 rows will still be more than enough to complete a sufficient analysis. 

#Now to filter out any outliers...first, velocity outliers.
#I decided that I wanted to select these particular upper and lower bounds for this reason... 
lower_bound <- 61.9 
#When all pitches from 2020 MLB compared in order of velocity, 61.9 was the lowest value where two pitches 
#thrown by pitchers (not position players) followed each other in the data set.
upper_bound <- 102.2
#Same reasoning with 102.2, (however there weren't any position players throwing this hard).

#Eliminate those outliers from the data frame.
library(dplyr)
pd1 <- pd1 %>%
        filter(ReleaseSpeed>=lower_bound, ReleaseSpeed <= upper_bound)
#This shrank the number of rows from 692,809 to 689,539

#Next step is to eliminate pitches that have impossible break numbers. 
#Those would be Horizontal breaks greater than 100 or less than -100. Those are misreads.
pd1 <- pd1 %>%
  filter(HorzBreak>=-100, HorzBreak <= 100)
#That shrank the data set down to 688,305. 
#There are no remaining outliers in vertical break.

#Next we will separate pitches from right-handed pitchers and left-handed pitchers.
#We do this because the movement from, let's say, a RHP curveball will move differently from a LHP curveball.
#That applies to all pitches because they are being released from different sides of the mound and breaking opposite directions.
#We will name those data frames pd_l and pd_r.
pd_l <- subset(pd1, PitcherThrowing == "L")
pd_r <- subset(pd1, PitcherThrowing == "R")

#Next we want to normalize those continuous values. 
pd_l <- cbind(pd_l[1], scale(pd_l[,3:10]))
pd_r <- cbind(pd_r[1], scale(pd_r[,3:10]))

#View the transformed data
head(pd_l)
head(pd_r)

#Now we want to run a couple k-means clustering algorithms.
#First we will analyze the RHP k-means with default parameters and an inital estimate number of pitch types of 6
#stats::kmeans(pd_r, centers = 6) #This code was not useful, in the end. 

#After some thought, I came to the conclusion that release points are not going to be indicative 
#of pitch type because each pitcher has a different arm slot and most of those will not change with 
# a different pitch type. One guy's release point on his slider may be drastically different from someone 
# else's slider but similar to the release point of another guy's fastball.
#Release point also has to do with how tall the pitcher is. 
#Two pitchers could have the same arm slot but one is 6 inches taller than the other. 
#His RelHeight would be higher and they'd have different release points.
#Pitches also end up all over the place, that doesn't really have anything to do in determining what pitch is being thrown.

#Eliminate pitch release side, height, extension, plate height, and plate side from pd_l and pd_r.
pd_l <- pd_l[,-c(2:4, 8:9)]
pd_r <- pd_r[,-c(2:4, 8:9)]

#Next we want to determine the optimal number of clusters (pitch types) by deciding which method to use.
#install.packages("factoextra")

# Elbow method ERROR
#factoextra::fviz_nbclust(pd_r, kmeans, method = "wss") 

# Silhouette method ERROR
#factoextra::fviz_nbclust(pd_r[,2:4], kmeans, method = "silhouette")

#Both of these received error messages that the vector memory was exhausted.
#Instead we will run a few K-means models with different number of clusters.
#Running these should take a total of about 5 minutes

kr2 <- kmeans(pd_r[,2:4], centers = 2, nstart = 25)
kr3 <- kmeans(pd_r[,2:4], centers = 3, nstart = 25)
kr4 <- kmeans(pd_r[,2:4], centers = 4, nstart = 25)
kr5 <- kmeans(pd_r[,2:4], centers = 5, nstart = 25)
kr6 <- kmeans(pd_r[,2:4], centers = 6, nstart = 25)
kr7 <- kmeans(pd_r[,2:4], centers = 7, nstart = 25)
kr8 <- kmeans(pd_r[,2:4], centers = 8, nstart = 25)
kr9 <- kmeans(pd_r[,2:4], centers = 9, nstart = 25)
kr10 <- kmeans(pd_r[,2:4], centers = 10, nstart = 25)

#We want to determine the optimal number of clusters we want to use.
#Elbow method

set.seed(123)

# Function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(pd_r[,2:4], k, nstart = 10 )$tot.withinss
}

# Define k values for k = 2 to k = 10
k.values <- 2:10

# Extract wss for 2-10 clusters
wss_values <- sapply(k.values, wss)

#Plot it
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main= "Right-Handed Pitchers")


View(wss_values)

#It's hard to determine the identify the inflection point as 4 or 5 so we will try the...
#Silhouette Method...  #Was not used in the end.
#set.seed(123)

# function to compute average silhouette for k clusters
#avg_sil <- function(k) {
#  km.res <- kmeans(pd_r[,2:4], k, nstart = 10)
#  ss <- cluster::silhouette(km.res$cluster, dist(pd_r[,2:4]))}

# Define k values k = 2 to k = 10
#k.values <- 2:10

#Extract avg silhouette for 2-10 clusters ERROR (exhausted)
#avg_sil_values <- sapply(k.values, avg_sil)

#The silhouette values won't compute so we will just use the elbow method. 
# After examining the elbow method the optimal number of clusters is five (inflection point, where the elbow bends).
#Reverting back to our k-means models we will choose the one with five centroids.
kr5 <- kmeans(pd_r[,2:4], centers = 5, nstart = 25)
print(kr5)

#Add cluster to the pd_r data frame
pd_r$cluster <- kr5$cluster

##Run the same process for LHP
#Find optimal number of clusters.

set.seed(123)

# Function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(pd_l[,2:4], k, nstart = 10 )$tot.withinss
}

# Define k values k = 2 to k = 10
k.values <- 2:10

# Extract wss for 2-10 clusters
wss_values <- sapply(k.values, wss)

#Plot it
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main= "Left-Handed Pitchers")


#The optimal number of clusters is also 5 (inflection point @ 5) for LHP
kl5 <- kmeans(pd_l[,2:4], centers = 5, nstart = 25)
print(kl5)

#Add cluster to the data frame
pd_l$cluster <- kl5$cluster

#Combine pd_l and pd_r with a row bind function.
#Make the final df just two columns: GameEventId, and cluster 
final <- rbind(pd_l, pd_r)
final <- final[, c(1,5)]

#Compare the clusters next to the original values in pd1
pd1$cluster <- final$cluster

#Turn the clusters into categorical variables for plotting
pd1$cluster <- as.character(pd1$cluster)

#Visualize the clusters #Revealed that the classifications were off between righties and lefties. 
library(ggplot2)
ggplot(pd1, aes(x=HorzBreak, y=VertBreak, color = cluster, fill = cluster)) +
  stat_ellipse(data = pd1, geom = "polygon", alpha = 0.2) +
  coord_cartesian() +
  theme_bw() +
  ggtitle("Cluster Profiles by Movement", subtitle = "Catcher's Perspective") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Horizontal Break") + ylab("Vertical Break") + 
  geom_vline(xintercept =0) +
  geom_hline(yintercept =0) +
  facet_wrap(~PitcherThrowing) 

#Realized that two different models return different results. For example... 
#Cluster 3 in pd_l is similar to Cluster 4 in pd_r.
#Cluster 4 in pd_l is similar to Cluster 5 in pd_r.
#Cluster 2 in pd_l is similar to Cluster 1 in pd_r.
#Cluster 5 in pd_l is similar to Cluster 2 in pd_r.
#Cluster 1 in pd_l is similar to Cluster 3 in pd_r.

#Convert clusters from pd_r to reflect the same profile for clusters in pd_l.
pd_r$cluster<- ifelse(pd_r$cluster==4, 3,
                ifelse(pd_r$cluster==5, 4,
                  ifelse(pd_r$cluster==1, 2,
                     ifelse(pd_r$cluster==2, 5, 1))))

#Recreate the final data frame
final <- rbind(pd_l, pd_r)
final <- final[, c(1,5)]

#Compare the new clusters next to the original values
pd1$cluster <- final$cluster

#Turn the clusters into categorical variables
pd1$cluster <- as.character(pd1$cluster)

#Group averages by throwing side and Cluster
#This is where we will determine which cluster profiles as which pitch. 
ClusterAvg <- pd1 %>%
  select(PitcherThrowing, cluster, ReleaseSpeed, HorzBreak, VertBreak) %>%
  group_by(PitcherThrowing, cluster) %>%
  summarise(Velo = round(mean(ReleaseSpeed),1), 
            HB = round(mean(HorzBreak),1), 
            VB = round(mean(VertBreak),1))
View(ClusterAvg)

#From here, I've determined the following classifications:
# Cluster 2 = Four-seam fastball
# Cluster 4 = Two-seam fastball
# Cluster 1 = Curveball
# Cluster 5 = Slider
# Cluster 3 = Changeup
#Update the final table to refelct these classifications.
final$cluster<- ifelse(final$cluster== 2, "Four-seam Fastball",
                        ifelse(final$cluster== 4, "Two-seam Fastball",
                             ifelse(final$cluster== 1, "Curveball",
                                    ifelse(final$cluster== 5, "Slider", "Changeup"))))

#Turn classificiation into pitch type for visual
pd1$pitch <- final$cluster

#Re-visualize the clusters with labels.
library(ggplot2)
ggplot(pd1, aes(x=HorzBreak, y=VertBreak, fill = pitch)) +
  stat_ellipse(data = pd1, geom = "polygon", alpha = 0.5) +
  coord_cartesian() +
  theme_bw() +
  ggtitle("Cluster Profiles by Movement", subtitle = "Catcher's Perspective") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Horizontal Break") + ylab("Vertical Break") + 
  labs(fill = "Pitch Type")+
  geom_vline(xintercept =0) +
  geom_hline(yintercept =0) +
  facet_wrap(~PitcherThrowing)

#Export final dataframe as CSV
write.csv(final,"9992787_categories.csv", row.names = FALSE)




