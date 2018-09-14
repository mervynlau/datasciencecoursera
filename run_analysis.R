# Getting and Cleaning Data Course Project
setwd("../Getting and Cleaning Data Course Project")
getwd()
library(tidyverse)
library(data.table)

README <- read_lines("./UCI HAR Dataset/README.txt")
README
features_info <- read_lines("./UCI HAR Dataset/features_info.txt")
features_info

# Its activity label. 
activity_labels <-
 read_table("./UCI HAR Dataset/activity_labels.txt", col_names = FALSE)
colnames(activity_labels) <- c("label", "activity")
activity_labels

# A 561-feature vector with time and frequency domain variables.
features <-
  read_table("./UCI HAR Dataset/features.txt", col_names = FALSE) 
features

# Training data set
X_train <- 
  read_table("../Getting and Cleaning Data Course Project/UCI HAR Dataset/train/X_train.txt", col_names = FALSE) 
colnames(X_train) <- as.vector(t(features[,1]))

# Extracts only the measurements on the mean and standard deviation for each measurement.
X_train_mean_std <- select(X_train, contains("-mean()"), contains("-std()"))

y_train <-
  read_table("./UCI HAR Dataset/train/y_train.txt", col_names = FALSE) 
colnames(y_train) <- "label"
# Uses descriptive activity names to name the activities in the data set
y_train <- left_join(y_train, activity_labels, by = "label")

subject_train <- 
  read_table("./UCI HAR Dataset/train/subject_train.txt", col_names = FALSE)
colnames(subject_train) <- "subject"
subject_train <- mutate(subject_train, set = "train")

train_data <- cbind(subject_train, y_train, X_train_mean_std) %>% tbl_df()

# Test data set
X_test <- 
  read_table("./UCI HAR Dataset/test/X_test.txt", col_names = FALSE) 
colnames(X_test) <- as.vector(t(features[,1]))

# Extracts only the measurements on the mean and standard deviation for each measurement.
X_test_mean_std <- select(X_test, contains("-mean()"), contains("-std()"))

y_test <-
  read_table("./UCI HAR Dataset/test/y_test.txt", col_names = FALSE) 
colnames(y_test) <- "label"
# Uses descriptive activity names to name the activities in the data set
y_test <- left_join(y_test, activity_labels, by = "label")

subject_test <- 
  read_table("./UCI HAR Dataset/test/subject_test.txt", col_names = FALSE)
colnames(subject_test) <- "subject"
subject_test <- mutate(subject_test, set = "test")

test_data <- cbind(subject_test, y_test, X_test_mean_std) %>% tbl_df()

# Merges the training and the test sets to create one data set
full_data <- rbind(train_data, test_data)
full_data <- tibble::rowid_to_column(full_data, "ID")

# Reshaping the data
full_gather <- gather(full_data, key = "key", value = "value", `1 tBodyAcc-mean()-X`:`543 fBodyBodyGyroJerkMag-std()`) 
full_separate <- separate(full_gather, key, into = c("num", "key"), sep = " ") %>% select(- num)
full_separate <- separate(full_separate,  key, into = c("sensor","key","axis"), sep = "-")

full_spread <- spread(full_separate, key, value)
full_spread <- mutate(full_spread, set = as.factor(set), activity = as.factor(activity), axis = as.factor(axis), domain = as.factor(substring(sensor,1,1)),  sensor = as.factor(substring(sensor, 2)))
colnames(full_spread)[colnames(full_spread)=="mean()"] <- "mean"
colnames(full_spread)[colnames(full_spread)=="std()"] <- "std"
tidy_data <- select(full_spread, ID, subject, set, activity, domain, sensor, axis, mean, std)
tidy_data
summary(tidy_data)
write_csv(tidy_data, "tidy_data.csv")
# Step 5
tidy_data_avg <- select(tidy_data, -ID) %>% group_by(subject, set, activity, domain, sensor, axis) %>% summarise(avg.mean = mean(mean), avg.std = mean(std))
tidy_data_avg
summary(tidy_data_avg)
glimpse(tidy_data_avg)

#Export final tidy data
write.table(tidy_data_avg, "tidy_data_avg.txt", row.names = FALSE)
