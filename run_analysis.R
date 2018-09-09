setwd("../Getting and Cleaning Data Course Project")
library(tidyverse)
library(data.table)

README <- read_lines("./UCI HAR Dataset/README.txt")
features_info <- read_lines("./UCI HAR Dataset/features_info.txt")
README
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

X_train <- 
  read_table("../Getting and Cleaning Data Course Project/UCI HAR Dataset/train/X_train.txt", col_names = FALSE) 
colnames(X_train) <- as.vector(t(features[,1]))
glimpse(X_train)

filter(features, str_detect(X1, "mean()") | str_detect(X1, "std()"))

X_train_mean_std <- select(X_train, contains("-mean()"), contains("-std()"))

y_train <-
  read_table("./UCI HAR Dataset/train/y_train.txt", col_names = FALSE) 
colnames(y_train) <- "label"
y_train <- left_join(y_train, activity_labels, by = "label")
glimpse(y_train)

# An identifier of the subject who carried out the experiment.
subject_train <- 
  read_table("./UCI HAR Dataset/train/subject_train.txt", col_names = FALSE)
colnames(subject_train) <- "subject"
subject_train <- mutate(subject_train, set = "train")
glimpse(subject_train)

train_data <- cbind(subject_train, y_train, X_train_mean_std) %>% tbl_df()
glimpse(train_data)

# A 561-feature vector with time and frequency domain variables.
X_test <- 
  read_table("./UCI HAR Dataset/test/X_test.txt", col_names = FALSE) 
colnames(X_test) <- as.vector(t(features[,1]))

X_test_mean_std <- select(X_test, contains("-mean()"), contains("-std()"))

y_test <-
  read_table("./UCI HAR Dataset/test/y_test.txt", col_names = FALSE) 
colnames(y_test) <- "label"
y_test <- left_join(y_test, activity_labels, by = "label")

# An identifier of the subject who carried out the experiment.
subject_test <- 
  read_table("./UCI HAR Dataset/test/subject_test.txt", col_names = FALSE)
colnames(subject_test) <- "subject"
subject_test <- mutate(subject_test, set = "test")

test_data <- cbind(subject_test, y_test, X_test_mean_std) %>% tbl_df()
glimpse(test_data)

# Combine train and test data sets
full_data <- rbind(train_data, test_data)

glimpse(full_data)
full_data
full_data <- tibble::rowid_to_column(full_data, "ID")

# Reshaping data

full_gather <- gather(full_data, key = "key", value = "value", `1 tBodyAcc-mean()-X`:`543 fBodyBodyGyroJerkMag-std()`) 
glimpse(full_gather)
full_gather
full_separate <- separate(full_gather, key, into = c("num", "key"), sep = " ") %>% select(- num)
full_separate <- separate(full_separate,  key, into = c("sensor","key","axis"), sep = "-")
full_separate

full_spread <- spread(full_separate, key, value)
full_spread <- mutate(full_spread, set = as.factor(set), activity = as.factor(activity), axis = as.factor(axis), domain = as.factor(substring(sensor,1,1)),  sensor = substring(sensor, 2))
colnames(full_spread)[colnames(full_spread)=="mean()"] <- "mean"
colnames(full_spread)[colnames(full_spread)=="std()"] <- "std"
tidy_data <- select(full_spread, ID, subject, set, activity, domain, sensor, axis, mean, std)
tidy_data
summary(tidy_data)
write_csv(tidy_data, "tidy_data.csv")

tidy_data_avg <- select(tidy_data, -ID) %>% group_by(subject, set, activity, domain, sensor, axis) %>% summarise(avg.mean = mean(mean), avg.std = mean(std))
tidy_data_avg
glimpse(tidy_data_avg)
write_csv(tidy_data_avg, "tidy_data_avg.csv")
write.table(tidy_data_avg, "tidy_data_avg.txt", row.names = FALSE)
