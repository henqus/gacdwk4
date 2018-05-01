## R cript: run_analysis

library(dplyr)

# 1. Create a data directory        

if (!file.exists("./data")){dir.create("./data")}

# 2. Download the Human Activity Recognition dataset and unzip it

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
localZip <- "./data/all.zip"
unzipped <- "./data/UCI HAR Dataset"

if (!file.exists(localZip)){ download.file(fileUrl, localZip)}

if (!file.exists(unzipped)){ unzip(localZip, exdir = "./data")}

# 3. load the features table and detect mean and standard deviation columns

features <- tbl_df(
        read.table("./Data/UCI HAR Dataset/features.txt", 
                   col.names = c("Id", "Feature")))

# 4. Find features to select

means.and.stds <- grep("mean\\(\\)|std\\(\\)", features$Feature)

# 5. Define tidy names for selected features

new.labels <- grep("mean\\(\\)|std\\(\\)", features$Feature, value = TRUE)
new.labels <- sub("^t", "Time", new.labels)
new.labels <- sub("^f", "Frequency", new.labels)
new.labels <- gsub("BodyBody", "Body", new.labels)
new.labels <- gsub("Body", ".Body", new.labels)
new.labels <- gsub("Acc", ".Acc", new.labels)
new.labels <- gsub("Gyro", ".Gyro", new.labels)
new.labels <- gsub("Jerk", ".Jerk", new.labels)
new.labels <- gsub("Mag", ".Mag", new.labels)
new.labels <- gsub("Gravity", ".Gravity", new.labels)
new.labels <- gsub("-", ".", new.labels)
new.labels <- gsub("\\(\\)", "", new.labels)
new.labels <- gsub("mean", "Mean", new.labels)
new.labels <- gsub("std", "Std", new.labels)

# 6. Load training data, select wanted features, add column names 
#       and add columns for subject-id, activity-id

train <- tbl_df(read.table("./Data/UCI HAR Dataset/train/X_train.txt") %>%
                        select( means.and.stds)
                )
colnames(train) <- new.labels
train <- tbl_df(cbind( 
        rename(tbl_df(read.table("./Data/UCI HAR Dataset/train/subject_train.txt")), 
               subject.id = V1),
        rename(tbl_df(read.table("./Data/UCI HAR Dataset/train/y_train.txt")),
               activity.id = V1),
        train))
        
# 7. Load test data, select wanted features, add column names 
#       and add columns for subject-id, activity-id

test <- tbl_df(read.table("./Data/UCI HAR Dataset/test/X_test.txt") %>%
                       select( means.and.stds)
                )
colnames(test) <- new.labels
test <- tbl_df(cbind( 
        rename(tbl_df(read.table("./Data/UCI HAR Dataset/test/subject_test.txt")), 
               subject.id = V1),
        rename(tbl_df(read.table("./Data/UCI HAR Dataset/test/y_test.txt")),
               activity.id = V1),
        test))

# 8. Merge train and test data and replace activity-id by readable activity

activities <- tbl_df(
        read.table("./Data/UCI HAR Dataset/activity_labels.txt", 
                   col.names = c("Id", "Activity")))

merged <- tbl_df(
        rbind(train, test) %>%
        merge( activities, by.x = "activity.id", by.y = "Id") %>%
        select(subject.id, activity = Activity, everything(), -activity.id) 
        )

# 9. Create nice summary

means <- tbl_df(summarise_all(group_by(merged, subject.id, activity), funs(mean)))

# 10. Write tidy summary to a csv-file called “HAR_avg_per_subject_and_activity.csv”

write.table(means, "HAR_avg_per_subject_and_activity.txt",
          row.names = FALSE)

# run_analysis has FINISHED
