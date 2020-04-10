
## Libraries
library(dplyr)
library(tidyr)
library(magrittr)

## Preparing dataset

## COlumn names for dataset
features = read.table("UCI HAR Dataset/features.txt",col.names = c("n","functions"))
## activity labels to be merged into dataset by "code"
activity = read.table("UCI HAR Dataset/activity_labels.txt",col.names = c("code","activity"))
## row names for test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
## Record entries for test data matched by column names
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
## "code" to match activity labels in test data
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
## row names for train data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
## Record entries for train data matched by column names
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
## "code" to match activity labels in test data
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

## 1. MERGES THE TRAINING & THE TEST SETS TO CREATE ONE DATA SET 
## to merge test &  train records  
X <- rbind(x_train, x_test)
## to merge test & train codes
Y <- rbind(y_train, y_test)
## to merge test & train subjects
Subject <- rbind(subject_train, subject_test)
## to merge subjects, records & codes to make one master dataset
Merged_Data <- cbind(Subject, Y, X)

## 2.	EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION FOR EACH MEASUREMENT
extract <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

## 3. USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET
## Merging by "code"
extract_activity <- merge(activity,extract,"code")
## Removing variable "code"
extract_activity <- extract_activity[,-1]

## 4.APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES
## using gsub function to find & replace
names(extract_activity)<-gsub("^f", "Frequency", names(extract_activity))
names(extract_activity)<-gsub("tBody", "TimeBody", names(extract_activity))
names(extract_activity)<-gsub("-mean()", "Mean", names(extract_activity), ignore.case = TRUE)
names(extract_activity)<-gsub("-std()", "STD", names(extract_activity), ignore.case = TRUE)
names(extract_activity)<-gsub("-freq()", "Frequency", names(extract_activity), ignore.case = TRUE)
names(extract_activity)<-gsub("angle", "Angle", names(extract_activity))
names(extract_activity)<-gsub("gravity", "Gravity", names(extract_activity))
names(extract_activity)<-gsub("Acc", "Accelerometer", names(extract_activity))
names(extract_activity)<-gsub("Gyro", "Gyroscope", names(extract_activity))
names(extract_activity)<-gsub("BodyBody", "Body", names(extract_activity))
names(extract_activity)<-gsub("Mag", "Magnitude", names(extract_activity))
names(extract_activity)<-gsub("^t", "Time", names(extract_activity))

## final dataset
tidy_dataset <- extract_activity

 ## 5. FROM THE DATA SET IN STEP 4, CREATES A SECOND, INDEPENDENT TIDY DATA SET WITH THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT
tidydata_mean <- tidy_dataset %>% group_by(subject,activity) %>% summarise_all(funs(mean))
write.table(tidydata_mean, "tidydata_mean.txt", row.name=FALSE)