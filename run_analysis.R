
## Set the working directory


if (!dir.exists("Project3")){
  dir.create("Project3")
}
  
setwd("Project3")

## download and unzip the dataset
filename <- "getdata_dataset.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

setwd("UCI HAR Dataset")

## Load the data
features <- read.table("features.txt", header = FALSE, sep = "")
xtest <- read.table("test/X_test.txt", header = FALSE, sep = "")
ytest <- read.table("test/y_test.txt", header = FALSE, sep = "")
xtrain <- read.table("train/X_train.txt", header = FALSE, sep = "")
ytrain <- read.table("train/y_train.txt", header = FALSE, sep = "")
subject_test <- read.table("test/subject_test.txt", header = FALSE, sep = "")
subject_train <- read.table("train/subject_train.txt", header = FALSE, sep = "")

# Create one dataset by merging the training and the test data sets
values <- rbind(xtest, xtrain)

#extract the column names
factors <- features[,2]
character <- as.character(factors)
colnames(values) <- c(character)

# merge activities from test and train dataset
activity <- rbind(ytest, ytrain)
  
# merge subjects from test and train dataset
subjects <- rbind(subject_test, subject_train)

# create the whole data set containing #(values, activity,subjects)
big <- cbind(subjects, activity, values)

# subset mean & std & put together 
# Extracting only the relevant measurements #(mean, standard-deviation) for
# each measurement 
mean <- big[,grepl("mean", names(big))]
std <- big[,grepl("std", names(big))]
mean_std <- cbind(subjects, activity, mean, std)

# Use desrcitptiva activity by renaming the first two columns 
# to "subjects_id" and "activity_id" 
colnames(mean_std)[1] <- "subject_id"
colnames(mean_std)[2] <- "activity_id"

# Adding the descriptive activities
# First creating an activity table containing the
# activity ids and the descriptions, then merge the activity table with
# the table with the observations.
activity_desc <- c("walking", "walking_up", "walking_down", "sitting", "standing", "laying")
activity_id <- seq(1, 6)
activity_table <- data.frame(activity_id, activity_desc)

dataset_with_activity_names <- merge(x = mean_std, y = activity_table, 
                                     by = "activity_id", all.x = TRUE)

# Rename the column names to make them more descriptive
# "t" -> "time, "f" -> "frequency", "Gyro" -> "Gyroscope", 
# "Acc" -> "Acceleration", "Mag" -> "Magnitude" 

#colnames(df) <- gsub('.', '', colnames(df), fixed=TRUE)
colnames(dataset_with_activity_names) <- gsub("^t", "time", 
                                              colnames(dataset_with_activity_names),
                                              fixed=FALSE)
colnames(dataset_with_activity_names) <- gsub("^f", "frequency", 
                                              colnames(dataset_with_activity_names),
                                              fixed=FALSE)
colnames(dataset_with_activity_names) <- gsub("Gyro", "Gyroscope", 
                                              colnames(dataset_with_activity_names),
                                              fixed=FALSE)
colnames(dataset_with_activity_names) <- gsub("Acc", "Acceleration", 
                                              colnames(dataset_with_activity_names),
                                              fixed=FALSE)
colnames(dataset_with_activity_names) <- gsub("Mag", "Magnitude", 
                                              colnames(dataset_with_activity_names),
                                              fixed=FALSE)
install.packages("dplyr") ##for the grouping function
library(dplyr)

## Use dyplr group_by to group the rows with the same subject_id and activity.

activity_grouped <- group_by(dataset_with_activity_names, activity_desc, subject_id)

## Use dyplr to collaps the groups into one row by calculating the mean for
## each variable and observation.

activity_mean <- summarise_all(activity_grouped, funs(mean), na.rm = TRUE)
activity_mean$activity_id <- NULL
write.table(activity_mean, "result.txt")

