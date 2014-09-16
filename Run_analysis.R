# Getting and Cleaning Data Course Project

# Create R Scripts that does the following:
# Merges the training and the test sets to create one data set
# Extracts only the measurements on the mean and standard deviation for each measurement
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names
# From the data set in step 4, create a second independent tidy data set with the average of each variable for each activity and each subject

# Download and Read UCI file into R
 if(!file.exists("data")) {dir.create("data")} 
 	fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
 	download.file(fileUrl, destfile = "uci.zip", method= "curl")
 	 if (!file.exists("uci.zip")) 
 	 	unzip("uci.zip")
 	 
 	 
# Merge the training and the test sets to create one data set
 	 # First, extract data
ucifeatures <- read.table("./UCI HAR Dataset/features.txt", colClasses = c("character"))
uciactivity <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("Id", "Activity"))
ucitestx <- read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE)
ucitesty <- read.table("./UCI HAR Dataset/test/Y_test.txt", header=FALSE)
ucitestsub <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE)
ucitrainx <- read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE)
ucitrainy <- read.table("./UCI HAR Dataset/train/Y_train.txt", header=FALSE)
ucitrainsub <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE)
     
     # Next, merge test set
mergetest <- cbind(cbind(ucitestx, ucitestsub), ucitesty)
      
     # Merge train set
mergetrain <- cbind(cbind(ucitrainx,ucitrainsub), ucitrainy)

     # Merge both training and test sets
ucidata <- rbind(mergetest, mergetrain)

     # Load names column with descriptive variable names 
ucilabels <- rbind(rbind(ucifeatures, c(562, "Subject")), c(563, "Id")) [,2]
columnnames <- ucilabels

# Extract only the measurements on the mean and standard deviation for each measurement

ucidata_mean_std <- ucidata[, grepl("mean|std|Subject|Id", columnnames)]

# Use descriptive activity names to name the activities in the data set
ucidata_mean_std <-join(ucidata_mean_std, uciactivity, type = "left", match = "first")
ucidata_mean_std <- ucidata_mean_std[, -1]

# Appropriately label the data set with descriptive names
# Replace current names with more descriptive names

ucidata_mean_std_names <- make.names(ucidata_mean_std)
ucidata_mean_std_names <- gsub("\\-|\\(|\\)", "", ucidata_mean_std_names)
ucidata_mean_std_names <- gsub('Acc', "Acceleration", ucidata_mean_std_names)
ucidata_mean_std_names <- gsub('Mag', "Magnitude", ucidata_mean_std_names)
ucidata_mean_std_names <- gsub('^t', "Timedomain", ucidata_mean_std_names)
ucidata_mean_std_names <- gsub('Gyro', "Gyroscope", ucidata_mean_std_names)
ucidata_mean_std_names <- gsub('Body', "body", ucidata_mean_std_names)
ucidata_mean_std_names <- gsub('^f', "Frequencydomain", ucidata_mean_std_names)
ucidata_mean_std_names <- gsub('\\.std', ".StandardDeviation", ucidata_mean_std_names)
ucidata_mean_std_names <- gsub('\\.mean', ".Mean", ucidata_mean_std_names)
ucidata_mean_std_names <- gsub('Freq\\.', "Frequency.", ucidata_mean_std_names)
ucidata_mean_std_names <- gsub('Freq$', "Frequency", ucidata_mean_std_names)

# Create a second independent tidy data set with the average of each variable for each activity and subject

library(plyr)
tidydata <- ddply(ucidata_mean_std, c("Id","Activity"), numcolwise(mean))

#save the tidy data set
write.table(tidydata, file = "tidydata.txt")

