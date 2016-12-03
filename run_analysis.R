rm(list=ls())
#download and unzip the zip file including the data
if(!file.exists("./data")){dir.create("./data")}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl,destfile="./data/gacdata.zip")
unzip(zipfile="./data/gacdata.zip",exdir="./data")

inputpath <- "./data/UCI HAR Dataset/"
testpath <- paste(inputpath, "test", sep = "")
testfiles <- list.files(testpath)
trainpath <- paste(inputpath, "train", sep = "")
trainfiles <- list.files(trainpath)

subject_test_raw <- read.table(paste(testpath, "\\subject_test.txt", sep = ""))
x_test_raw <- read.table(paste(testpath, "\\X_test.txt", sep = ""))
y_test_raw <- read.table(paste(testpath, "\\y_test.txt", sep = ""))

subject_train_raw <- read.table(paste(trainpath, "\\subject_train.txt", sep = ""))
x_train_raw <- read.table(paste(trainpath, "\\X_train.txt", sep = ""))
y_train_raw <- read.table(paste(trainpath, "\\y_train.txt", sep = ""))

#Step 1. Merges the training and the test sets to create one data set
#let's look at the data a little bit to get an idea how they fit together
unique(subject_test_raw$V1)
#test set includes observations from 9 subjects (2,4,9,10,12,13,18,20,24)
unique(subject_train_raw$V1)
#train set includes observations from the other 21 subjects
summary(y_test_raw)
#Shows that y_test and y_train are probably the activity labels (ranges from 1-6) for boths sets

#let's combine the 3 files into one data frame for both test and train sets
testdataframe <- as.data.frame(cbind(subject_test_raw,y_test_raw,x_test_raw))
traindataframe <- as.data.frame(cbind(subject_train_raw,y_train_raw,x_train_raw))

dim(testdataframe)
dim(traindataframe)
#these two on their own  look fine. Now let's merge them using rbind 
rawdata <- rbind(testdataframe, traindataframe)

#Step 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#Let's download features.txt so we can subset only the columns we want

features <- read.table(paste(inputpath, "\\features.txt", sep = ""))
colnames(rawdata) <- c("subject", "activity",as.character(features[,2]))

#I'm assuming that the variables ending in mean() and std() are mean and std variables, and the ONLY mean and std variables
#NOTE this excludes feature names that have mean or std earlier in the name (it is not clear to me that these are mean or std vars)
meanstddata <- rawdata[, c(1,2,grep("mean\\(\\)", names(rawdata)),grep("std\\(\\)", names(rawdata)))]

#Step 3. Uses descriptive activity names to name the activities in the data set
#we need the activity_labels file to appropriately label these activities 
labels <- read.table(paste(inputpath, "\\activity_labels.txt", sep = ""))
labels$V2


meanstddata$activity <- as.factor(meanstddata$activity)
meanstddata$activity <- gsub(1,labels$V2[1],meanstddata$activity)
meanstddata$activity <- gsub(2,labels$V2[2],meanstddata$activity)
meanstddata$activity <- gsub(3,labels$V2[3],meanstddata$activity)
meanstddata$activity <- gsub(4,labels$V2[4],meanstddata$activity)
meanstddata$activity <- gsub(5,labels$V2[5],meanstddata$activity)
meanstddata$activity <- gsub(6,labels$V2[6],meanstddata$activity)


#Step 4. Appropriately labels the data set with descriptive variable names.
#from the README.txt and the features_info.txt, I will create descriptive variable names using the following replacements:
#t=Time; f=Frequency; Acc=Accelerometer; Gyro=Gyroscope; Mag=Magnitude;mean=Mean; std= StdDev; x=Xaxis; Y=Yaxis; Z=Zaxis
#i also replace "BodyBody" (which I assume to be an error) with "Body" 

names(meanstddata) <- gsub("^t", "Time", names(meanstddata))
names(meanstddata) <- gsub("^f", "Frequency", names(meanstddata))
names(meanstddata) <- gsub("Acc", "Accelerometer", names(meanstddata))
names(meanstddata) <- gsub("Gyro", "Gyroscope", names(meanstddata))
names(meanstddata) <- gsub("Mag", "Magnitude", names(meanstddata))
names(meanstddata) <- gsub("BodyBody", "Body", names(meanstddata))
names(meanstddata) <- gsub("-[Mm]ean\\(\\)", "Mean", names(meanstddata))
names(meanstddata) <- gsub("-[Ss]td\\(\\)", "StdDev", names(meanstddata))
names(meanstddata) <- gsub("-X", "Xaxis", names(meanstddata))
names(meanstddata) <- gsub("-Y", "Yaxis", names(meanstddata))
names(meanstddata) <- gsub("-Z", "Zaxis", names(meanstddata))

#Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(reshape2)
tidydata <- aggregate(. ~ subject + activity, data = meanstddata, mean)
#I assert that this data set is tidy. As can be seen by the dim function, the dimensions of this dataframe are 180 rows and 68 columns
#30 subjects times 6 activies = 180 observations (which are defined as the combination of each subject with each activity) of the 66 variables

write.table(tidydata, file = "data/tidydata.txt", row.names = FALSE)

