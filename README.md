#Getting and Cleaning Data: Course Project

1.	Download and unzip the zip file which contains the dataset
2.	Read in the various files to begin looking at the data
3.	Explore the structure of the different files to discover how they interact and what should be merged together for analysis
4.	Create one dataset for the train set and one for the test set. This is done by combining the subject, x and y files together using cbind.
5.	Merges the training and the test sets to create one data set using rbind. We now have one full raw data set with which to move forward
6.	Extracts only the measurements on the mean and standard deviation for each measurement. (Note: I'm assuming that the variables ending in mean() and std() are mean and std variables, and the ONLY mean and std variables. This EXCLUDES feature names that have mean or std earlier in the name as it is not clear to me that these are mean or standard deviation variables.)
7.	Uses descriptive activity names to name the activities in the data set. I take these directly from the activity_labels file included with the data set. 
8.	Appropriately labels the data set with descriptive variable names. After careful consideration of both the README.txt and the features_info.txt files, I will create descriptive variable names using the following replacements: t=Time; f=Frequency; Acc=Accelerometer; Gyro=Gyroscope; Mag=Magnitude;mean=Mean; std= StdDev; x=Xaxis; Y=Yaxis; Z=Zaxis. I also replace "BodyBody" (which I assume to be an error) with "Body"
9.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  The definition of a tidy data set is: 1) Each variable you measure should be in one column 2) Each different observation of that variable should be in a different row 3) There should be one table for each kind of variable.  The assignment asks to for a tidy data set with the average of each variable for each activity and each subject. As can be seen by the dim function, the dimensions of this dataframe are 180 rows and 68 columns. 30 subjects times 6 activities = 180 observations (which are defined as the combination of each subject with each activity) of the averages of the 66(the first two columns are the subject and activity) standard deviation and mean variables.
