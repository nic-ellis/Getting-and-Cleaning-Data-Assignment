##Assignment Getting and Cleansing Data

## Part 1. Merges the training and the test sets to create one data set

##Download the data
fileURL<-"
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "C:/Users/User/Documents/Data Science/Part 3/Class Project")
unzip("getdata-projectfiles-UCI HAR Dataset.zip")
setwd("~/Data Science/Part 3/Class Project/UCI HAR Dataset")

#Read in the description datasets and label the columns (these will later be used for labels)
install.packages(data.table)
library(data.table)

features<-read.table("C:/Users/User/Documents/Data Science/Part 3/Class Project/UCI HAR Dataset/features.txt", header=FALSE)
activity_labels <-read.table("C:/Users/User/Documents/Data Science/Part 3/Class Project/UCI HAR Dataset/activity_labels.txt", header=FALSE)

colnames(features) <- c("feature_id","feature_name") 
colnames(activity_labels) <- c("activity_id","activity_name")  

# read in the training data sets and label the columns

#subjects
subject_train<- read.table("C:/Users/User/Documents/Data Science/Part 3/Class Project/UCI HAR Dataset/train/subject_train.txt", header=FALSE)
#data
x_train<- read.table("C:/Users/User/Documents/Data Science/Part 3/Class Project/UCI HAR Dataset/train/X_train.txt", header=FALSE)
#labels
y_train<- read.table("C:/Users/User/Documents/Data Science/Part 3/Class Project/UCI HAR Dataset/train/Y_train.txt", header=FALSE)

colnames(subject_train) <- "subject_id"
colnames(y_train) <- "activity_id"
colnames(x_train) <- features$feature_name

# read in the test data sets and label the columns
#subjects
subject_test<- read.table("C:/Users/User/Documents/Data Science/Part 3/Class Project/UCI HAR Dataset/test/subject_test.txt", header=FALSE)
#data
x_test<- read.table("C:/Users/User/Documents/Data Science/Part 3/Class Project/UCI HAR Dataset/test/X_test.txt", header=FALSE)
#labels
y_test<- read.table("C:/Users/User/Documents/Data Science/Part 3/Class Project/UCI HAR Dataset/test/Y_test.txt", header=FALSE)

colnames(subject_test) <- "subject_id"
colnames(y_test) <- "activity_id"
colnames(x_test) <- features$feature_name

# join the test data to the training data using a row bind
Final_X <- rbind(x_train,x_test)
Final_Y <- rbind(y_train,y_test)
Final_Subject <- rbind(subject_train,subject_test)

# combine the subject, x and y data into one dataframe using a column bind
FinalData <- cbind(Final_Subject,Final_Y,Final_X)


## Part 2. Extracts only the measurements on the mean and standard deviation for each measurement

# subset the data and return columns that have the word "mean","std","subject_id" or "activity_id
meanandstdData <- FinalData[,grepl("mean|std|subject_id|activity_id", names(FinalData))]


## Part 3. Uses descriptive activity names to name the activities in the data set

# Merge the meanandstdData within the activity table to bring in descriptive names
mergedData <- merge(meanandstdData,activity_labels,key="activity_id")

# Reorder the columns so the activity name is next to the ativity id
reorderedData<-mergedData[,c(1,82,2:81)]


## Part 4. Labels the data set with descriptive variable names

# Remove parentheses
names(reorderedData) <- gsub('\\(|\\)',"",names(reorderedData))

#Change first letter t/f to Time/Frequency
names(reorderedData) <- gsub("^(t)","Time-",names(reorderedData))
names(reorderedData) <- gsub("^(f)", "Freq-", names(reorderedData))

#change second part of the names
names(reorderedData) <- gsub("Acc", "Acceleration", names(reorderedData))
names(reorderedData) <- gsub("GyroMag","GyroMagnitude", names(reorderedData))
names(reorderedData) <- gsub("JerkMag","JerkMagnitude", names(reorderedData))
names(reorderedData) <- gsub("BodyBody","Body", names(reorderedData))


## Part 5. From the data set in part 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject   
install.packages("plyr")
library(plyr)

DataAvg<-aggregate(. ~subject_id + activity_id, reorderedData, mean)
DataAvg<-DataAvg[order(DataAvg$subject_id,DataAvg$activity_id),]
write.table(DataAvg, "C:/Users/User/Documents/Data Science/Part 3/Class Project/Class Assignment Output.txt")