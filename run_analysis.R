library(dplyr)

## Load all Data Files
TestX <- read.table("UCI HAR Dataset/test/X_test.txt")
TestY <- read.table("UCI HAR Dataset/test/y_test.txt")
TestSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")

TrainX <- read.table("UCI HAR Dataset/train/X_train.txt")
TrainY <- read.table("UCI HAR Dataset/train/y_train.txt")
TrainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")


##
##
##Change the data in TestY and TrainY to add on the Activity Label
TestY$V1 <- activity_labels[TestY$V1,]$V2
TrainY$V1 <- activity_labels[TrainY$V1,]$V2

##
##
## change the column names on the test data
colnames(TestSubject) <- c("SubjectID")
colnames(TrainSubject) <- c("SubjectID")

colnames(TestY) <-  c("Activity")
colnames(TrainY)<-  c("Activity")

colnames(TestX) <- c(as.vector(features$V2))
colnames(TrainX) <- c(as.vector(features$V2))

##
##
## Create a Vector of only the Columns that are Mean or STD 
## drop all other column names
AllColsNames <- names(TestX)
ColsToKeep <- AllColsNames[grep("mean|std",AllColsNames)]
ColsToKeep <- ColsToKeep[grep("meanFreq",ColsToKeep,invert=TRUE)]

AllColsNames <- paste("Mean of", AllColsNames)

## Create a subset of the data based on those column names
TestX <- TestX[ColsToKeep]
TrainX <- TrainX[ColsToKeep]

## combine the data by column first
TestAll <- cbind(TestSubject,TestY, TestX)
TrainAll <- cbind(TrainSubject,TrainY, TrainX)

## combine the 2 sets of data
AllData <- rbind(TestAll, TrainAll)

## Rename the columns
fixcols <- names(AllData)
fixcols <- sub("mean","Mean",fixcols)
fixcols <- sub("std","StandardDeviation",fixcols)

fixcols <- sub("x$","X",fixcols)
fixcols <- sub("y$","Y",fixcols)
fixcols <- sub("z$","Z",fixcols)

#the above messings up my 2nd columns...quick fix
fixcols[2] <- "Activity"

fixcols <- sub("^t","Time",fixcols)
fixcols <- sub("^f","FastFourierTransform",fixcols)

fixcols <- sub("BodyBodyGyroJerkMag-","BodyBodyGyroscopeJerkMagnitude",fixcols)
fixcols <- sub("BodyBodyAccJerkMag-","BodyBodyAccelerometerJerkMagnitude",fixcols) 
fixcols <- sub("BodyBodyGyroMag-","BodyBodyGyroscopeMagnitude",fixcols) 
fixcols <- sub("BodyAccJerkMag-","BodyAccelerometerJerkMagnitude",fixcols) 
fixcols <- sub("BodyAccMag-","BodyAccelerometerMagnitude",fixcols) 
fixcols <- sub("BodyAccJerk-","BodyAccelerometerJerk",fixcols) 
fixcols <- sub("BodyAcc-","BodyAccelerometer",fixcols) 
fixcols <- sub("BodyGyroJerkMag-","BodyGyroscopeJerkMagnitude",fixcols) 
fixcols <- sub("BodyGyroMag-","BodyGyroscopeMagnitude",fixcols) 
fixcols <- sub("BodyGyroJerk-","BodyGyroscopeJerk",fixcols) 
fixcols <- sub("BodyGyro-","BodyGyroscope",fixcols) 
fixcols <- sub("GravityAccMag-","GravityAccelerometerMagnitude",fixcols) 
fixcols <- sub("GravityAcc-","GravityAccelerometer",fixcols) 

fixcols <- sub("()","",fixcols,fixed=TRUE)

## Apply the new column headers
colnames(AllData) <- fixcols




AllDataMean <- AllData %>% 
                group_by(SubjectID, Activity) %>% 
                summarise_each(funs(mean))



write.table(AllData,"AllData.txt",row.name=FALSE, sep = ",")
write.table(AllData, file = "AllData.csv", sep = ",", col.names = NA, qmethod = "double")

write.table(AllDataMean,"AllDataMean.txt",row.name=FALSE, sep = ",")
write.table(AllDataMean, file = "AllDataMean.csv", sep = ",", col.names = NA, qmethod = "double")


