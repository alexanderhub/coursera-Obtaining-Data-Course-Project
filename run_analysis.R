## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Prepare workspace
rm(list=ls())   # clean up
setwd("~/Documents/ADdocuments/DataScience/repos/coursera-Obtaining-Data-Course-Project")

if (!require(plyr)) {
        install.packages(plyr)
}
if (!require(httr)) {
        install.packages(httr)
}

require(httr) 
require(plyr)

# Download data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileLocal <- "data.zip"
if (!file.exists(fileLocal)) {
        print("dowloading file")
        download.file(fileUrl, destfile=fileLocal, method="curl")
}

# Unzip and create folders (if they don't exist already)
dataFolder <- "UCI HAR Dataset"
resultsFolder <- "results"
if (!file.exists(dataFolder)) {
        print("unzipping file")
        unzip(fileLocal)
} 
if (!file.exists(resultsFolder)) {
        print("creating results folder")
        dir.create(resultsFolder)
} 

# Read txt and convert to data.frame
gettables <- function (filename,cols = NULL) {
        print(paste("Getting table:", filename))
        f <- paste(dataFolder,filename,sep="/")
        data <- data.frame()
        if(is.null(cols)) {
                data <- read.table(f,sep="",stringsAsFactors=F)
        } else {
                data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
        }
        data
}

# Run and check gettables
features <- gettables("features.txt")

# Read data and build database
getdata <- function(type, features){
        print(paste("Getting data", type))
        subject_data <- gettables(paste(type,"/","subject_",type,".txt",sep=""),"id")
        y_data <- gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
        x_data <- gettables(paste(type,"/","X_",type,".txt",sep=""),features$V2)
        return (cbind(subject_data,y_data,x_data))
}

# Run and check getdata
test <- getdata("test", features)
train <- getdata("train", features)

# Save the resulting data in the indicated folder
saveresults <- function (data,name){
        print(paste("saving results", name))
        #file <- paste(resultsFolder, "/", name,".csv" ,sep="")
        #write.csv(data,file)
        file <- paste(resultsFolder, "/", name,".txt" ,sep="")
        write.table(data,file,row.names=FALSE,sep='\t');
}

### Required tasks ###

#1) Merge the training and the test sets to create one data set.
data <- rbind(train, test)
data <- arrange(data, id)

#2) Extract only the measurements on the mean and standard deviation for each measurement. 
mean_and_std <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveresults(mean_and_std,"mean_and_std")

#3) Use descriptive activity names to name the activities in the data set.
activity_labels <- gettables("activity_labels.txt")

#4) Appropriately label the data set with descriptive variable names. 
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

#5) Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidy_dataset <- ddply(mean_and_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")
saveresults(tidy_dataset,"tidy_dataset")
