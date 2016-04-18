file <- "dataset.zip"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataset_dir <- "UCI HAR Dataset"
result_dir <- "Results"

## FUNCTIONS
## read table from the data file and apply cols
getTable <- function (filename,cols = NULL){
    
    print(paste("Reading table...", filename))
    
    f1 <- unz(file, paste(dataset_dir, filename, sep="/"))
    
    data <- data.frame()
    
    if(is.null(cols)){
        data <- read.table(f1, sep="", stringsAsFactors=FALSE)
    } else {
        data <- read.table(f1, sep="", stringsAsFactors=FALSE, col.names= cols)
    }
    
    
    data
    
}
## Read and create a complete data set
getData <- function(type, features){
    
    print(paste("Getting data...", type))
    
    subject <- getTable(paste(type, "/", "subject_", type, ".txt", sep=""), "id")
    y <- getTable(paste(type, "/", "y_", type, ".txt", sep=""), "activity")    
    x <- getTable(paste(type, "/", "X_", type, ".txt", sep=""), features$V2) 
    
    return (cbind(subject,y,x)) 
}

## Write the data into the result folder
writeData <- function (data, name){
    
    print(paste("Saving data...", name))
    
    file <- paste(result_dir, "/", name, ".csv" ,sep="")
    write.csv(data,file)
}

## write the data as txt
writeTxt <- function (data, name){
    
    print(paste("Saving data...", name))
    
    file <- paste(result_dir, "/", name, ".txt" ,sep="")
    write.table(data, file, row.names=FALSE)
}

##Install required packacets  
if(!is.element("plyr", installed.packages()[,1])){
    print("Installing packages")
    install.packages("plyr")
}

## create dir
if(!file.exists(result_dir)){
    print("Creating result dir")
    dir.create(result_dir)
}

## download data  
if(!file.exists(file)){    
    ##Downloads the data file
    print("downloading data")
    download.file(url,file, mode = "wb")
}

## common data tables
#features used for col names when creating train and test data sets
features <- getTable("features.txt")

## load the datasets
train <- getData("train",features)
test <- getData("test",features)

## 1. Merges the training and the test sets to create one data set.
# merge datasets
data <- rbind(train, test)

# rearrange the data using id
data <- arrange(data, id)


## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.

activity_labels <- getTable("activity_labels.txt")

data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)



## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
dataset_one <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]


# write dataset_one into results folder
writeData(dataset_one,"dataset_one")

## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
library(plyr)
dataset_two <- ddply(dataset_one, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })

# Adds "_mean" to colnames
colnames(dataset_two)[-c(1:2)] <- paste(colnames(dataset_two)[-c(1:2)], "_mean", sep="")

# write tidy dataset2 into results dir
writeData(dataset_two,"dataset_two")

# write tidy data as txt
writeTxt(dataset_two, "tidyData")


