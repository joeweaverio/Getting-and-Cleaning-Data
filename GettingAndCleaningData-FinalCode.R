# Instructions
# 
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
# Review criterialess 
# 
# The submitted data set is tidy.
# The Github repo contains the required scripts.
# GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
# The README that explains the analysis files is clear and understandable.
# The work submitted for this project is the work of the student who submitted it.
# 
# Getting and Cleaning Data Course Projectless 
# 
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
# 
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#     
#     http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
#     
#     https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following.
# 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# 
# Good luck!
myDir <- "C:/Users/Joseph/OneDrive/BigData/rstudio-workdr/3-Get-CleanData/final/data";

if(!file.exists(myDir)){
    dir.create(path = myDir);
    
    if(myDir == myDir){setwd(myDir); print(myDir);}
    getwd();
    dir()
    
} else {if(myDir == getwd()){setwd(myDir); print(myDir); getwd(); dir(no.. = F)}}

rm(list = ls())

#load libs
library(httr) 
library(dplyr)
library(data.table)

#download data
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    fileName <- "project-dataset.zip"
    download.file(fileUrl,destfile=fileName, mode = "wb", method = "auto")

#List zip contents 
    unzip(fileName, list = T);
    
#Unzip contents
    unzip(fileName, list = F, overwrite = T)

#get features/variables data.table


#func getfeature set test or train type
    getSetDT <- function(fname, columnName = NULL)
    {
        library(data.table);

        if(file.exists(fname) && is.null(columnName)) 
        {dt <- data.table(read.table(fname))}
        else if (file.exists(fname) && !is.null(columnName)) 
            {dt <- data.table(read.table(fname, col.names = columnName))}
        else 
        {
            print(paste("filename does not exist", fname))
            stop();
        }
        return(dt);
    }
    
    getFeatureSetDT <- function(feature, columnName)
    {
        f <- paste0("UCI HAR Dataset/", feature)
        allFiles <- list.files(f, full.names = T, no.. = T, pattern = "txt$");

        library(data.table);
        
        for(fid in seq_along(allFiles))
        {
            fname <- allFiles[fid]
            
            if(grepl(pattern = "X_", ignore.case = T, x = fname)) {xDT <- fread(fname, col.names = as.character(columnName$V2));}
            if(grepl(pattern = "Y_", ignore.case = T, x = fname)) {yDT <- fread(fname, col.names = "Activity");}
            if(grepl(pattern = "subject", ignore.case = T, x = fname)) {subDT <- fread(fname, col.names = "ID");}
        }
        
        return(cbind(subDT, yDT, xDT))
    }
    
    saveCSV <- function (data, fname){
        write.csv(data,fname)
    }
    
    features <- "UCI HAR Dataset/features.txt";  
    setFeatures <- getSetDT(features);
    testDataSet <- getFeatureSetDT("test", setFeatures)
    traingDataSet <- getFeatureSetDT("train", setFeatures)
    
    
# Merges the training and the test sets to create one data set.
    mMainDataSet <- rbind(testDataSet, traingDataSet)
    inx <- order(mMainDataSet$ID, na.last = T)
    ordMainDataSet <- mMainDataSet[inx]

#Extracts only the measurements on the mean and standard deviation for each measurement.    
    only_mean_std <- ordMainDataSet[,c(1,2,grep("std", colnames(ordMainDataSet)), grep("mean", colnames(ordMainDataSet)))]
    saveCSV(only_mean_std,"only_mean_std.csv") 

# Uses descriptive activity names to name the activities in the data set
    activity_labels <- getSetDT("UCI HAR Dataset/activity_labels.txt")


# Appropriately labels the data set with descriptive variable names.    
    ordMainDataSet$Activity <- factor(ordMainDataSet$Activity, levels=activity_labels$V1, labels=activity_labels$V2)

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.   
    library(plyr)
    tidyDS <- ddply(ordMainDataSet, .(ID, Activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
    colnames(tidyDS)[-c(1:2)] <- paste(colnames(tidyDS)[-c(1:2)], "_mean", sep="")
    saveCSV(tidyDS,"tidyDS.csv")
