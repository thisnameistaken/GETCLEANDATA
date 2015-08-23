analysis <- function(directory) {
  setwd(directory)
  training = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
  training[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
  training[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
  
  testing = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
  testing[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
  testing[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
  
  activitylabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
  
  
  features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
  features[,2] = gsub('-mean', 'Mean', features[,2])
  features[,2] = gsub('-std', 'Std', features[,2])
  features[,2] = gsub('[-()]', '', features[,2])
  

  alldata = rbind(training, testing)
  
  
  colswewant <- grep(".*Mean.*|.*Std.*", features[,2])
  features <- features[colsWeWant,]
  colswewant <- c(colswewant, 562, 563)
  alldata <- allData[,colswewant]
  colnames(allData) <- c(features$V2, "Activity", "Subject")
  colnames(allData) <- tolower(colnames(allData))
  
  currentactivity = 1
  for (currentactivitylabel in activityaabels$V2) {
    allData$activity <- gsub(currentactivity, currentactivitylabel, allData$activity)
    currentactivity <- currentactivity + 1
  }
  

  
  alldata$activity <- as.factor(alldata$activity)
  alldata$subject <- as.factor(alldata$subject)
    
  tidy = aggregate(alldata, by=list(activity = alldata$activity, subject=alldata$subject), mean)
  tidy[,90] = NULL
  tidy[,89] = NULL
  write.table(tidy, "tidy.txt", sep="\t")
}
