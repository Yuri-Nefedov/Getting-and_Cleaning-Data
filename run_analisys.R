runAnalysis <- function() {
  
  dataDir <- "./UCI HAR Dataset" 
  filePath <- function(...) { paste(..., sep = "/") }
  readData <- function(path) {
    read.table(filePath(dataDir, path))
  }
  
  # (1) Merging the training and the test sets to create one data set
  X_Train <- readData("train/X_train.txt")
  print("XTrain completed")
  X_Test  <- readData("test/X_test.txt")
  print("XTest completed")
  merged <- rbind(X_Train, X_Test)
  print("Merged")
  
  # Assigning feature names
  featureNames <- readData("features.txt")[, 2]
  names(merged) <- featureNames
  
  # (2) Extracting only the measurements on the mean and standard deviation for each measurement
  matches <- grep("(mean|std)\\(\\)", names(merged)) # find mean() or std()
  limited <- merged[, matches]
  print("Means & std() extracted")
  
  # (3) Using descriptive activity names to name the activities in the data set
  yTrain <- readData("train/y_train.txt")
  yTest  <- readData("test/y_test.txt")
  yMerged <- rbind(yTrain, yTest)[, 1]
  
  activityNames <-
    c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
  activities <- activityNames[yMerged]
  
  # (4) Appropriately labeling the data set with descriptive variable names
 
  names(limited) <- gsub("^t", "Time", names(limited))        # Change t to Time
  names(limited) <- gsub("^f", "Frequency", names(limited))   # Change f to Frequency
  names(limited) <- gsub("-mean\\(\\)", "Mean", names(limited)) # Change mean() to Mean
  names(limited) <- gsub("-std\\(\\)", "StdDev", names(limited)) # Change std() to StdDev
  names(limited) <- gsub("-", "", names(limited))             # Remove extra dashes
  names(limited) <- gsub("BodyBody", "Body", names(limited))  # Remove BodyBody error
  
  # Add activities and subject with nice names
  subjectTrain <- readData("train/subject_train.txt")
  subjectTest  <- readData("test/subject_test.txt")
  subjects <- rbind(subjectTrain, subjectTest)[, 1]
  
  tidy <- cbind(Subject = subjects, Activity = activities, limited)
  
  # (5) Creating a second, independent tidy data set with 
  # the average of each variable for each activity and each subject
  library(plyr)
  # Column means for all but the subject and activity columns
  limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
  
  tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans)
  names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])
  
  # Write file
  write.table(tidyMeans, "tidyMeans.txt", row.names = FALSE)
  print("tidyMeans.txt written")
  
  X_Train <- NULL
  X_Test <- NULL 
} 