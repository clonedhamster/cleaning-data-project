readFeatures <- function(featureData) {
  f <- read.csv(featureData, sep = "", header = FALSE)  
  f[, 2] <- gsub("[-()]", "" ,f[, 2])
  f[, 2] <- gsub(",", "" ,f[, 2])  
  return(f[, 2])
}

readActivities <- function(fileActivities) {
  activities <- read.csv(fileActivities, sep = "", header = FALSE)
  activities[, 2] <- gsub("_", "", activities[, 2])
  activities[, 2] <- tolower(activities[, 2])
  return(activities)
}

readXdata <- function()
{
 train <- read.csv(file = 'train/X_train.txt', sep = '', header = FALSE)
 test <- read.csv(file = 'test/X_test.txt', sep = '', header = FALSE)
 X <- rbind(train, test)
 f <- readFeatures('features.txt')
 colnames(X) <- f
 return(X)
}

readYdata <- function() {
  train <- read.csv(file = 'train/y_train.txt', sep = '', header = FALSE)
  test <- read.csv(file = 'test/y_test.txt', sep = '', header = FALSE)
  activities <- readActivities('activity_labels.txt')
  Y <- rbind(train, test)
  Y[, 1] <- activities[Y[, 1], 2]
  colnames(Y) <- 'activities'
  return(Y)
}

readSubjects <- function() {
  train <- read.csv(file = 'train/subject_train.txt', sep = '', header = FALSE)
  test <- read.csv(file = 'test/subject_test.txt', sep = '', header = FALSE)
  S <- rbind(train, test)
  colnames(S) <- 'subject'
  return(S)
}

readAndFilter <- function() {
  S <- readSubjects()
  Y <- readYdata()
  X <- readXdata()
  goodFeatures <- grep("mean|std", tolower(colnames(X)))
  X <- X[, goodFeatures]
  cleaned <- cbind(S, Y, X)
  return(cleaned)
}

tidyDataSet <- function(data) {
  uniqueSubjects <- unique(data[, 1])
  uniqueActivities <- unique(data[, 2])
  ndatacols <- dim(data)[2]
  
  resultDf <- data.frame(matrix(ncol = ndatacols - 2, nrow = 1))
  subjects <- data.frame(matrix(ncol = 1, nrow = 1))
  acts <- data.frame(matrix(ncol = 1, nrow = 1))
  
  
  for(s in uniqueSubjects) {
    tmpS <- subset(data, data$subject == s)
    for(a in uniqueActivities) {
      tmpA <- subset(tmpS, tmpS$activities == a)
      meanVals <- colMeans(tmpA[,3:ndatacols])
      resultDf <- rbind(resultDf, meanVals )
      subjects <- rbind(subjects, s)
      acts <- rbind(acts, a)
    }
  }
  
  resultDf <- cbind(subjects, acts, resultDf)
  colnames(resultDf) <- colnames(data) 
  return(resultDf[complete.cases(resultDf), ])
}

cleanedData <- readAndFilter()
write.table(cleanedData, "merged_clean_data.txt")
write.table(tidyDataSet, "tidy_set.txt")