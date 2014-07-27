featurescount = 561

##------------------ Part 1 : Merge training & test dataset -------------------
## Load the activity label dataset and give meaningful name to columns
activities <- read.csv(".//UCI HAR Dataset//activity_labels.txt",
                       sep = " ",
                       header = FALSE,
                       col.names = c("activityid", "activitylabel"))

## Load the subject id dataset
subjecttrain <- read.csv(".//UCI HAR Dataset/train/subject_train.txt",
                         header = FALSE,
                         col.names ="subjectid",
                         colClasses = "factor")
subjecttest <- read.csv(".//UCI HAR Dataset//test//subject_test.txt",
                        header = FALSE,
                        col.names ="subjectid",
                        colClasses = "factor")
subjecttotal <- rbind(subjecttrain, subjecttest)

## Load features' descriptions
featuresnames <- read.csv(".//UCI HAR Dataset//features.txt",
                          sep =" ",
                          header = FALSE,
                          stringsAsFactors = FALSE)[, 2]

## Get a logical vector of the features to be kept
meanstandardfeatures = featuresnames[grepl(".*(mean|std).*", featuresnames)]
idxSelectedFeatures <- featuresnames %in% meanstandardfeatures

## Create a list of columns to be selected while invoking read.fwf.
## Features are 16 characters wide splitted in 1 whitespace character to be
## removed + 15 characters that must be selected if belonging to a mean/std
## columns and removed otherwise. Thus we generate a sequence of 561 occurrences
## of the folowing pattern: (-1, 15) for a colnumn to be retain in the final
## result set. (-1, -15) for a column to be excluded from the final result set.
widthsColumns <- rep(c(-1, 15), featurescount)
dim(widthsColumns) <- c(2, featurescount)
widthsColumns[2, ] <- widthsColumns[2, ]*ifelse(idxSelectedFeatures, 1, -1)
dim(widthsColumns) = 2*featurescount

## Load the training dataset by leveraging the previously built vector of
## column to be kept
xtrain <- read.fwf(".//UCI HAR Dataset//train//X_train.txt",
                   widths = widthsColumns,
                   col.names = meanstandardfeatures,
                   header = FALSE,
                   comment.char = "",
                   n = 11500)
xtest <- read.fwf(".//UCI HAR Dataset//test//X_test.txt",
                  widths = widthsColumns,
                  col.names = meanstandardfeatures,
                  header = FALSE,
                  comment.char = "",
                  n = 3000)
xtotal <- rbind(xtrain, xtest)

## Read the ativity id's data set and give meaningful names to columns
ytrain <- read.csv(".//UCI HAR Dataset//train//y_train.txt",
                   header = FALSE,
                   col.names = "activityid")
ytest <- read.csv(".//UCI HAR Dataset//test//y_test.txt",
                  header = FALSE,
                  col.names = "activityid")
ytotal <- rbind(ytrain, ytest)

## Merge 'activity id' dataset with the 'activity label' dataset
ytotal <- data.frame(activitylabel = merge(ytotal,
                                           activities,
                                           by = "activityid")[,2])

## Produce the final dataset
data <- cbind(subjecttotal, ytotal, xtotal)

## Clean up memory
rm(list=c("subjecttest", "subjecttrain", "subjecttotal",
          "xtest", "xtrain", "xtotal",
          "ytest", "ytrain", "ytotal",
          "activities", "featuresnames", "idxSelectedFeatures",
          "widthsColumns", "meanstandardfeatures", "featurescount"))

##-- Part 2 : For each subjectid/activitylabel tuple calculate mean of all features --
## For all numeric columns in the dataset except the first two columns (subjectid/activitylabel)
## calculate the mean of the column
datacumul <- aggregate(data[,3:dim(data)[2]],
                       by = list(subjectid = data$subjectid, activitylabel = data$activitylabel),
                       mean)
write.csv(data, file = ".//TidyDataset.csv", row.names = FALSE)
write.csv(datacumul, file = ".//TidyAggregatedDataset.csv", row.names = FALSE)

