# 1. Merges the training and the test sets to create one data set.
#read the Test and Train files 
#X Test files


x_train <- read.table("train/X_train.txt")
x_test <- read.table("test/X_test.txt")
X <- rbind(x_train, x_test)

#Subject files
subj_train <- read.table("train/subject_train.txt")
subj_test <- read.table("test/subject_test.txt")
S <- rbind(subj_train, subj_test)

#Y files
y_train <- read.table("train/y_train.txt")
y_test <- read.table("test/y_test.txt")
Y <- rbind(y_train, y_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("features.txt")
mean_std_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, mean_std_features]
names(X) <- features[mean_std_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))



# 3. Uses descriptive activity names to name the activities in the data set.
#Use the description from activity_labels.txt file

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"


# 4. Appropriately labels the data set with descriptive activity names.
names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_data.txt")


# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
#For each subject loop through for each activity

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]
row = 1
for (s in 1:numSubjects) {
for (a in 1:numActivities) {
result[row, 1] = uniqueSubjects[s]
result[row, 2] = activities[a, 2]
tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
row = row+1
}
}

# upload your data set as a txt file created with write.table() using row.name=FALSE 
write.table(result, "tidy_data.txt", row.names=FALSE)
