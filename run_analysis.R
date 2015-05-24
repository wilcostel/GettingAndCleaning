# ------------------------------------------------------------------------------
# Process the "Human Activity Recognition Using Smartphones" dataset
#
# Did as part of coursera project work
# Author: Wilco Stel
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Step 1 - Merge the training and the test sets 
# ------------------------------------------------------------------------------

# training data
train_x <- read.table("data//train/X_train.txt", nrows=7352, comment.char="")
train_sub <- read.table("data//train/subject_train.txt", col.names=c("subject"))
train_y <- read.table("data/train//y_train.txt", col.names=c("activity"))
train_data <- cbind(train_x, train_sub, train_y)

# test data
test_x <- read.table("data//test/X_test.txt", nrows=2947, comment.char="")
test_sub <- read.table("data/test/subject_test.txt", col.names=c("subject"))
test_y <- read.table("data/test//y_test.txt", col.names=c("activity"))
test_data <- cbind(test_x, test_sub, test_y)

# merge both training and test data
data <- rbind(train_data, test_data)


# ------------------------------------------------------------------------------
# Step 2 - Extracts only the measurements on the mean and standard 
# deviation for each measurement.
# ------------------------------------------------------------------------------

feature_list <- read.table("data//features.txt", col.names = c("id", "name"))
features <- c(as.vector(feature_list[, "name"]), "subject", "activity")


# filter only features that has mean or std in the name

filtered_feature_ids <- grepl("mean|std|subject|activity", features) & !grepl("meanFreq", features)
filtered_data = data[, filtered_feature_ids]

# ------------------------------------------------------------------------------
# Step 3 - Uses descriptive activity names
# ------------------------------------------------------------------------------


activities <- read.table("data//activity_labels.txt", col.names=c("id", "name"))
for (i in 1:nrow(activities)) {
  filtered_data$activity[filtered_data$activity == activities[i, "id"]] <- as.character(activities[i, "name"])
}

# ------------------------------------------------------------------------------
# step 4 - Label the data set with descriptive variable names.
# ------------------------------------------------------------------------------

filtered_feature_names <- features[filtered_feature_ids]
filtered_feature_names <- gsub("\\(\\)", "", filtered_feature_names)
filtered_feature_names <- gsub("Acc", "-Acceleration", filtered_feature_names)
filtered_feature_names <- gsub("Mag", "-Magnitude", filtered_feature_names)
filtered_feature_names <- gsub("^t(.*)$", "\\1-time", filtered_feature_names)
filtered_feature_names <- gsub("^f(.*)$", "\\1-frequency", filtered_feature_names)
filtered_feature_names <- gsub("(Jerk|Gyro)", "-\\1", filtered_feature_names)
filtered_feature_names <- gsub("BodyBody", "Body", filtered_feature_names)
filtered_feature_names <- tolower(filtered_feature_names)


names(filtered_data) <- filtered_feature_names

# ------------------------------------------------------------------------------
# step 5 - Create a tidy data set
# with the average of each variable for each data activity and each subject.
# ------------------------------------------------------------------------------


group_filtered_data<-group_by(filtered_data,subject,activity)
tidy_data<-group_filtered_data %>%
  summarise_each(funs(mean)) %>%
  gather(measurement, mean, -activity, -subject)

# Save the data set
write.table(tidy_data, file="tidy_data.txt", row.name=FALSE)
