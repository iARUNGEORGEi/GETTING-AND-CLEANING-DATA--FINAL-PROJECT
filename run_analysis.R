# READING THE DATAA

library(dplyr)                                                           # Loading the package
getwd()
list.files()                                                             # Checking for necessary .txt files in the project directory                                                             
features <- read.table("features.txt")                                  # Reading features.txt data set
colnames(features) <- c("n", "functions")                               
x_test <- read.table("X_test.txt", col.names = features$functions)      # Changing column names of the X_test file
activities <- read.table("activity_labels.txt")                        # Reading activity_labels.txt data set
colnames(activities) <- c("code", "activity")                          # Changing column names of the activities 
y_test <- read.table("y_test.txt")
colnames(y_test) <- "code"
subject_test <- read.table("subject_test.txt", col.names = "subject")
subject_train <- read.table("subject_train.txt", col.names = "subject")
x_train <- read.table("X_train.txt", col.names = features$functions)
y_train <- read.table("y_train.txt", col.names = "code")

# OBJECTIVE 1: Merging Training and Test Data sets of 3 different categories 

x_test
x_train
X_total <- rbind(x_train, x_test)                     # Row binding of 'X' training and test data set

Y_total <- rbind(y_train, y_test)                     # Row binding of 'Y' training and test data set
subject_total <- rbind(subject_train, subject_test)   # Row binding of 'subject' training and test data set 
final_data_set <- cbind(subject_total, X_total, Y_total)
head(final_data_set)


# OBJECTIVE 2: Extracting Mean and Standard Deviation of Individual Measurements

extracted_data_set <- final_data_set %>% select(subject, code, contains("mean"), contains("std"))
head(extracted_data_set)


# OBJECTIVE 3: Replacing the codes with the respective activity names

activities
extracted_data_set$code
extracted_data_set$code <- activities[extracted_data_set$code, 2]
extracted_data_set$code
extracted_data_set

# OBJECTIVE 4: Assigning valid names to measurement parameters.

names(extracted_data_set) [2] <- "activity"
names(extracted_data_set) <- gsub("Acc", "Accelerometer", names(extracted_data_set))
names(extracted_data_set) <- gsub("Gyro", "Gyroscope", names(extracted_data_set))
names(extracted_data_set) <- gsub("BodyBody", "Body", names(extracted_data_set))
names(extracted_data_set) <- gsub("Mag", "Magnitude", names(extracted_data_set))
names(extracted_data_set) <- gsub("^t", "Time", names(extracted_data_set))
names(extracted_data_set) <- gsub("^f", "Frequency", names(extracted_data_set))
names(extracted_data_set) <- gsub("tBody", "TimeBody", names(extracted_data_set))
names(extracted_data_set) <- gsub(".mean...", "-Mean()-", names(extracted_data_set), ignore.case = TRUE)
names(extracted_data_set) <- gsub(".std...", "-STD()-", names(extracted_data_set), ignore.case = TRUE)
names(extracted_data_set) <- gsub(".freq...", "Frequency-", names(extracted_data_set), ignore.case = TRUE)
names(extracted_data_set) <- gsub("angle", "Angle", names(extracted_data_set))
names(extracted_data_set) <- gsub("gravity", "Gravity", names(extracted_data_set))

extracted_data_set

# OBJECTIVE 5: To find mean of all the variables

Final_Mean <- extracted_data_set %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
View(Final_Mean)
write.table(Final_Mean, "Final_Mean.txt", row.names = FALSE)

str(Final_Mean)
