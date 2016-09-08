# step 0 download the data set
# download ZIP
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

# unzip to directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

# step 1. Merges the training and the test sets to create one data set.
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
features <- read.table("./data/UCI HAR Dataset/features.txt")
Activity <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

# add column names
colnames(x_test) <- features[,2]
colnames(x_train) <- features[,2]
colnames(y_test) <- "activityID"
colnames(y_train) <- "activityID"
colnames(subject_test) <- "userID"
colnames(subject_train) <- "userID"
colnames(Activity) <- c("activityID", "Activity")

  #combine the datasets
Train <- cbind(subject_train, y_train, x_train)
Test<- cbind(subject_test, y_test, x_test)
Complete <- rbind(Train, Test)

# step 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# extract colmn numners with measurements on mean and std from features list
ms<- grep(".*mean.*|.*std.*", features[,2])
ms2 <- c(1,2, ms+2)

# extract those columens from dataset

MSset <- Complete[ms2]

# step 3. Uses descriptive activity names to name the activities in the data set

#merge activity name into MSset
MSActivity <-   join(Activity, MSset, by = "activityID")
MSAct <- MSActivity[,2:82]

# Move ID to first column
col_idx <- grep("userID", names(MSAct))
AllData <- MSAct[, c(col_idx, (1:ncol(MSAct))[-col_idx])]

# step 4. Appropriately labels the data set with descriptive variable names.

# create clean list for data labels
names <- features[ms,2]
names <- gsub('-mean', 'Mean', names)
names <- gsub('-std', 'Std', names)
names <- gsub('[-()]', '', names)

colnames(AllData) <- c("userID","Activity", names)

# step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#create new set with mean for each user + activity
TD<-aggregate(. ~userID + Activity, AllData, mean)
TD<-TD[order(TD$userID,TD$Activity),]

#write txt file
write.table(TD, "tidydata.txt", row.names = FALSE, quote = FALSE)