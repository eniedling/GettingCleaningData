#Load required libraries
library(dplyr)
library(data.table)
library(tidyr)

# get the path of the executing script and set as working dir
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

#check whether folder 'data' exists, if not create it
if (!file.exists("data")) {  dir.create("data") }

#check whether the zip file exisits, if not download it
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileLocal <- "./data/gcd_project.zip"
if (!file.exists("./data/gcd_project.zip")) {
  download.file(fileUrl, destfile = fileLocal)
}

#unzip the files
unzip(fileLocal, exdir = "./data", overwrite = TRUE)

# data files are assummed in the same folder or sub-folders
fileActivityLabels = "./data/UCI HAR Dataset/activity_labels.txt"
fileFeatures = "./data/UCI HAR Dataset/features.txt"

#files to identify the subjects
fileSubjectsTrain = "./data/UCI HAR Dataset/train/subject_train.txt"
fileSubjectsTest = "./data/UCI HAR Dataset/test/subject_test.txt"

#files with observations
fileXTrain = "./data/UCI HAR Dataset/train/X_train.txt"
fileXTest  = "./data/UCI HAR Dataset/test/X_test.txt"

#files with activities (match by line)
fileYTrain = "./data/UCI HAR Dataset/train/y_train.txt"
fileYTest  = "./data/UCI HAR Dataset/test/y_test.txt"

### Loading the data
activity_ID_Desc<- tbl_df(fread(file.path(fileActivityLabels)))

df_features <- tbl_df(fread(file.path(fileFeatures)))

df_subjectsTrain <- tbl_df(fread(file.path(fileSubjectsTrain)))
df_subjectsTest <- tbl_df(fread(file.path(fileSubjectsTest)))

df_train_X <- tbl_df(fread(file.path(fileXTrain)))
df_test_X <- tbl_df(fread(file.path(fileXTest)))

df_train_y <- tbl_df(fread(file.path(fileYTrain)))
df_test_y <- tbl_df(fread(file.path(fileYTest)))

### Labeling the data

colnames(activity_ID_Desc) <- c("ActivityID", "ActivityDesc")
colnames(df_train_X) <- df_features$V2
colnames(df_test_X) <- df_features$V2

### Merging the data

df_train <- cbind(df_subjectsTrain, df_train_y)
colnames(df_train) <- c("Subject","Activity")
df_train <- cbind(df_train, df_train_X)

df_test <- cbind(df_subjectsTest, df_test_y)
colnames(df_test) <- c("Subject","Activity")
df_test <- cbind(df_test, df_test_X)

df_data <- rbind(df_train, df_test)

rm(list = ls(pattern = "df_t")) #drop all test and train sets
rm(list = ls(pattern = "file")) #drop all file references

columnsMeanStd <- grep("mean\\(\\)|std",colnames(df_data))
columnsMeanStd <- append(1:2, columnsMeanStd)

df_data <- df_data[columnsMeanStd]

df_data <- merge(df_data, activity_ID_Desc, by.x="Activity", by.y = "ActivityID", all=TRUE)
#neworder <- c(1,69,2:68)
# 'swap' ActivityID column with description, which was added as the last column
NoColumns <- length(colnames(df_data))
neworder <- c(1,NoColumns,2:(NoColumns - 1))
setcolorder(df_data, neworder)
df_data <- subset(df_data, select = -c(Activity) )

colnames(df_data) <- gsub("^t","time",colnames(df_data) )
colnames(df_data) <- gsub("^f","frequency",colnames(df_data) )
colnames(df_data) <- gsub("mean\\(\\)","Mean",colnames(df_data) )
colnames(df_data) <- gsub("std\\(\\)","StandardDeviation",colnames(df_data) )
colnames(df_data) <- gsub("Acc","Accelerometer",colnames(df_data) )
colnames(df_data) <- gsub("Gyro","Gyroscope",colnames(df_data) )
colnames(df_data) <- gsub("Mag","Magnitude",colnames(df_data) )
colnames(df_data) <- gsub("BodyBody","Body",colnames(df_data) )

print(colnames(df_data))  #verify variable names

df_data_tp <- gather(df_data, variable, value, -c(Subject, ActivityDesc) )
df_data_tp <- group_by(df_data_tp, Subject, ActivityDesc, variable)
final_result <- summarize(df_data_tp, mean = mean(value))
final_result <- spread(final_result, variable, mean)

write.table(final_result, file = "final_result.txt", sep = ",", row.name=FALSE)