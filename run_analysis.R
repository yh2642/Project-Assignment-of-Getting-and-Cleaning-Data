old.dir <- getwd()

setwd("./UCI HAR Dataset")
feature_label <- read.table("features.txt")
activity_label <- read.table("activity_labels.txt")
data_test <- read.table("./test/X_test.txt")
activity_test <- read.table("./test/y_test.txt")
names(activity_test) <- "activity" 
subject_test <- read.table("./test/subject_test.txt")
names(subject_test) <- "subject"
body_acc_x_test <- read.table("./test/Inertial Signals/body_acc_x_test.txt")
body_acc_y_test <- read.table("./test/Inertial Signals/body_acc_y_test.txt")
body_acc_z_test <- read.table("./test/Inertial Signals/body_acc_z_test.txt")
body_gyro_x_test <- read.table("./test/Inertial Signals/body_gyro_x_test.txt")
body_gyro_y_test <- read.table("./test/Inertial Signals/body_gyro_y_test.txt")
body_gyro_z_test <- read.table("./test/Inertial Signals/body_gyro_z_test.txt")
total_acc_x_test <- read.table("./test/Inertial Signals/total_acc_x_test.txt")
total_acc_y_test <- read.table("./test/Inertial Signals/total_acc_y_test.txt")
total_acc_z_test <- read.table("./test/Inertial Signals/total_acc_z_test.txt")

data_train <- read.table("./train/X_train.txt")
activity_train <- read.table("./train/y_train.txt")
names(activity_train) <- "activity" 
subject_train <- read.table("./train/subject_train.txt")
names(subject_train) <- "subject"
body_acc_x_train <- read.table("./train/Inertial Signals/body_acc_x_train.txt")
body_acc_y_train <- read.table("./train/Inertial Signals/body_acc_y_train.txt")
body_acc_z_train <- read.table("./train/Inertial Signals/body_acc_z_train.txt")
body_gyro_x_train <- read.table("./train/Inertial Signals/body_gyro_x_train.txt")
body_gyro_y_train <- read.table("./train/Inertial Signals/body_gyro_y_train.txt")
body_gyro_z_train <- read.table("./train/Inertial Signals/body_gyro_z_train.txt")
total_acc_x_train <- read.table("./train/Inertial Signals/total_acc_x_train.txt")
total_acc_y_train <- read.table("./train/Inertial Signals/total_acc_y_train.txt")
total_acc_z_train <- read.table("./train/Inertial Signals/total_acc_z_train.txt")

merge_data <- rbind(data_test, data_train)                     # merge by row of three table

merge_activity <- rbind(activity_test, activity_train)

merge_subject <- rbind(subject_test, subject_train)

## label the variables
var.labels <- feature_label[,"V2"]
var.labels <- as.character(var.labels)     # transfer the labels from factor to character 
names(var.labels) <- names(merge_data)         # rename the var.labels according x_test
library(Hmisc)
label(merge_data) <- lapply(names(var.labels), function(x) label(merge_data[,x]) <- var.labels[x])


merge_data_all <- cbind(merge_activity, merge_subject, merge_data)

merge_data_all$body_acc_x_mean <- apply(rbind(body_acc_x_test, body_acc_x_train), 1, mean)
merge_data_all$body_acc_x_sd <- apply(rbind(body_acc_x_test, body_acc_x_train), 1, sd)
merge_data_all$body_acc_y_mean <- apply(rbind(body_acc_y_test, body_acc_y_train), 1, mean)
merge_data_all$body_acc_y_sd <- apply(rbind(body_acc_y_test, body_acc_y_train), 1, sd)
merge_data_all$body_acc_z_mean <- apply(rbind(body_acc_z_test, body_acc_z_train), 1, mean)
merge_data_all$body_acc_z_sd <- apply(rbind(body_acc_z_test, body_acc_z_train), 1, sd)
merge_data_all$body_gyro_x_mean <- apply(rbind(body_gyro_x_test, body_gyro_x_train), 1, mean)
merge_data_all$body_gyro_x_sd <- apply(rbind(body_gyro_x_test, body_gyro_x_train), 1, sd)
merge_data_all$body_gyro_y_mean <- apply(rbind(body_gyro_y_test, body_gyro_y_train), 1, mean)
merge_data_all$body_gyro_y_sd <- apply(rbind(body_gyro_y_test, body_gyro_y_train), 1, sd)
merge_data_all$body_gyro_z_mean <- apply(rbind(body_gyro_z_test, body_gyro_z_train), 1, mean)
merge_data_all$body_gyro_z_sd <- apply(rbind(body_gyro_z_test, body_gyro_z_train), 1, sd)
merge_data_all$total_acc_x_mean <- apply(rbind(total_acc_x_test, total_acc_x_train), 1, mean)
merge_data_all$total_acc_x_sd <- apply(rbind(total_acc_x_test, total_acc_x_train), 1, sd)
merge_data_all$total_acc_y_mean <- apply(rbind(total_acc_y_test, total_acc_y_train), 1, mean)
merge_data_all$total_acc_y_sd <- apply(rbind(total_acc_y_test, total_acc_y_train), 1, sd)
merge_data_all$total_acc_z_mean <- apply(rbind(total_acc_z_test, total_acc_z_train), 1, mean)
merge_data_all$total_acc_z_sd <- apply(rbind(total_acc_z_test, total_acc_z_train), 1, sd)

merge_data_all$subject  <- factor(merge_data_all$subject)
merge_data_all$activity <- factor(merge_activity$activity)
levels(merge_data_all$activity) <- activity_label$V2

spIns <- split(merge_data_all[,3:581], merge_data_all$activity)
Activity_mean <- sapply(spIns, colMeans)

spIns_subject <- split(merge_data_all[,3:581], merge_data_all$subject)
Subject_mean <- sapply(spIns_subject, colMeans)

output_tidy_data <- merge(Activity_mean, Subject_mean, by = "row.names", sort = FALSE)
colnames(output_tidy_data)[1] <- "Variables"

setwd(old.dir)

write.table(output_tidy_data, file = "./output_tidy_data.txt", row.names = FALSE)
