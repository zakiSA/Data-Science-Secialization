library(dplyr)

##Creating Data Frame with Train and Test data
##(1)Import train, test data files using read.table()
        my_X_train_data <-read.table("./train/X_train.txt")
        my_X_test_data <-read.table("./test/X_test.txt")
        my_y_train_data <- read.table("./train/y_train.txt")
        my_y_test_data <- read.table("./test/y_test.txt")
        my_subject_train_data <- read.table("./train/subject_train.txt")
        my_subject_test_data <- read.table("./test/subject_test.txt")

##Import features.txt file using read.table()
        my_features_data <- as.character(read.table("features.txt")[,2])
        my_features_data <- c("subject_num", "activity_label", my_features_data)

##Bind columns to train and test data frames
        my_train_data <- cbind(my_subject_train_data, my_y_train_data, my_X_train_data) 
        my_test_data <- cbind(my_subject_test_data, my_y_test_data, my_X_test_data)

##Merge train and test data frames using rbind()
        my_data <- rbind(my_train_data, my_test_data)
##Use features_data as column names to comnined data frame
        colnames(my_data) <- my_features_data
        
##(2)Extract columns with mean and standard deviation measurements 
##Find columns that have mean and standard deviation measurements
        mean_col_num <- grep("-mean",names(my_data))
        std_col_num <- grep("-std",names(my_data))
        mean_col_val <-grep("-mean",names(my_data),value = TRUE)
        std_col_val <- grep("-std",names(my_data),value = TRUE)
        
##(3) & (4)Extract columns from combined data frame(my_data)
        MSdata <- my_data[,c("subject_num","activity_label",mean_col_val,std_col_val)]
##Replace activity label numbers with activity names                
        activity_map <- read.table("./activity_labels.txt")
        MSdata[MSdata["activity_label"] == 1,2] <- as.character(activity_map[1,2])
        MSdata[MSdata["activity_label"] == 2,2] <- as.character(activity_map[2,2])
        MSdata[MSdata["activity_label"] == 3,2] <- as.character(activity_map[3,2])
        MSdata[MSdata["activity_label"] == 4,2] <- as.character(activity_map[4,2])
        MSdata[MSdata["activity_label"] == 5,2] <- as.character(activity_map[5,2])
        MSdata[MSdata["activity_label"] == 6,2] <- as.character(activity_map[6,2])

## (5) Create an independent, tidy data set with data from step (4)
## with average of each variable for each activity for each subject
        MS_avg_data_tbl <- MSdata %>% group_by(subject_num,activity_label) %>% 
                summarize_all(funs(mean))
        MS_avg_df <- as.data.frame(MS_avg_data_tbl)
        ##print(head(MS_avg_df))
        write.table(MS_avg_df,file = "output.txt",row.names = FALSE)
        

        
        
