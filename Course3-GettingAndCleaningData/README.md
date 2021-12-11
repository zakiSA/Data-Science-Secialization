
Inorder to create a tidy data set for later analysis the following steps were performed

The training and test data files were imported into RStudio using read.table()
	1)X_train.txt
	2)X_test.txt
	3)y_train.txt
	4)y_test.txt
	5)subject_train.txt
	6)subject_test.txt

Create Train and Test data frames using cbind() to bind activity_label (as column 2) from y_train.txt, y_test.txt to train and text data frames respectively and subject_num (as column 1)derived from subject_train.txt and sub_test.txt to train and test data frames respectively

Create Single data frame by merging training and test data frames using rbind()

Used features.txt data to add column names to merged data frame

Created new data set from columns with measurements for mean and standard deviation from merged data frame using grep() 

Substituted activity names for activity labels for above data set derived from activity_labels.txt
Created and independent data frame with data from above with average for each variable, for each activity(LAYING,SITTING,STANDING,WALKING,WALKING_DOWNSTAIRS,WALKING_UPSTAIRS for each subject(1-30)

Full Description of original data set available at 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
All files from original data set available a at
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
