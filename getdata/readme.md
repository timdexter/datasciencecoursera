# Course Project

Create one R script called run_analysis.R that does the following.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Dependencies

 run_analysis.R depends on  data.table and reshape2

# Steps to work on this course project

1. Download the data files and place them into a folder called 'data'. You'll have a ```UCI HAR Dataset``` folder.
2. Place ```run_analysis.R``` in the parent folder of the data folder and set it as your working directory.
3. Run ```source("run_analysis.R")```, then it will generate a new file ```tiny_data.txt``` in your working directory.
