##Getting and Cleaning Data Course Project

Manipulation of data from the Samsung Galaxy S Smartphone.

The task for the project is to create run_analysis.R that does the following. 
* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement. 
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive variable names. 
* From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


First thing that was performed in the run_analysis.R was to load all the Data Files and collect into Data.Frames

Changed the data frame for the ActivityLabels to contain the activity label instead of the ID

Changed all column names on the Subject tables, Y tables and X Tables

Created a vector that contained all the column names for the Data Tables (TestX and TrainX)
Remade the Vector with other the Mean and StdDev columns
Then removed the meanFreq as well since it was dragged along

Recreated the TestX and TrainX tables with only the columns that we are interested in

Now it is time to Column combine the TestSubject, TestY and TestX tables
Same happens with the TrainSubject, TrainY and TrainX tables
That put together the SubjectID, the Activity and all the Mean and StdDev columns of data

These were called TestAll and TrainAll

TestAll and TrainAll were combined with the Row Combine function to create a Data.Frame called AllData


At this point all Columns were appropriately labelled with descriptive variable names


At the very end a Chaining function was applied to get the Data.Frame that was grouped by the subjectId and Activity and 
the mean of all the data columns.
This was using the dplyr library.
The dplyr makes it fast and efficient with code to produce the output that was needed.


To create the Tidy table, the reshape2 was used.
Using the Melt function it was very easy to rearrange the data to create the Tidy table.


Finally, I wrote out the table in various formats that I could view.  

