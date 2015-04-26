################################################################################
# run_analysis.R:
#
# Takes data collected from accelerometers of the Samsung Galaxy S smartphone
# and creates two tidy data sets:
#
# * One raw data set containing all mean and standard deviation measurements
# * One data set containing means of all other values, grouped by subject ID.
#
# The description of the original data can be found at: http://bit.ly/1mEvWTG
#
# The original data can be found at: http://bit.ly/1p1sdWC
#
################################################################################

################################################################################
# tidy: A function to create tidy column names. This function iterates over the
# patterns in the included 'patterns.psv' file to create column names that are:
#
#        * be all lower case
#        * be descriptive
#        * not repeated
#        * not contain non-alphabetic characters or numbers
#
# The 'patterns.csv' file is a pipe ('|') -delimited file containing search-and
# -replace strings such as:
#
# Pattern|Replacement
# ^t|time
# ^f|frequency
# mad\(\)|medianabsolutedeviation
# max\(\)|maximum
# min\(\)|minimum
# sma\(\)|signalmagnitudearea
#
tidy <- function(cnames)
{
    # Read the 'patterns.psv' file.
    patterns <- read.table("patterns.psv", sep="|", header=TRUE)
    
    # Iterate over all of the patterns to clean up the column names.
    for (i in seq(1, nrow(patterns)))
    {
        cnames <- sub(patterns[i, "Pattern"], patterns[i, "Replacement"],
                      cnames)
    }
    
    # Return the data column name data frame.
    cnames
}

################################################################################
# Combine the 'test' and 'train' datasets:

# Read the 'test' data into data frames.
yTest <- read.csv("UCI HAR Dataset/test/y_test.txt", header = FALSE)
subjectTest <- read.csv("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
xTest <- read.table("UCI HAR Dataset/test/x_test.txt", header = FALSE)

# Create a data frame combining all three 'test' data frames.
test <- cbind(xTest, yTest, subjectTest)

# Read the 'train data into data frames.
yTrain <- read.csv("UCI HAR Dataset/train/y_train.txt", header=FALSE)
subjectTrain <- read.csv("UCI HAR Dataset/train/subject_train.txt",
                         header=FALSE)
xTrain <- read.table("UCI HAR Dataset/train/x_train.txt", header = FALSE)

# Create a data frame combining all three 'train' data frames.
train <- cbind(xTrain, yTrain, subjectTrain)

# Combine the 'test' data frame with the 'train' data frame.
composite <- rbind(test, train)

# Read the column names into a data frame from the 'features.txt' file.
columnNames <- read.table("UCI HAR Dataset/features.txt", header = FALSE)

# Create a character vector to contain all of the column names, including
# 'activityid' and 'subjectid'.
allcolumns <- character(length = nrow(columnNames) + 2)

# Add all the the column names from 'features.txt' to the vector
for (i in seq(1, nrow(columnNames)))
{
    allcolumns[i] <- as.character(columnNames[i, 2])
}

# Add 'activityid' and 'subjectid' to the column name vector.
allcolumns[nrow(columnNames) + 1] <- "activityid"
allcolumns[nrow(columnNames) + 2] <- "subjectid"
colnames(composite) <- allcolumns

################################################################################
# Create two data sets of the data: The first contains all mean and standard
# deviation data points, the other everything else. The first data set will be
# saved as raw data. The second will be to calculate the mean of all data
# points by subject id.

# Create a boolean vector to identify all mean and standard deviation columns.
columnstokeep <-grepl("-(mean|std)|^(activityid|subjectid)$",
                      colnames(composite), perl=TRUE)

# Create a data frame for the raw data.
final <- composite[columnstokeep]

# Create a data frame for the data to calculate averages. Simply use the inverse
# of the boolean data set.
toaverage <- composite[! columnstokeep]

# The second data set will also need the subjectid column.
toaverage$subjectid <- composite$subjectid

################################################################################
# Look up the data labels contained in the 'activity_labels.txt' file
# corresponding to the values in the 'activityid' column. Create a new column
# in the data frame containing the labels.

# Read the activity lables from the 'activity_labels.txt' file.
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt",
                             header = FALSE)

# Create a character vector for the 'activitylabel' column that will be added to
# the data frame.
alabels <- character(length = nrow(final))

# Fill the 'activitylabel' character vector.
for (i in seq(1, nrow(final)))
{
    alabels[i] <- as.character(activityLabels[final[i, "activityid"], "V2"])
}

# Add the 'activitylabel' character vector to the data frame.
final$activitylabel <- alabels

# Use the 'colnames' function to create tidy data frame column names. Replace
# the original column names with the new names.
colnames(final) <- tidy(colnames(final))

# Save the data frame
write.table(final, file = "final.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

################################################################################
# Create the second data set by calculating the means of the columns, grouped by
# subject id.

# Clean up the data set column names.
colnames(toaverage) <- tidy(colnames(toaverage))

# Create the average of each data point, grouped by subject id.
averages <- aggregate(toaverage, list(toaverage$subjectid), mean)

# Write the second data set to CSV file.
write.table(averages, file = "averages.csv", append = FALSE, quote = TRUE,
            sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))
