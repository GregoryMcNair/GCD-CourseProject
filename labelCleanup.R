patterns <- read.table("patterns.tdf", sep="|", header=TRUE)
print(patterns)

cleanup <- function(cv)
{
    patterns <- c("fBody", "^t", "^f", "mean\\(\\)", "std\\(\\)", "Acc-", "-X$", "-Y$", "-Z")
    replacements <- c("", "time", "frequency", "mean", "standarddeviation", "acceleration", "-xaxis", "-yaxis", "-zaxis")
    
    for (j in seq(1, length(patterns)))
    {
        print(paste("Replacing ", patterns[j], " with ", replacements[j]))
        cv <- sub(patterns[j], replacements[j], cv)
    }

    print(cv)
    cv
}

columnNames <- read.table("UCI HAR Dataset/features.txt", header = FALSE)
cc <- character(length = nrow(columnNames))
for (i in seq(1, nrow(columnNames)))
{
    cc[i] <- as.character(columnNames[i, 2])
}

dd <- cleanup(cc)
print(dd)
