
# loading needed packages
library(tidyverse)
library(reshape2)


# Reading the data in R ----

# The files are all in .csv format, separated by commas. So let's use the `read.csv()` function. We could also use `read.table()` with the arguments `sep = ","` and `header = TRUE`
# So let's read the five sets of data. A very useful function for reading data is the `list.files()` from the **base** package. This function lists files in a directory, based on a pattern.

# We will apply the function to list all files in the directory `data` with the extension `.csv`.
files_path <- list.files(path = "data/raw/cestes",
                         pattern = ".csv",
                         full.names = TRUE)

files_path

# The `files_path` object is a vector of five elements (after all, there are five files) containing the full name of the file. Let's use the contents of this vector in the `read.csv()` function. We will use the a loop to read all data at once.
file_names <- gsub(".csv", "", basename(files_path), fixed = TRUE)
for (i in 1:length(files_path)) {
  data <- read.csv(files_path[[i]])
  assign(file_names[i], data)
}


# Let's apply the `head()`, `dim()` and `summary()` functions to inspect all files. Try to understand based on the output and the help page (e.g.: `?head`) what each of the functions returns.

# Understanding the object `comm`
head(comm)
dim(comm)
summary(comm)
##------JULY 28

#Which are the 5 most abundant species overall in the dataset?
sum1 <- mapply(sum,comm[,-1])
sort(sum1, decreasing=T)

#How many species are there in each site? (Richness) SPECIES PER SITE

#----------ABUNDANCE
sumA<-apply(comm[,-1], 1, sum)

#---RICHNESS
comm2 <- comm
comm2[comm2 > 0] <- 1
sum2<-apply(comm2[,-1], 1, sum)

sp <- c()
#Which of the species that is most abundant in each site
for (i in 1:nrow(comm[,-1])){
  #sp[i] <- which.max(comm[i,])
  sp[i] <- names(which.max(comm[i,-1]) )
}

comm$AbundantSp <- sp





