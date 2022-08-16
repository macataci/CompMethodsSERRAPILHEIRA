
# loading needed packages
library(tidyverse)
library(reshape2)
#install.packages("vegan")
#install.packages("FD")
library(vegan)

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

#---CREATING FUNCTIONS FOR SHANNON AND SIMPSON DIVERSITY IN COMM DATASET
#---------------Shannon


ShannonCalc <- function (x){
  ShannonFila <- c()
  for (i in 1:nrow(x)){
    fila <- x[i,]
    totalFila <- sum(fila)
    nozero <- fila[fila!=0]
    multi <- (nozero/totalFila)*log(nozero/totalFila)
    sumita <- sum(multi)
    ShannonFila[i] <- -sumita
  }
  return(ShannonFila)
}
commNum <- comm[,2:57]
ShannonCalc (commNum)
comm$H <- ShannonCalc (commNum)
View(comm)
#Comprobación
diversity (commNum, "shannon")
#Sí dio

#---------------------------Simpson
SimpsonCalc <- function (x){
  SimpsonFila <- c()
  for (i in 1:nrow(x)){
    fila <- x[i,]
    totalFila <- sum(fila)
    prob <- fila/totalFila
    elevado <- prob^2
    sumita <- sum(elevado)
    SimpsonFila[i] <- 1-sumita}
  return(SimpsonFila)
}

comm$Simp <- SimpsonCalc (commNum)
View(comm)
#Comprobación
diversity (commNum, "simpson")
#Sí dio

#-----------------InvSimpson
InvSimpsonCalc <- function (x){
  InvSimpsonFila <- c()
  for (i in 1:nrow(x)){
    fila <- x[i,]
    totalFila <- sum(fila)
    prob <- fila/totalFila
    elevado <- prob^2
    sumita <- sum(elevado)
    InvSimpsonFila[i] <- 1/sumita}
  return(InvSimpsonFila)
}

comm$InvSimp <- InvSimpsonCalc (commNum)
View(comm)
#Comprobación
diversity (commNum, "invsimpson")
#Sí dio




### DIVERSE METRICS PART 2
Community.A <- c(10, 6, 4, 1)
Community.B <- c(17, rep(1,7))
diversity (Community.A, "shannon")
diversity (Community.B, "shannon")
diversity (Community.A, "invsimpso")
diversity (Community.B, "invsimpso")
renyi.comA <- renyi (Community.A)
renyi.comB <- renyi (Community.B)
union <- rbind(renyi.comA, renyi.comB)
matplot(t(union), type ="l", axes=F)
box()
axis(side=2, )
axis(side=1, labels=c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "Inf" ), at=(1:11))#se intersectan entonces no se sabe
legend("topright", legend = c("Community A", "Community B"), lty=c(1,2), col=c(1,2))

renyi.comBHill <- renyi (Community.B, hill=T)
