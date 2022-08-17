#install.packages("SYNCSA")
install.packages("taxize")
library(vegan)
library(cluster)
library(FD)
library(SYNCSA)
library(taxize)
library(dplyr)

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
head(comm)[,1:6]
head(traits)[,1:6]
rownames(comm)[1:6]

rownames(comm) <- paste0("Site", comm[,1])
comm <- comm[,-1]
head(comm)[,1:6]

rownames(traits) <- paste0(traits[,1])
traits <- traits[,-1]
head(traits)[,1:6]


richness <- vegan::specnumber(comm)

#Taxonomic diversity
#individuals distance matrix: 0 same species, 1 different
shannon <- vegan::diversity(comm)
simpson <- vegan::diversity(comm, index = "simpson")


#When analyzing functional traits between individuals
#is no longer determined by their belonging to a species,
#but to their position in the trait space.

#We use Gower distance (metric) to deal with distances in trait things.

gow <- cluster::daisy(traits, metric="gower") #dissmilatirys
gow2 <- FD::gowdis(traits) #distances
identical(gow, gow2) #bota F porque tienen clases diferentes
class(gow2)
plot(gow, gow2, asp = 1) #same values



#------Rao’s quadratic entropy calculations in R----
#revisar teoría
tax <- rao.diversity(comm) #taxonomic
fun <- rao.diversity(comm, traits = traits) #functional
plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
abline(a = 0, b = 1)

#----FD indices
#we can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)
#the returned object has Villéger's indices and Rao calculation
names(FuncDiv1)
FuncDiv <- dbFD(x = traits, a = comm, messages = F)


#----Ahora sacamos las familias
TaxonName <- splist$TaxonName
classification_data <- classification(TaxonName, db="ncbi")
str(classification_data)
length(classification_data)

#Indexing the list --- estos dos son lo mismo
classification_data$"Arisarum vulgare"
classification_data[[1]]

#Filter the family with every element and find the family

tibble_ex <- classification_data[[1]] %>%
  filter(rank=="family") %>%
  select(name)

#ahora lo hago para todas las listas
extract_family <- function(x){

  #!is.null o sea si es F?
  if(!is.null(dim(x)))
  {
    y <- x%>%
    filter(rank=="family") %>%
    select(name)
    return(y)
  }
}
