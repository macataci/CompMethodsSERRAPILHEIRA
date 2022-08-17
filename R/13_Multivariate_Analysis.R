#Response variable - matrix with objects
#Predictors/variables species or other variables
#distances/dissimilarity measures

#objects(sites?) presence/absence of species
#euclidean distance does not work with abundance. Standardize?

#PCA

#---CLUSTERING
#Kmeans, hierarchical clustering
#install.packages("palmerpenguins")
library(palmerpenguins)
data(penguins)
dim(penguins)
Penguins <- na.omit(penguins)
hclust(d=Penguins[,-1])

#######----AHORA SI TUTORIAL---

library(vegan)
library(cluster)
library (dplyr)
data(dune)

#tiene 30 species, en 20 sitios, tiene las abundances

data(dune.env)
table(dune.env$Management)

bray_distance <- vegdist(dune)
# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))

b_cluster <- hclust(bray_distance, method = "average")
c_cluster <- hclust(chord_distance, method = "average")


par(mfrow = c(1, 2))
plot(b_cluster)
plot(c_cluster)

par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)

par(mfrow = c(1, 1))



#--------------------ORDINATION METHODS-------
#Def: organize data along the axes representing most of the var

#Unconstrained -> extract gradients from main matrix
#Constrained -> second matrix is used

#---PCA, prcomp() y para ecology vegan::rda()

#bajarle dimensionalidad mantendiendo variation. Numeric data should be standarized


is(chord_distance)
norm <- decostand(dune, "norm")
pca <- rda(norm)
plot(pca, choices=c(2,3))
summary(pca)

#PCA with environmental matrix
names(dune.env)
new <- dune.env[, c("A1", "Moisture", "Manure")]
colnames(new) <- c("Aluminio", "Moisture", "Manure")
new$Aluminio <- as.numeric(new$Aluminio)
new$Moisture <- as.numeric(new$Moisture)
new$Manure <- as.numeric(new$Manure)

pca_enc <- rda(new)
pairs(new)
plot(pca_enc)
