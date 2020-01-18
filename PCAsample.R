#PCA 
#Pieta House - 1317
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Merged")
suicide1317 <- read.csv("Suicide1317.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

library(ggfortify)
df <- suicide1317
df <- df[, -2]#remove categorical 
names(df)

#scree plot to find optimal clusters
df.pca <- prcomp(df[, c(1:18)], center = TRUE, scale. = TRUE)
screeplot(df.pca, type = "lines")
#print matrix of clusters found
summary(df.pca)

#plot clusters
biplot(df.pca)

##################################
#pca - suicide8013 with factors
setwd("C:/Users/Alexander/OneDrive - National College of Ireland/4th Year/4thYR Project/Datasets/Merged")
suicide8013 <- read.csv("FINALsuicide8013cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

library(ggfortify)
df <- suicide8013
remove.factors(df)
df <- df[,-which(sapply(df, class) == "factor")]
names(df)

df.pca <- prcomp(df[, c(1:108)], center = TRUE, scale. = TRUE)

#print matrix of clusters found
summary(df.pca)

#scree plot to find optimal clusters
screeplot(df.pca, type = "lines")

#rotation matrix - shows variables contributing the most
df.pca$rotation

#plot clusters
biplot(df.pca)

autoplot(prcomp(df), data = suicide8013, colour = 'Date')
autoplot(prcomp(df), data = suicide8013, colour = 'Location')
autoplot(prcomp(df), data = suicide8013, colour = 'Location', 
         loadings = TRUE, loadings.colour = "blue",
         loadings.label = TRUE, loadings.label.size = 3)

#######################
#pca - suicide8017
suicide0817 <- read.csv("FINALsuicide0817cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

library(ggfortify)
df <- suicide0817

df <- df[,-which(sapply(df, class) == "factor")]
names(df)
df.pca <- prcomp(df[, c(1:107)], center = TRUE, scale. = TRUE)

#print matrix of clusters found
summary(df.pca)

#scree plot to find optimal clusters
screeplot(df.pca, type = "lines")

#rotation matrix - shows variables contributing the most
df.pca$rotation

#plot clusters
biplot(df.pca)

autoplot(prcomp(df), data = suicide0817, colour = 'Date')
autoplot(prcomp(df), data = suicide0817, colour = 'Location')
autoplot(prcomp(df), data = suicide0817, colour = 'Location', 
         loadings = TRUE, loadings.colour = "blue",
         loadings.label = TRUE, loadings.label.size = 3)

########
#PCA 50 - 17
suicide5017 <- read.csv("FINALsuicide5017cuswih.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)

library(ggfortify)
df <- suicide5017

df <- df[,-which(sapply(df, class) == "factor")]
names(df)
df.pca <- prcomp(df[, c(1:110)], center = TRUE, scale. = TRUE)

#print matrix of clusters found
summary(df.pca)

#scree plot to find optimal clusters
screeplot(df.pca, type = "lines")

#rotation matrix - shows variables contributing the most
df.pca$rotation

#plot clusters
biplot(df.pca)

autoplot(prcomp(df), data = suicide5017, colour = 'Date')
autoplot(prcomp(df), data = suicide5017, colour = 'Location')
autoplot(prcomp(df), data = suicide0817, colour = 'Location', 
         loadings = TRUE, loadings.colour = "blue",
         loadings.label = TRUE, loadings.label.size = 3)
