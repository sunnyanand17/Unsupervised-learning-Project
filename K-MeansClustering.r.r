#Clustering the StudentKnowledgeData dataset

#Approach1: K-means clustering

#import the data into the present environment
View(StudentKnowledgeData)
mydata  = StudentKnowledgeData

#let us analyze the data to find if categorical variables are there and if so
#transform them.
summary(mydata)
dim(mydata)
names(mydata)
head(mydata)
#all the variables are of the type numeric. There are 5 variables,402observations:
#STG: num
#SCG: num
#STR: num
#LPR: num
#PEG: num

# We can now remove any records that have NAs
myDataClean = na.omit(mydata)
dim(myDataClean)
summary(myDataClean)
#let us first analyze the data for some .Let us create some plots
table(myDataClean$STG,myDataClean$SCG)
table(myDataClean$SCG)
table(myDataClean$STR)
table(myDataClean$LPR)
table(myDataClean$PEG)

attach(myDataClean)
plot(myDataClean)
#we donot need to centre or scale the data as the data for each of the variable 
#lies in the range of (0,1).However it has large variations internally.So we will
#scale this data.
scaled_data = as.matrix(scale(myDataClean))

#table(scaled_data$STG,scaled_data$SCG)

#Let us apply kmeans for k=5 clusters 
kmm = kmeans(scaled_data,5,nstart = 50,iter.max = 15)
#Let us check the value of k=5 clusters now
kmm
kmm$centers
kmm$cluster
kmm$withinss
kmm$tot.withinss
kmm$totss
kmm$betweenss
kmm$betweenss/kmm$totss

########################################Elbow Method############################
#Elbow Method for finding the optimal number of clusters

set.seed(123)

# Compute and plot wss for k = 2 to k = 15.

k.max <- 15
data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})

wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#let us anlze which should be choosen for the number of clusters , either 3 or 4
kmm1 = kmeans(scaled_data,3)
kmm1
kmm2 = kmeans(scaled_data, 4)
kmm2
#Therefore for k=5 the between_ss/total_ss ratio tends to change slowly and remain
#less changing as compared to other k's.so for this data k=5 should be a good choice 
#for number of clusters

plot(scaled_data[,c(1,2)], col=kmm1$cluster)
points(kmm1$centers[,c(1,2)], col=1:5, pch=8, cex=2)



#####################################NbClust Analysis########################################

#NbClust package provides 30 indices for determining the number of clusters and 
#proposes to user the best clustering scheme from the different results obtained 
#by varying all combinations of number of clusters, distance measures, and 
#clustering methods

#install.packages("NbClust",dependencies = TRUE)
library(NbClust)
nb <- NbClust(scaled_data, diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=15, method = "kmeans", 
              index = "all", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))
################################################################################
###############################BIC analysis#####################################


#let us check the BIC to determine the optimal number of clusters
install.packages("mclust",dependencies = TRUE)
library(mclust)
# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.

d_clust <- Mclust(as.matrix(scaled_data), G=1:15, 
                  modelNames = mclust.options("emModelNames"))
d_clust$BIC
d_clust$z
d_clust$classification
d_clust$modelName
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
plot(d_clust)


################################################################################
#############################Calinski criterion#################################


install.packages("fpc",dependencies = TRUE)
library(fpc)
kmmnew = kmeansruns(mydata,krange=2:6,criterion="ch",
           iter.max=50,runs=50,
           scaledata=TRUE)
kmmnew$bestk
################################################################################
