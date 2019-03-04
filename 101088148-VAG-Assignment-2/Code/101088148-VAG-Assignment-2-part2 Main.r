windows(record=TRUE)

# Installing required packages
#install.packages("rggobi")

# Calling necessary libraries
#library(ggplot2)
#library(rggobi)

#---------------------------------------#
# Setting up the directory and database #
#---------------------------------------#

Student.Number <- "101088148"
Initials <- "VAG"
ASorLAB <- "Assignment"
Assignment.Number <- "2"
Student.info <- paste(Student.Number, Initials, ASorLAB, Assignment.Number, sep="-")

# "drive", AND "path.up" SHOULD BE THE ONLY PARTS THAT REQUIRE YOUR PROFESSOR 
# OR TA TO BE ABLE TO RUN YOUR CODE

drive="S:"
path.upto <- paste("Grad 3 term", "Data Mining", sep="/" )
code.dir <- paste(drive, path.upto, Student.info, "Code", sep="/")
data.dir <- paste(drive, path.upto, Student.info, "Data", sep="/")
work.dir <- paste(drive, path.upto, Student.info, "Work", sep="/")
setwd(work.dir)


# Reding the data
vote <- paste(data.dir,"votes.csv", sep="/")
votes <- read.csv(vote)
head(votes)
fact <- paste(data.dir,"county_facts.csv", sep="/")
facts <- read.csv(fact)
head(facts)

library(dplyr)
# Cleaning of data
facts[1:10,1:10]
facts <-facts[facts[,3]!="",]
dim(facts)
facts[1:2,]
votes_facts<-inner_join(facts[,1:3],votes, by="fips")
dim(votes_facts)
votes_facts = lapply(votes_facts, as.numeric)
write.csv(votes_facts,file="votes.new.csv")
votesfactss<- paste(work.dir,"votes.new.csv", sep="/")
votes_factss <- read.csv(votesfactss)
head(votes_factss)
summary(votes_factss)

votes.new<-votes_factss[ ,c(2:3, 11:12, 15, 23:24, 80:84)]
head(votes.new)
dim(votes.new)
winning<-ifelse(votes.new$Trump > votes.new$Romney,1,ifelse(votes.new$Trump < votes.new$Romney,2,NA))
summary(winning)
head(winning)
str(winning)
votes.new$winning <- winning
head(votes.new)

#Davies Bouldin

Davies.Bouldin <- function(A, SS, m) {
  # A - the centres of the clusters
  # SS - the within sum of squares
  # m - the sizes of the clusters
  N <- nrow(A) # number of clusters
  # intercluster distance
  S <- sqrt(SS/m)
  # Get the distances between centres
  M <- as.matrix(dist(A))
  # Get the ratio of intercluster/centre.dist
  R <- matrix(0, N, N)
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      R[i,j] <- (S[i] + S[j])/M[i,j]
      R[j,i] <- R[i,j]
    }
  }
  return(mean(apply(R, 1, max)))
}

#K-Means Clustering

kmcluster <- function(data.new,data.select)
{
  min.err <- 100
  errs <- rep(0, 10)
  DBI <- rep(0, 10)
  size.cluster <- rep(0,10)
  max.cluster <- rep(0,12)
  library(cluster)
  library(fpc)
  oldpar <- par(mfrow = c(4,4))
  par(mar=c(2,1,2,1))
  for (i in 2:12) {
    set.seed(654321)
    k_m <- kmeans(data.new, i, 12)
    plotcluster(data.new,col=data.select$state_abbr, k_m$cluster, main=paste(i,"cluster"))    
    errs[i-1] <- sum(k_m$withinss)
    DBI[i-1] <- Davies.Bouldin(k_m$centers, k_m$withinss, k_m$size)
    size <- "\n\nSize of all cluster"
    an <- "\n\n---------Analysis of cluster having K="
    end <- "---------"
    cat(paste(an,i,end))
    cat(size)
    cat("\nVotes:\n")
    con <- table(data.select$winning, k_m$cluster)
    print(con)
    misclassify<-0
    for (n in 1:i)
    {
      misclassify=misclassify+k_m$size[n]-max(con[,n])
    }  
    mis<-("\nTotal misclassifications=")
    cat(paste(mis,misclassify))
    if(misclassify<min.err)
    {
      min.err<-misclassify
      min.err.cluster<-i
    }
    ss <- "\nSSE ="
    db <- "\nDBI ="
    cat(paste(ss,errs[i-1]))
    cat(paste(db,DBI[i-1]))
  }
  plot(2:12, errs, main="SSE")
  lines(2:12, errs)
  plot(2:12, DBI, main="Davies-Bouldin")
  lines(2:12, DBI)
  par(oldpar)
  cat(paste("\n\n\nLeast DBI=",min(DBI)))
}

kmclusterPlot <- kmcluster(votes.new,votes.new)

#best cluster is 5

#best seed

min.err <- function(data.new,data.select,ke)
{
  min.err <- 550
  for(j in 1:4000)
  {
    size.cluster <- rep(0,10)
    max.cluster <- rep(0,12)
    library(cluster)
    library(fpc)
    set.seed(j)
    k_m <- kmeans(data.new, ke, 12)
    con <- table(data.select$winning, k_m$cluster)
    misclassify<-0
    for (n in 1:ke)
    {
      misclassify = misclassify+k_m$size[n]-max(con[,n])
    }  
    if(misclassify < min.err)
    {
      min.err <- misclassify
      min.err.seed<-j
    }
  } 
  cat(paste("\nMinimum misplacements =",min.err))
  cat(paste("\nSeed of minimum misplacements =",min.err.seed))
}  

kmclusterPlot2 <- min.err(votes.new,votes.new,5)

#Seed of minimum misplacements is 1

set.seed(1)
Centre <- kmeans(votes.new, 5, 12)
print(Centre$centers)
bestresult<-function(data.new,data.select,k,seed)
{  
  set.seed(seed)
  k_m <- kmeans(data.new, k, 12)
  plotcluster(data.new,col=data.select$state_abbr, k_m$cluster, main=paste(k,"clusters")) 
  print(paste("best Cluster= ",k," Analysis= "))
  incorrect <- rep(0,k)
  con <- table(data.select$winning, k_m$cluster)
  print(con)
  cat("\n\n")
}

kmclusterPlot3 <- bestresult(votes.new,votes.new,5,1)



#Euclidean Clustering

mancluster <- function(data.new,data.select)
{
  min.err<-100
  errs <- rep(0, 10)
  avg.width <- rep(0, 10)
  size.cluster <- rep(0,10)
  max.cluster <- rep(0,12)
  library(cluster)
  library(fpc)
  oldpar <- par(mfrow = c(4,4))
  par(mar=c(2,1,2,1))
  for (i in 2:12) {
    set.seed(654321)
    library(fpc)
    man <- pam(data.new, i, metric = "euclidean")
    plotcluster(data.new,col=data.select$state_abbr, man$cluster, main=paste(i,"clusters"))
    avg.width[i-1] <- man$silinfo$avg.width
    size <- "\n\nSize of all clusters"
    analysis <- "\n\n---------Analysis of Clusters whose K="
    en <-"---------"
    cat(paste(analysis,i,en))
    cat(size)
    cat("\nVotes:\n")
    con <- table(data.select$winning, man$cluster)
    print(con)
    misclassify<-0
    for (n in 1:i)
    {
      misclassify=misclassify+ man$clusinfo[n,1]-max(con[,n])
    }  
    mis<-("\nTotal misclassifications=")
    cat(paste(mis,misclassify))
    if(misclassify<min.err)
    {
      min.err<-misclassify
      min.err.cluster<-i
    }
    
    db <- "\nAverage silhoutte width="
    cat(paste(db,avg.width[i-1]))
  }
  plot(2:12, avg.width, main="Silhouette Width")
  lines(2:12, avg.width)
  par(oldpar)
  cat(paste("\n\n\nHighest Average Silhoutte Width = ",max(avg.width)))
}
manPlot <- mancluster(votes.new,votes.new)

#best cluster is 6

#best seed
min.err <- function(data.new,data.select,ke)
{
  min.err<-550
  for(j in 1:10)
  {   
    size.cluster <- rep(0,10)
    max.cluster <- rep(0,12)
    library(cluster)
    library(fpc)
    set.seed(j)
    man <- pam(data.new, ke, metric = "manhattan")
    con <- table(data.select$winning, man$clustering)
    misclassify<-0
    for (n in 1:ke)
    {
      misclassify=misclassify+man$clusinfo[n,1]-max(con[,n])
    }  
    if(misclassify<min.err)
    {
      min.err<-misclassify
      min.err.seed<-j
    }
  } 
  cat(paste("\nMinimum misplacements = ",min.err))
  cat(paste("\nSeed of Minimum misplacements= ",min.err.seed))
}  
manPlot2 <- min.err(votes.new,votes.new,2)

#best result

bestresults<-function(data.new,data.select,k,seed)
{  
  set.seed(seed)
  man <- pam(data.new, k, metric = "euclidean")
  
  plotcluster(data.new,col=data.select$state_abbr, man$cluster, main=paste(k,"clusters"))
  print(paste("best Cluster=",k," Analysis="))
  incorrect <- rep(0,k)
  con <- table(data.select$winning, man$clustering)
  print(con)
  cat("\n\n")
}
bestresults1 <-bestresults(votes.new,votes.new,6,1)

