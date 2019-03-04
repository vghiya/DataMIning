windows(record=TRUE)

# Installing required packages
#install.packages("arules")
#install.packages("rggobi")

# Calling necessary libraries
#library(arules)
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


# Reading data into nutrition variable from the data folder from the directory
nutrition <- paste(data.dir,"nndb_flat.csv", sep="/")
nutrition <- read.csv(nutrition)


#REmoving the null values from the databse
nutrition <- na.omit(nutrition)


#reducing data by just considering the nutrients and removing every other thing
nutrition1 <-nutrition[,c(8:40)]
summary(nutrition1)


#creating subset of original data by checking where Protein nutrient is greater than 50
carbohydrates_subset<-subset(nutrition, Carb_g>50)
carbohydrates_subset$Descrip
carbohydrates_subset$Energy_kcal


#plotting the graph for protein and fat and visualizing accordingly
plot(nutrition$Carb_g, nutrition$Energy_kcal, xlab="carbohydrates", ylab="energy", main="carbohydrates vs energy", col="Red")

#plotting histogram for Vitc_mg to check the frequency of occurences 
hist(nutrition$Calcium_mg, xlab = "Calcium", main = "Histogram of Calcium levels", xlim = c(0,50), breaks = 2000)


#performing PCA on the data
nutrition_pca <- prcomp(nutrition1)
head(nutrition_pca)
summary(nutrition_pca)

