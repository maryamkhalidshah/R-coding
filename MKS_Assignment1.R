###Load required package ggplot2
library(ggplot2)

###Read in the data into a dataframe and view the head of the data
#----------------------------------
TitanicData <- "TitanicDataset.csv"
TitanicDF <- read.csv(TitanicData,header = T, 
                      na.strings = c(""))
head(TitanicDF,n=10)
#----------------------------------

###Print out the structure of the data
#----------------------------------
str(TitanicDF)
#----------------------------------

###Change the type of variables and print str
#----------------------------------
TitanicDF$Survived <- as.factor(TitanicDF$Survived)
TitanicDF$Sex <- as.factor(TitanicDF$Sex)
TitanicDF$Pclass <- as.factor(TitanicDF$Pclass)
TitanicDF$Embarked <- as.factor(TitanicDF$Embarked)

str(TitanicDF)
#----------------------------------

###Clean the data and view the head of the data parts(a-e)
#----------------------------------
TitanicDF$PassengerId <- NULL #passenger ID does not provide any additional
                              #information - either biographical or about the
                              #people as passengers of the Titanic.
TitanicDF$Name <- NULL   #Names are not useful to analyze this data
TitanicDF$Cabin <- NULL  #Individual customers'cabin numbers not needed for an
                         #overall analysis of the data
TitanicDF$Ticket <- NULL
TitanicDF$Fare <- round(TitanicDF$Fare,2)                       

head(TitanicDF, n=10)
#----------------------------------

###Clean the data
#----------------------------------
cat("The Titanic Dataset has a total of", nrow(TitanicDF), "rows.")   #to check rows
cat("The Titanic Dataset has a total of", ncol(TitanicDF), "columns.")#to check columns

(nrow(TitanicDF[complete.cases(TitanicDF),])) #to check number of complete rows

TitanicDF <- na.omit(TitanicDF) #to remove rows with NA values

##To check if NA values removed
Before <- read.csv("TitanicDataset.csv",header=TRUE, na.string=c("")) #Original dataset
head(Before, n=20) #printing original dataset
head(TitanicDF, n=20) #printing updated dataset (After)

isTRUE(nrow(TitanicDF[complete.cases(TitanicDF),])==nrow(TitanicDF)) #TRUE would mean NA values removed

##Aggregate SibSp and Parch
TitanicDF$FamilyNum <- TitanicDF$SibSp + TitanicDF$Parch
TitanicDF$SibSp <- NULL #remove Sibsp
TitanicDF$Parch <- NULL #remove Parch
#----------------------------------

###Exploratory Data Analysis - Part 1
#----------------------------------
names(TitanicDF) #getting names of variables

##Column names
ColNames <- (names(TitanicDF))

##part(a)
##Creating a function DF_Tables that will print tables of each
##variable and variable names, leaving a line between each table. 
DF_Tables <- function(x){
  for (x in 1:ncol(TitanicDF)) {
    print(names(TitanicDF[x]))
    print(table(TitanicDF[ColNames[x]]))
    cat("\n")
  }
}

DF_Tables() #to check function $ view tables

##part (b)
##Creating a function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
##Creating a function to calculate Range
Range <- function(x){
  max(TitanicDF[,x])-min(TitanicDF[,x])
}
##Creating Function CT that will calculate mean,median,mode,var & range if var
##is numeric, and mode if not numeric.
CT <- function(x){
  if(is.numeric(TitanicDF[,x])==TRUE){
    mean <- mean(TitanicDF[,x])
    median <- median(TitanicDF[,x])
    mode <- getmode(TitanicDF[,x])
    variance <- var(TitanicDF[,x])
    range <- Range(x)
    print(paste("The mean of", colnames(TitanicDF[x]), "is", mean))
    print(paste("The median of", colnames(TitanicDF[x]), "is", median))
    print(paste("The mode of", colnames(TitanicDF[x]), "is", mode))
    print(paste("The variance of", colnames(TitanicDF[x]), "is", variance))
    print(paste("The range of", colnames(TitanicDF[x]), "is", range))
  }else{
    mode <- getmode(TitanicDF[,x])
    print(paste("The mode of", colnames(TitanicDF[x]), "is", mode))
  }
}
##Checking the function CT
CT(ColNames[1]) #not numeric
CT(ColNames[4]) #numeric

##part(c)
##Creating a function t_test that will perform an indep samples t-test
t_test <- function(){
  t_test <- t.test(TitanicDF[,4]~TitanicDF[,3])
  print(paste("independent-samples t-test:", ColNames[4], "by", ColNames[3]))
  print(t_test)
}

t_test() #checking function
#----------------------------------

###Visual EDA
#----------------------------------
##part(a)
##GRAPHS

##Bubbleplot - Variable 1
bp <- ggplot(TitanicDF, aes(x=TitanicDF[,1], y="count")) + 
  geom_count(aes(color=Survived)) +
  ggtitle(paste("Bubbleplot -", ColNames[1])) +
  scale_x_discrete(ColNames[1])
bp #view bubbleplot

##Jittered Points - Variable 2
jit <- ggplot(TitanicDF,aes(x = TitanicDF[,2], y = "count")) +
  geom_jitter(color=TitanicDF[,2], alpha=I(0.6), size=I(2),
              shape=I(20)) +
  ggtitle(paste("Jittered Points -", ColNames[2])) +
  scale_x_discrete(ColNames[2])
jit #view jittered points

##Pie chart - Variable 3
females <- sum(TitanicDF[,3]=="female") #total females
males <- sum(TitanicDF[,3]=="male") #total males
#Assigning variables
group <- c("male", "female")
value <- c(males, females)
#Creating df
df <- data.frame(group, value)
df
#Create bar plot to visualize data
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp
##Create pie chart
pie <- bp + coord_polar("y", start=0) +
  ggtitle(paste("Pie Chart -",group[1], "&", group[2]))+
  scale_x_discrete(ColNames[3])+
  scale_fill_manual(values=c("palegreen", "turquoise"))
pie #view pie chart

##Histogram - Variable 4
hist <- ggplot(TitanicDF,aes(x = TitanicDF[,4])) +
  geom_histogram(binwidth = 1, 
                 fill=alpha("cornflowerblue",1)) +
  ggtitle(paste("Histogram of", ColNames[4])) +
  scale_x_continuous(ColNames[4])
hist #view histogram

##Density Plot - Variable 5
dp <- ggplot(TitanicDF,aes(x = TitanicDF[,5])) +
  geom_density(aes(color=I("green"))) + 
  ggtitle(paste("Density Plot -", ColNames[5])) +
  scale_x_continuous(ColNames[5])
dp #view dp

##Bar graph - Variable 6
bar <- ggplot(TitanicDF,aes(x = TitanicDF[,6])) +
  geom_bar(aes(fill=Embarked),
           color=c("royal blue","grey","orange")) +
  ggtitle(paste("Bar Graph of", ColNames[6])) +
  scale_x_discrete(ColNames[6]) +
  scale_fill_manual(values=c("royal blue","grey","orange"))
bar #view bar graph

##Frequency Polygon - Variable 7
fp <- ggplot(TitanicDF,aes(x = TitanicDF[,7])) +
  geom_freqpoly(aes(color=I("maroon")),binwidth = 1) + 
  ggtitle(paste("Frequency Polygon -", ColNames[7])) +
  scale_x_continuous(ColNames[7])
fp #view fp

##part(b)
#Visualizing correlation between Fare & Pclass
qplot(Pclass, data=TitanicDF, geom="bar", weight=Fare,
      fill=Pclass)+scale_y_continuous("Fare") +
  ggtitle("Bar Chart - Pclass vs Fare") +
  scale_fill_manual(values = c("khaki","tan1","tomato2"))

##part(c)
##Bar Graph to visualize if Survival affected by Gender. 
bar <- ggplot(TitanicDF,aes(x = TitanicDF[,3])) +
  geom_bar(aes(fill=Survived)) +
  ggtitle("Bar Graph of - Survival by Gender") +
  scale_x_discrete(ColNames[3]) +
  scale_y_discrete(ColNames[1]) +
  scale_fill_manual(values=c("gold1","darkturquoise"))
bar #view bar graph
#----------------------------------

###Feature Generation
#----------------------------------
##Creating a new feature AGEBIN via binning
TitanicDF$AGEBIN <- ifelse(TitanicDF$Age <= 11,"Child",
                       ifelse(TitanicDF$Age <=19,"Teen",
                              ifelse(TitanicDF$Age <= 45,"Middle","Late")))

head(TitanicDF) #view updated DF
View(head(TitanicDF,n=10))
#----------------------------------

###Saving a clean augmented dataframe to a .csv file
write.csv(TitanicDF,"MKS_TitanicDatasetUpdated.csv")
#----------------------------------
