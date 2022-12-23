# Install tidyverse.
install.packages('tidyverse')

# Import package.
library(tidyverse)

# Read the CSV file containing turtle sales data
Tur_Sales <- read.csv('/Users/shaunman/Documents/LSE/C3A/LSE_DA301_assignment_files/turtle_sales.csv', header=TRUE)

# Omitted and confirmed that there are no missing values within the dataset
Tur_Sales=na.omit(Tur_Sales)
sum(is.na(Tur_Sales))

# View the first 5 rows of the Tur_Sales Dataframe
head(Tur_Sales,n=5)

# View the dimesnions of the Tur_Sales Dataframe
dim(Tur_Sales)

# View the Internal Structure of the Tur_Sales Dataframe
str(Tur_Sales)

# Remove Redundant Columns from the Dataframe
Tur_Sales= subset(Tur_Sales,select= -c(Year,Ranking,Genre,Publisher))

# View the modified dataframe.
head(Tur_Sales,n=5)

# View the dimesnions of the modified Tur_Sales Dataframe
dim(Tur_Sales)

# View the Internal Structure of the modified Tur_Sales Dataframe
str(Tur_Sales)

# View a summary of the Tur_Sales Dataframe
summary(Tur_Sales)

# Create a scatterplot to visualize Global Sales
qplot(y=Global_Sales, main="Global Sales Scatterplot", xlab='Number of Instances', ylab="Global Sales", data=Tur_Sales)

# Create a scatterplot to visualize NA Sales
qplot(y=NA_Sales, main="NA Sales Scatterplot", xlab='Number of Instances', ylab="NA Sales", data=Tur_Sales)

# Create a scatterplot to visualize EU Sales
qplot(y=EU_Sales,main="EU Sales Scatterplot", xlab='Number of Instances', ylab="EU Sales", data=Tur_Sales)

# Create a bar plot to visualize the Global Sales
qplot(x=Global_Sales, bins=20,main="Global Sales Bar Plot", xlab='Number of Instances', ylab="Global Sales",data=Tur_Sales)

# Create a bar plot to visualize the NA Sales
qplot(x=NA_Sales, bins=20,main="NA Sales Bar Plot", xlab='Number of Instances', ylab="NA Sales",data=Tur_Sales)

# Create a box plot to visualize the Global Sales
qplot(y=Global_Sales,data=Tur_Sales,main="Global Sales Box Plot",geom='boxplot')

# Create a box plot to visualize the NA Sales
qplot(y=NA_Sales,data=Tur_Sales,main="NA Sales Box Plot",geom='boxplot')

# 5.3.0 Load and explore the data and continue to use the data frame 

# View the Minimum Sales across all regions
min(c(Tur_Sales$Global_Sales,Tur_Sales$NA_Sales,Tur_Sales$EU_Sales))

# View the Minimum Sales across all regions
max(c(Tur_Sales$Global_Sales,Tur_Sales$NA_Sales,Tur_Sales$EU_Sales))

# View the Average Sales across all regions
mean(c(Tur_Sales$Global_Sales))

# View Summary of the Tur_Sales Dataframe
summary(Tur_Sales)

# 5.3.0 Determine the impact on sales per product_id
#View the sum of sales grouped by Product ID
tur_group=aggregate(cbind(NA_Sales,EU_Sales,Global_Sales)~Product,Tur_Sales,sum)

# View the summar of the new datframe containaing sales grouped by Product ID
summary(tur_group$NA_Sales)

# Remove outliers with the tur_group data Global_Sales Outliers
Q1 <- quantile(tur_group$Global_Sales,.25)
Q3 <- quantile(tur_group$Global_Sales, .75)
IQR <- IQR(tur_group$Global_Sales)
tur_group <- subset(tur_group, tur_group$Global_Sales > (Q1 - 1.5*IQR) & tur_group$Global_Sales < (Q3 + 1.5*IQR))

# Remove outliers with the tur_group data NA_Sales Outliers
Q1 <- quantile(tur_group$NA_Sales,.25)
Q3 <- quantile(tur_group$NA_Sales, .75)
IQR <- IQR(tur_group$NA_Sales)
tur_group <- subset(tur_group, tur_group$NA_Sales > (Q1 - 1.5*IQR) & tur_group$NA_Sales < (Q3 + 1.5*IQR))

# Remove outliers with the tur_group data EU_Sales Outliers
Q1 <- quantile(tur_group$EU_Sales,.25)
Q3 <- quantile(tur_group$EU_Sales, .75)
IQR <- IQR(tur_group$EU_Sales)
tur_group <- subset(tur_group, tur_group$EU_Sales > (Q1 - 1.5*IQR) & tur_group$EU_Sales < (Q3 + 1.5*IQR))

summary(tur_group)
dim(tur_group)

#5.4.0 Create plots to review and determine insights into the data set.
# Create a scatterplot to visualize Global Sales
qplot(y=Global_Sales, main="Global Sales Scatterplot", xlab='Number of Instances', ylab="Global Sales", data=tur_group)

# Create a bar plot to visualize the Global Sales
qplot(x=Global_Sales, bins=20,main="Global Sales Bar Plot", xlab='Number of Instances', ylab="Global Sales",data=tur_group)

# Create a box plot to visualize the data
qplot(y=Global_Sales,data=tur_group,main="Global Sales Box Plot",geom='boxplot')

# 5.5.0 Determine the normality of the data set
# 5.5.1 Create a qq plot for Sales data
# qqplot for Global Sales Data
qqplot
qqnorm(tur_group$Global_Sales)
qqline(tur_group$Global_Sales)

# qqplot for NA Sales Data
qqplot
qqnorm(tur_group$NA_Sales)
qqline(tur_group$NA_Sales)

# qqplot for EU Sales Data
qqplot
qqnorm(tur_group$EU_Sales)
qqline(tur_group$EU_Sales)


# 5.5.2 Perform a Shapiro-Wilk Test
shapiro.test(tur_group$Global_Sales)
shapiro.test(tur_group$NA_Sales)
shapiro.test(tur_group$EU_Sales)
# All 3 columns result in a p value of 0 which is less than alpha of 0.05
# Hence, we can reject the assumption that the dataset is normally distributed.


# 5.5.3 Determine the Skewness and Kurtosis
install.packages("moments")
library(moments)

# Skewness test for the sales columns
skewness(tur_group$Global_Sales)
# As the Global Sales distribution has a a postive skew of 1.277064,\n
# we can assume that distrubtion is very strongly positively skewed.
skewness(tur_group$NA_Sales)
# As the NA Sales distribution has a a postive skew of 0.7751834,\n
# we can assume that distrubtion is strongly positively skewed.
skewness(tur_group$EU_Sales)
# As the EU Sales distribution has a a postive skew of 0.8574472,\n
# we can assume that distrubtion is very strongly positively skewed.

#Kurtosis test for Sales columns
kurtosis(tur_group$Global_Sales)
# As the kurtosis value is 4.778914>3, it indicates a heavy tailed distribution.
kurtosis(tur_group$NA_Sales)
# As the kurtosis value is 3.503553>3, it indicates a heavy tailed distribution.
kurtosis(tur_group$EU_Sales)
# As the kurtosis value is 2.832725<3, it indicates a light tailed distribution.

#5.6 View the correlation between the datasets
cor(tur_group$Global_Sales,tur_group$NA_Sales)
# Global Sales and NA Sales have a very strong positive correlation of 0.820.
cor(tur_group$Global_Sales,tur_group$EU_Sales)
# Global Sales and EU Sales have a strong positive correlation of 0.745.
cor(tur_group$NA_Sales,tur_group$EU_Sales)
# Global Sales and NA Sales have a moderately positive correlation of 0.523.

#6.3 Create a simple linear regression model.

model1=lm(Global_Sales~NA_Sales,data=tur_group)
summary(model1)
plot(tur_group$NA_Sales,tur_group$Global_Sales)
abline(model1,col='red')

model2=lm(NA_Sales~EU_Sales,data=tur_group)
summary(model2)
plot(tur_group$EU_Sales,tur_group$NA_Sales)
abline(model2,col='blue')

# 6.4 Multiple linear regression model
model3=lm(Global_Sales~NA_Sales+EU_Sales,data=tur_group)
summary(model3)
plot(tur_group$NA_Sales+tur_group$EU_Sales,tur_group$Global_Sales)
abline(model3,col='red')

#6.5 Predict Global Sales based on the provided values.
predict_new=data.frame(NA_Sales=c(34.02,3.93,2.73,2.26,22.08), EU_Sales=c(23.80,1.56,0.65,0.97,0.52))
predict(model3,predict_new)

