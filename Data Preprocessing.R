#Remove warnings
options(warn=-1)

#Importing Libraries
#library(XLConnect)
#library(xlsx)
library(DMwR)
library(infotheo)
library(magrittr)
library(dplyr)
library(dummies)
library(vegan)

#Setting up working directory
getwd()
setwd("C:/Users/Ben Roshan/Documents")

#Importing CSV file in R
readLines(con='German-Credit_1.csv',n=5)
df1=read.table(file='German-Credit_1.csv',header=T,sep=',')
head(df1)
df1=read.csv(file='German-Credit_1.csv',header=T)
head(df1)
df2=read.csv(file='German-Credit_2.csv',header=T)
head(df2)

#Reading excel file
#nifty_excel <- read.xlsx(location2, sheetIndex = 1)

#Displaying column names
colnames(df1)
colnames(df2)

#Merge two dataframes
df3=merge(x=df1,y=df2,by='OBS',all=T)

#Explore data
head(df3)
tail(df3)
summary(df3)
str(df3)

#Splitting numerical the numerical and categorical attributes
numattr=c('DURATION','AMOUNT','INSTALL_RATE','AGE','NUM_CREDITS','NUM_DEPENDENTS')
catattr=setdiff(x=colnames(df3),y=numattr)

#Converting to character
df3$OBS=as.character(df3$OBS)
str(df3)

#test conversion of numeric and factor 
test_vec=c(0,1,1,1,1,0,0,0,1,0,1,0,1,0)
test_vec
fac_vec=as.factor(test_vec)
fac_vec
reconverted_vec=as.numeric(fac_vec)
reconverted_vec

#Converting to factor by converting into character
df3$RESPONSE=as.factor(as.character(df3$RESPONSE))

#Subsetting and converting into character cat attributes 
df_cat=subset(df3,select = catattr)
df3 %<>%mutate_each_(funs(factor(.)),catattr)
str(df3)
#df3[,catattr]=data.frame(apply(df_cat,2,function(x) as.factor(as.character(x))))
#str(df3)

#Handling missing values
##Sum of NA values
colSums(is.na(x=df3))
sum(is.na(df3))

#Drop missing values
df4=na.omit(df3)
dim(df3)
dim(df4)
sum(is.na(df4))

#Imputing the missing values
manyNAs(df3,0.1)

#Central imputation
df3_imputed=centralImputation(data=df3) 
sum(is.na(df3_imputed))

#Knn imputation
df3_imputed1=knnImputation(data=df3,k=5)
sum(is.na(df3_imputed1))

#Binning the variable
x=c(5,6,7,8,8,8,8,11,20,21,22)

x0=discretize(x,disc = "equalfreq",nbins=4)
table(x0)

x1=discretize(x,disc = "equalwidth",nbins=4)
table(x1)

#Doing in our dataframe
amtbin=discretize(df3_imputed$AMOUNT,disc="equalfreq",nbins=4)
table(amtbin)

amtbin=discretize(df3_imputed$AMOUNT,disc="equalwidth",nbins=4)
table(amtbin)

#Dummy variables-created for categorical variables
df_ex=datasets::warpbreaks
table(df_ex$tension)

dummy_ex=dummy(df_ex$tension)

head(dummy_ex)

df_cat=subset(df3_imputed,select=catattr)
df_cat_dummies=data.frame(apply(df_cat,2,function(x) dummy(x)))
dim(df_cat_dummies)

#Standardization of numerical attributes
df_num=df3_imputed[,numattr]
#Range method
df_num2=decostand(x=df_num,method='range')
summary(df_num2)

#Standardization
df_num3=decostand(x=df_num,method='standardize')
summary(df_num3)

#Binding the dataframe
df_final=cbind(df_num3,df_cat)
head(df_final)

#Train-Test split
rows=seq(1,1000,1)
set.seed(123)
trainrows=sample(rows,600)
train_data=df_final[trainrows,]
test_data=df_final[-c(trainrows),]
dim(train_data)
dim(test_data)

#Building model
lm_model=lm(AMOUNT~DURATION,data=train_data)
summary(lm_model)


#Data Visualizations
data=df_final

#Histogram
hist(data$AGE)
hist(data$AGE,col="yellow")

#Boxplot
boxplot(data$AGE,horizontal = TRUE)
boxplot(AMOUNT~RESPONSE,data=data,xlab="TARGET",ylab="AMOUNT",main="Continous vs Categorical")

#Barplot
barplot(table(data$RESPONSE))
barplot(table(data$RESPONSE),col="Green")

#Scatterplot
plot(x=data$AGE,y=data$AMOUNT,xlab="DURATION",ylab="AMOUNT",main="Continuous vs Continuous")
