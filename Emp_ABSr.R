#Clearing the environment
rm(list=ls(all=T))
setwd("E:/edWisor/EdProject")
getwd
library(dplyr)
library(readxl)
library(ggplot2)
library(corrgram)
library(DMwR)
library(caret)
library(randomForest)
library(unbalanced)
library(dummies)
library(e1071)
library(Information)
library(MASS)
library(rpart)
library(gbm)
library(ROSE)
library(DataCombine)
library(class)
library(mice)
library(VIM)
library(outliers)
library(usdm)

############################Reading data file#########################
df = read.csv("E:/edWisor/EdProject/project1.csv", header=TRUE)
names(df)

###########Exploratory data analysis##############
dim(df)
str(df)
head(df)
tail(df)
colnames(df)
summary(df)


# Using above Exploratory Data Analysis categorize data in 2 categories "1.continuous" and "2.categorical"
continuous_vars = c('Distance.from.Residence.to.Work','Service.time','Age','Work.load.Average.day','Transportation.expense','Hit.target','Weight','Height','Body.mass.index','Absenteeism.time.in.hours')

categorical_vars = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week','Seasons','Disciplinary.failure', 'Education', 'Social.drinker','Social.smoker', 'Son', 'Pet')

#################################################################################
#Transforming data types
df$ID = as.numeric(as.character(df$ID))
df$Reason.for.absence[df$Reason.for.absence %in% 0] = 20
df$Reason.for.absence = as.numeric(as.character(df$Reason.for.absence))
df$Month.of.absence[df$Month.of.absence %in% 0] = NA
df$Month.of.absence = as.numeric(as.character(df$Month.of.absence))
df$Day.of.the.week = as.numeric(as.character(df$Day.of.the.week))
df$Seasons = as.numeric(as.character(df$Seasons))
df$Disciplinary.failure = as.factor(as.character(df$Disciplinary.failure))
df$Education = as.factor(as.character(df$Education))
df$Son = as.numeric(as.character(df$Son))
df$Social.drinker = as.factor(as.character(df$Social.drinker))
df$Social.smoker = as.factor(as.character(df$Social.smoker))
df$Pet = as.numeric(as.character(df$Pet))
df$Work.load.Average.day=as.numeric(gsub(",","",df$Work.load.Average.day,fixed=TRUE))

###############################Missing value analysis###############################

mis_vals<- data.frame(apply(df,2,function(x){sum(is.na(x))}))

mis_vals$Columns = row.names(mis_vals)

names(mis_vals)[1] ="mis_percentage"

mis_vals$mis_percentage = (mis_vals$mis_percentage/nrow(df)) * 100
mis_vals = mis_vals[,c(2,1)]
write.csv(mis_vals, "mis_percentage_R.csv", row.names = F)

#df <- as.data.frame(df)

#############################Visualizing missing data######################################

ggplot(data = mis_vals[1:18,], aes(x=reorder(Columns, -mis_percentage),y = mis_percentage))+ geom_bar(stat = "identity",fill = "grey")+xlab("Variables")+ggtitle("Missing data percentage") + theme_bw()

###########################Missing value imputation#############################

sum(is.na(df))
sum(is.na(df$`Body mass index`))
#print(mean(df$`Body mass index`,na.rm=TRUE))
#print(median(df$`Body mass index`,na.rm=TRUE))
#using knn imputation method for missing values
df = knnImputation(df, k=5)


#################################Outlier analysis#######################################
#Getting the data for numeric columns
numeric_index = sapply(df, is.numeric)
numeric_data = df[,numeric_index]

#Getting the data for factor columns
factor_data = df[,!numeric_index]

#Checking outliers using boxplots
for(i in 1:ncol(numeric_data)) {
  assign(paste0("box",i), ggplot(data = df, aes_string(y = numeric_data[,i])) + stat_boxplot(geom = "errorbar", width = 0.5) + geom_boxplot(outlier.colour = "red", fill = "grey", outlier.size = 1) +
           labs(y = colnames(numeric_data[i])) + ggtitle(paste("Boxplot: ",colnames(numeric_data[i]))))
}

#Arrange the plots in grids
gridExtra::grid.arrange(box1,box2,box3,box4,ncol=2)
gridExtra::grid.arrange(box5,box6,box7,box8,ncol=2)
gridExtra::grid.arrange(box9,box10,ncol=2)


##############################################

#Getting names of numeric columns
numeric_columns = colnames(numeric_data)

#Replacing all outlier data with NA
for(i in numeric_columns){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(paste(i,length(val)))
  df[,i][df[,i] %in% val] = NA
}


#Check number of missing values
sapply(df,function(x){sum(is.na(x))})

#Get number of missing values after replacing outliers as NA
missing_values_out = data.frame(sapply(df,function(x){sum(is.na(x))}))
missing_values_out$Columns = row.names(missing_values_out)
row.names(missing_values_out) = NULL
names(missing_values_out)[1] = "miss_perc"
missing_values_out$miss_perc = ((missing_values_out$miss_perc/nrow(df)) *100)
missing_values_out = missing_values_out[,c(2,1)]
missing_values_out = missing_values_out[order(-missing_values_out$miss_perc),]
missing_values_out


#Compute the NA values using KNN imputation
df <- knnImputation(df, k = 3) 

#Using mean for imputation
#for(i in 1:ncol(df)) {
#  df[ ,i][is.na(df[ ,i])] = mean(df[ ,i], na.rm = TRUE)
#}

sum(is.na(df))

str(df)

########################################FEATURE SELECTION########################################
#Check for multicollinearity using VIF
vifstep(numeric_data)

#Check for multicollinearity using corelation graph
corrgram(numeric_data, order = F, upper.panel=panel.pie,text.panel=panel.txt, main = "Correlation Plot")

#Variable Reduction
df = subset.data.frame(df, select = -c(Body.mass.index))

#Make a copy of Clean Data
clean_data = df
library(writexl)
write_xlsx(clean_data, "clean_data.xlsx", col_names = F)

########################################FEATURE SCALING#########################
#Normality check
hist(df$Absenteeism.time.in.hours)

#Remove dependent variable
numeric_index = sapply(df,is.numeric)
numeric_data = df[,numeric_index]
numeric_columns = names(numeric_data)
numeric_columns = numeric_columns[-9]

#Normalization of continuous variables
for(i in numeric_columns){
  print(i)
  df[,i] = (df[,i] - min(df[,i]))/
    (max(df[,i]) - min(df[,i]))
}

#Get the names of factor variables
factor_columns = names(factor_data)

#Create dummy variables of factor variables
df = dummy.data.frame(df, factor_columns)

rmExcept(keepers = c("df"))

########################################DECISION TREE########################################
#RMSE:  0.348
#MAE: 0.306  
#R squared: 0.003
library(rpart.plot)
#Splitting data into train and test data
set.seed(1)
train_index = sample(1:nrow(df), 0.8*nrow(df))        
train = df[train_index,]
test = df[-train_index,]

#Build decsion tree using rpart
dt_model = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")

#Plot the tree
rpart.plot(dt_model)

#Predict for test cases
dt_predictions = predict(dt_model, test[,-26])

#Create data frame for actual and predicted values
df_pred = data.frame("actual"=test[,26], "dt_pred"=dt_predictions)
head(df_pred)

#Calculate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = dt_predictions, obs = test[,26]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")

########################################RANDOM FOREST########################################
#RMSE: 0.1834968 
#MAE: 0.1259944 
#R squared: 0.3306884

##Train the model using training data
rf_model = randomForest(Absenteeism.time.in.hours~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-26])

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test[,26]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")



########################################LINEAR REGRESSION########################################
#RMSE: 0.1961283 
#MAE: 0.1361010 
#R squared: 0.2389948 

##Train the model using training data
lr_model = lm(formula = Absenteeism.time.in.hours~., data = train)

#Get the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model, test[,-26])

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,lr_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_predictions, obs = test[,26]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

########################################DIMENSION REDUCTION USING PCA########################################
#Principal component analysis
prin_comp = prcomp(train)

#Compute standard deviation of each principal component
pr_stdev = prin_comp$sdev

#Compute variance
pr_var = pr_stdev^2

#Proportion of variance explained
prop_var = pr_var/sum(pr_var)

#Cumulative scree plot
plot(cumsum(prop_var), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", type = "b")

#Add a training set with principal components
train.data = data.frame(Absenteeism.time.in.hours = train$Absenteeism.time.in.hours, prin_comp$x)

# From the above plot selecting 45 components since it explains almost 95+ % data variance
train.data =train.data[,1:26]

#Transform test data into PCA
test.data = predict(prin_comp, newdata = test)
test.data = as.data.frame(test.data)

#Select the first 45 components
test.data=test.data[,1:26]

########################################DECISION TREE########################################
#RMSE: 0.12428932 
#MAE: 0.08446198 
#R squared: 0.69948786 

#Build decsion tree using rpart
dt_model = rpart(Absenteeism.time.in.hours ~., data = train.data, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model,test.data)

#Create data frame for actual and predicted values
df_pred = data.frame("actual"=test[,26], "dt_pred"=dt_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = dt_predictions, obs = test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")

########################################RANDOM FOREST########################################
#RMSE: 0.09219915 
#MAE: 0.06414949
#R squared: 0.89601668 

#Train the model using training data
rf_model = randomForest(Absenteeism.time.in.hours~., data = train.data, ntrees = 500)

#Predict the test cases
rf_predictions = predict(rf_model,test.data)

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")

########################################LINEAR REGRESSION########################################
#RMSE: 156764e-15
#MAE: 1.098044e-15
#R squared: 1.000000e+00

#Train the model using training data
lr_model = lm(Absenteeism.time.in.hours ~ ., data = train.data)

#Get the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model,test.data)

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,lr_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_predictions, obs =test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")