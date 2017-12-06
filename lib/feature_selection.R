######http://r-statistics.co/Variable-Selection-and-Importance-With-R.html

SpeedDating <- read.csv("~/Desktop/project5/SpeedDating_matchrate.csv")
row.names(SpeedDating)<-SpeedDating[,1]
SpeedDating<-data.frame(SpeedDating[,-1])
dim(SpeedDating)

###### correlation------------------------------------------------------------------
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# calculate correlation matrix
correlationMatrix <- cor(SpeedDating[,1:29])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.8)
# print indexes of highly correlated attributes
print(highlyCorrelated)
# correlation<-as.data.frame(correlationMatrix)



###### RF------------------------------------------------------------------
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(SpeedDating[,1:29], SpeedDating[,30], sizes=c(1:29), rfeControl=control)
# summarize the results
print(results)
results$optVariables
# list the chosen features
predictors(results)
# results$fit
# plot the results
plot(results, type=c("g", "o"))


# ###### variable importance based on Generalized cross validation (GCV)------------------------------------------------------------------
# # install.packages("earth")
# library(earth)
# marsModel <- earth( match.prob~ ., data=SpeedDating) # build model
# ev <- evimp (marsModel)
# plot(ev[,1])



###### Boruta------------------------------------------------------------------
# install.packages("Boruta")
library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(match.prob ~ ., data=na.omit(SpeedDating), doTrace=2)  # perform Boruta search
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance

select<-c("age","imprace","income","date","go_out","tvsports","clubbing","reading","concerts","music")

###### ridge regression------------------------------------------------------------------
install.packages("glmnet")
library(glmnet)
library(dplyr)
# x<-as.matrix(SpeedDating[,-30])
x<-as.matrix(select(SpeedDating,age,imprace,income,date,go_out,tvsports,clubbing,reading,concerts,music))
y<-as.vector(SpeedDating[,30])
train <- sample(1:nrow(x), 425)
test <- (-train)
ytest <- y[test]
lambda <- 10^seq(10, -2, length = 100)

#Ridge Regrerssion 
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
bestlam <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
# MSE
mean((ridge.pred-ytest)^2)

# LASSO 
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
# MSE 
mean((lasso.pred-ytest)^2)
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)[1:6,]
