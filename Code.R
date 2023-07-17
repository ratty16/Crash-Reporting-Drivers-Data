#Required library
library(caret)
library(rsample)
library(RWeka)
library(rpart)
library(rpart.plot)
library(MASS)
library(kernlab)
library(pROC)
library(caret)
library(rsample)
library(rpart)
library(rpart.plot)
library(pROC)
data<-read.csv('/Users/ratnas/Downloads/Changed.csv')
data$class<-data$Injury.Severity
data <- subset(data, select = -c(Injury.Severity))
data$class <- factor(data$class)
write.csv(data,'/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
#splitting Data into train and test:
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
write.csv(train,'/Users/ratnas/Desktop/Data Mining/Shivakumar_RatnaMeena-Vetriappan_Sneka_Project/Initialtrain.csv')
write.csv(test,'/Users/ratnas/Desktop/Data Mining/Shivakumar_RatnaMeena-Vetriappan_Sneka_Project/Initialtest.csv')

#10 fold cross Validation with 5 different models
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
#### 1.rpart ####
modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
cm <-confusionMatrix(test_pred, test$class)
cm

####2.KNN####
KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)
knnModel
plot(knnModel)
test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

####3.SVM####
set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)
pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm

####4.Random Forest####
modelLookup("rf")
model <- train(class ~ ., data = train, method = "rf", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)
data<-read.csv('/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
data$class[data$class==0] <-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
write.csv(train,'/Users/ratnas/Desktop/Data Mining/Shivakumar_RatnaMeena-Vetriappan_Sneka_Project/RFtrain.csv')
write.csv(test,'/Users/ratnas/Desktop/Data Mining/Shivakumar_RatnaMeena-Vetriappan_Sneka_Project/RFtest.csv')

ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)

  # Train the Random Forest model
rfFit <- train(x = train[, -23], 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)
## variable importance
imp <- varImp(rfFit)
imp
pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm

####5.nnet####
data<-read.csv('/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
set.seed(31)
nnetFit <- train(x = data[, -23], 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)
nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

###### Feature selection Methods #######
#####1.Recursive Feature Elimination (RFE)#####
data<-read.csv('/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
colnames(data)
data <- subset(data, select = -c(X,X.1))
data$class <- factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class
model <- rpart(class ~ ., data = data)
# Apply RFE
rfe_control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_result <- rfe(X, Y, rfeControl = rfe_control, method = "rpart")
rfe_result
data<-subset(data,select=c(Vehicle.Damage.Extent, Crash.Date.Time, Report.Number, Location, Collision.Type,class))
write.csv(data,'/Users/ratnas/Desktop/Data Mining/feat1.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)

## rpart
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)

modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)

## KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)

knnModel
plot(knnModel)

test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

##SVM
set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm

##nnet
data<-read.csv('/Users/ratnas/Desktop/Data Mining/feat1.csv')
data<-subset(data,select=c(Vehicle.Damage.Extent, Crash.Date.Time, Report.Number, Location, Collision.Type,class))
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class
model <- rpart(class ~ ., data = data)

# Apply RFE
rfe_control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_result <- rfe(X, Y, rfeControl = rfe_control, method = "rpart")
rfe_result
data<-subset(data,select=c(Imagine.that.COVID.19.is.cured.or.eradicated..Going.forward..how.much.of.your.time.would.you.prefer.to.work.remotely., How.much.of.your.time.would.you.have.preferred.to.work.remotely.last.year., How.much.of.your.time.would.you.have.preferred.to.work.remotely.in.the.last.3.months., Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.would.recommend.remote.working.to.others, Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.could.easily.collaborate.with.colleagues.when.working.remotely,class))
write.csv(data,'C:/Users/Aishwarya/Desktop/DataMiningProject/feat1.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
nnetFit <- train(x = data, 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)

nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

#random forest
ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)

set.seed(31)
rfFit <- train(x = train, 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)

## variable importance
imp <- varImp(rfFit)
imp

pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm


#########2.Wrapper-based Feature Selection#########
library(caret)
library(glmnet)
data<-read.csv('/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
colnames(data)
data <- subset(data, select = -c(X.1))
data$class <- factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class
model <- train(x = X, y = Y, method = "glmnet")

# Apply wrapper-based feature selection
glmnet_wrapper_result <- varImp(model, scale = FALSE)
glmnet_wrapper_result
data<-subset(data,select=c(Vehicle.Damage.Extent,Driver.Distracted.By,X,Location,Crash.Date.Time,Surface.Condition,Agency.Name,Vehicle.Movement,Vehicle.Model,Weather,Municipality,Driver.Substance.Abuse,Traffic.Control,Vehicle.Body.Type,Route.Type,Report.Number,Parked.Vehicle,Speed.Limit,Equipment.Problems,class,class))
write.csv(data,'/Users/ratnas/Desktop/Data Mining/feat2.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)

## rpart
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)

modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)


## KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)

knnModel
plot(knnModel)

test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

##SVM

set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm

##nnet
data<-read.csv('/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class
model <- train(x = X, y = Y, method = "glmnet")
glmnet_wrapper_result <- varImp(model, scale = FALSE)
glmnet_wrapper_result
data<-subset(data,select=c(Vehicle.Damage.Extent,Driver.Distracted.By,X,Location,Crash.Date.Time,Surface.Condition,Agency.Name,Vehicle.Movement,Vehicle.Model,Weather,Municipality,Driver.Substance.Abuse,Traffic.Control,Vehicle.Body.Type,Route.Type,Report.Number,Parked.Vehicle,Speed.Limit,Equipment.Problems,class,class))
write.csv(data,'/Users/ratnas/Desktop/Data Mining/feat2.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
nnetFit <- train(x = data, 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)

nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

#random forest
ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)

set.seed(31)
rfFit <- train(x = train, 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)
## variable importance
imp <- varImp(rfFit)
imp
pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm


########3.Filter Method########
data<-read.csv('/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
data <- subset(data, select = -c(X.1))
cors.data<-cor(data)
highly_correlated <- findCorrelation(cors.data, cutoff = 0.5)
data <- data[, -highly_correlated]
highly_correlated
data
data$class <- factor(data$class)
colnames(data)
write.csv(data,'/Users/ratnas/Desktop/Data Mining/feat3.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
## rpart
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)

## KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)
knnModel
plot(knnModel)
test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

##SVM
set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm

#nnet
data<-read.csv('C:/Users/Aishwarya/Desktop/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
cors.data<-cor(data)
highly_correlated <- findCorrelation(cors.data, cutoff = 0.7)
data <- data[, -highly_correlated]
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
nnetFit <- train(x = data, 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)

nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

#random forest
ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)
set.seed(31)
rfFit <- train(x = train, 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)

## variable importance
imp <- varImp(rfFit)
imp
pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm

########4.Chi.Sq########
data<-read.csv('/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
# Convert class variable to factor
data$class <- factor(data$class)
# Perform chi-square test on all predictors
chi_sq_results <- apply(data[, -ncol(data)], 2, function(x) {
  chisq.test(table(data$class, x))
})
# Get p-values from chi-square test results
p_values <- sapply(chi_sq_results, function(x) x$p.value)
# Set significance level
alpha <- 0.05
# Select predictors with p-value < alpha
selected_predictors <- names(p_values[p_values < alpha])
# Subset data with selected predictors
data <- data[, c(selected_predictors, "class")]
colnames(data)
write.csv(selected_data,"/Users/ratnas/Desktop/Data Mining/feat4.csv")
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)

#rpart
modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)
## KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)
knnModel
plot(knnModel)

test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

##SVM
set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm

#nnet
data<-read.csv('/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
cors.data<-cor(data)
highly_correlated <- findCorrelation(cors.data, cutoff = 0.7)
data <- data[, -highly_correlated]
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
nnetFit <- train(x = data, 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)
nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

#random forest
ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)
set.seed(31)
rfFit <- train(x = train, 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)

## variable importance
imp <- varImp(rfFit)
imp
pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm

########5.Boruta Feature Selection########
library(Boruta)
data<-read.csv('/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
data <- subset(data, select = -c(X.1))
data$class <- factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class
# Perform Boruta feature selection
set.seed(123)
boruta_res <- Boruta(X, Y)
# Get selected features from Boruta
selected_features <- getSelectedAttributes(boruta_res, withTentative = FALSE)
selected_features
data<-data[, selected_features]
data$class<-Y
write.csv(data,'/Users/ratnas/Desktop/Data Mining/feat5.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
## rpart
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)

modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)


## KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)

knnModel
plot(knnModel)

test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

##SVM

set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm

#nnet
data<-read.csv('/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
data <- subset(data, select = -c(X))
data$class <- factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class
# Perform Boruta feature selection
set.seed(123)
boruta_res <- Boruta(X, Y)

# Get selected features from Boruta
selected_features <- getSelectedAttributes(boruta_res, withTentative = FALSE)
selected_features
data<-subset(data,select = c(Report.Number,Agency.Name, Crash.Date.Time,Collision.Type,Vehicle.Damage.Extent,Location,class))
temp<-data
temp$newclass[temp$class==0]<-'N'
temp$newclass[temp$class==1]<-'M'
temp$newclass[temp$class==2]<-'Y'
temp$class<-temp$newclass
temp<-subset(temp,select=-c(newclass))
data<-temp
data$class <- as.factor(data$class)
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
nnetFit <- train(x = data, 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)

nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

#random forest
ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)
set.seed(31)
rfFit <- train(x = train, 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)

## variable importance
imp <- varImp(rfFit)
imp
pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm

##############################################################################
#Embedded Method:
library(glmnet)
data<-read.csv('/Users/ratnas/Desktop/Data Mining/FinalPreProcData.csv')
data <- subset(data, select = -c(X.1))
X<- model.matrix(class ~ ., data = data)
Y<-data$class
cv.out <- cv.glmnet(X, Y, alpha = 1)
colnames(data)
lambda <- cv.out$lambda.min
lasso_coef <- predict(cv.out, type = "coef", s = lambda)[,1]
data <- data[, -which(lasso_coef == 0)]
data$class <- factor(data$class)
write.csv(data,'/Users/ratnas/Desktop/Data Mining/feat3.csv')
colnames(data)
set.seed(31)
split <- initial_split(class, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
## rpart
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)

modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)


## KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)

knnModel
plot(knnModel)

test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

##SVM

set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm
#nnet
data<-read.csv('C:/Users/Aishwarya/Desktop/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
X<- model.matrix(class ~ ., data = data)
Y<-data$class
cv.out <- cv.glmnet(X, Y, alpha = 1)
lambda <- cv.out$lambda.min
lasso_coef <- predict(cv.out, type = "coef", s = lambda)[,1]
data <- data[, -which(lasso_coef == 0)]
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
nnetFit <- train(x = data, 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)

nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

#random forest
ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)

set.seed(31)
rfFit <- train(x = train, 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)

## variable importance
imp <- varImp(rfFit)
imp

pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm
