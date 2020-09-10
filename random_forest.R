
selected_vars <- predictors(lmProfile2)
#list of selected variables from RFE

selected_vars <- append(selected_vars, c("price", "car_ID")) #add price to data
df3 <- df[, selected_vars]
library(dplyr)

#train test split
set.seed(125)

train <- df3 %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(df3, train, by = 'car_ID')

train_id <- data.frame(car_ID = train$car_ID)
test_id  <- data.frame(car_ID = test$car_ID)

train <- train[,-14]
test  <- test[, -14]

X_train <- train[, -13]
Y_train <- train[, 13]

X_test <- test[, -13]
Y_test <- test[, 13]

# Train the model 
library(randomForest)

regr <- randomForest(x = X_train, 
                     y = Y_train , 
                     maxnodes = 20, 
                     ntree = 300)
# Make prediction
predictions <- predict(regr, X_test)

result <- data.frame(car_ID = test_id, price = Y_test)
result['prediction']<-  predictions

head(result)

# Import library for visualization
library(ggplot2)

ggplot()+ 
  geom_line(data=result, aes(x = car_ID, y = price), color="red") +
  geom_line(data=result, aes(x=car_ID, y=prediction ), color="green")

#performance metrices
library(Metrics)
print(paste0('Test RMSE: ' , rmse(Y_test,predictions) )) #RMSE
print(paste0('Test MAE: ' , mae(Y_test,predictions) )) #MAE
print(paste0('Test R2: ' ,
             caret::postResample(predictions , Y_test)['Rsquared'] )) #Rsquared

###########################################################
#grid search
customRF <- list(type = "Regression", 
                 library = "randomForest", 
                 loop = NULL)

customRF$parameters <- data.frame(parameter = c("maxnodes", "ntree"), 
                                  class = rep("numeric", 2), 
                                  label = c("maxnodes", "ntree"))

customRF$grid <- function(x, 
                          y, 
                          len = NULL, 
                          search = "grid") {}

customRF$fit <- function(x, 
                         y, 
                         wts, 
                         param, 
                         lev, 
                         last, 
                         weights, 
                         classProbs, ...) {
  randomForest(x, 
               y, 
               maxnodes = param$maxnodes, 
               ntree=param$ntree, ...)
}

customRF$predict <- function(modelFit, 
                             newdata, 
                             preProc = NULL, 
                             submodels = NULL)
  
predict(modelFit, newdata)

customRF$prob <- function(modelFit, 
                          newdata, 
                          preProc = NULL, 
                          submodels = NULL)
  
predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]

customRF$levels <- function(x) x$classes


# Set grid search parameters
library(caret)
metric<-'RMSE'
control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=3, 
                        search='grid')

# Outline the grid of parameters
seed <- 7
tunegrid <- expand.grid(.maxnodes=c(5, 10, 15, 20), 
                        .ntree=c(100, 200, 300, 400, 500))
set.seed(seed)

# Train the model
library(randomForest)
rf_gridsearch <- train(x=X_train, 
                       y=Y_train, 
                       method=customRF, 
                       metric=metric, 
                       tuneGrid=tunegrid, 
                       trControl=control)

plot(rf_gridsearch)
rf_gridsearch$bestTune
