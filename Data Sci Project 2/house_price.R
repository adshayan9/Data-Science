
house_data <- read.csv("data.csv", header = T)

# Keep the Relevant Variables for a better model #
clean_house <- house_data[-c(1,15,17,18)]
house_price <- clean_house[c(1,2,3,4,9)]

# Load the data for prediction capabilities #
library(rpart)

# Train a decision tree based on the dataset #
fit <- rpart(price ~ bedrooms + bathrooms + sqft_living + condition, data = house_price)

# Plot regression tree #
plot(fit, uniform = T)
text(fit, cex=.4)

# Initial testing - small sample testing #
print("Making predictions for the following 5 houses:")
print(head(house_price))

print("The predictions are")
print(predict(fit, head(house_price)))

print("Actual price")
print(head(house_price$price))

# We can already see that a basic prediction model won't work as it's not accurate enough #
# We will initially be using decision trees to build our model #

# Load the package for MAE (MEAN ABSOLUTE ERROR) Capabilities #
library(modelr)

# MAE for model fit = 188267.8
mae(model = fit, data = house_price)

# Split training data into training and Test data #
splitData <- resample_partition(house_price, c(test = 0.3, train = 0.7))

lapply(splitData, dim)

fit2 <- rpart(price ~ bedrooms + bathrooms + sqft_living + condition, data = splitData$train)

# MAE for model fit2 = 184195.6
mae(model = fit2, data = splitData$test)


# The code below is a loop that tries different values of maxdept and calls the get_mae function
# This is to find the ideal number of leaves for our decision tree and hence improve the model #
get_mae <- function(maxdepth, target, predictors, training_data, testing_data){
  
  predictors <- paste(predictors, collapse="+")
  formula <- as.formula(paste(target,"~",predictors,sep = ""))
  
  model <- rpart(formula, data = training_data,
                 control = rpart.control(maxdepth = maxdepth))
  mae <- mae(model, testing_data)
  return(mae)
}
  

target <- "price"
predictors <- c("bedrooms", "bathrooms", "sqft_living", "condition")
  
for(i in 1:10){
  mae <- get_mae(maxdepth = i, target = target, predictors = predictors, training_data = splitData$train, testing_data = splitData$test)
  print(glue::glue("Maxdepth: ",i,"\t MAE: ",mae))
} 

# We choose the model which has the lowest MAE #
predicted_prices <- predict(fit2, splitData$test)
test_data <- as.data.frame(splitData$test)
predict_data <- as.data.frame(predicted_prices)
all_datanew <- cbind(test_data, predict_data)


# Random Forests #

# Load the library for Random Forest use #
library(randomForest)
model1 <- randomForest(price ~ bedrooms + bathrooms + sqft_living + condition, data = splitData$train)

# MAE for model1 = 180323
mae(model = model1, data = splitData$test)

train_data <- as.data.frame(splitData$train)

# Similarly for decision trees, we choose the model which has the lowest MAE #
predicted_rfprices <- predict(model1, newdata = splitData$test)
rf_data <- cbind(test_data, predicted = as.data.frame(predicted_rfprices))

