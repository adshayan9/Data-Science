# Load Caret Package for Machine learning capabilities #
library(caret)

# Read csv of true and fake news datasets #
fake_news <- read.csv("Fake.csv",header = T)
true_news <- read.csv("True.csv", header = T)

# Read csv of dataset which contains both true and fake news w. Text Analytics. New_random df contains relevant columns #
all_news <- read.csv("Textanalytics_data.csv", header = T)
new_random <- all_news[,c(4,6,7,8,9,10)]

# Add column to classify whether news is fake or true #
fake_news$class <- "Fake"
true_news$class <- "True"

# Make it one data frame so Machine learning can be used #
all_data <- rbind(fake_news,true_news)

# Randomise the data #
set.seed(10)
rows <- sample(nrow(all_data))
random_data <- all_data[rows,]

# Create a Validation dataset #
# 80% of the rows in the original dataset can be used for training
validation_index <- createDataPartition(new_random$class,p=0.8,list = F)

# select 20% of the data for validation
validation <- new_random[-validation_index,]

# use the remaining 80% of data to training and testing the models
dataset <- new_random[validation_index,]


control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(class~., data=dataset, method="lda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(class~., data=dataset, method="rpart", metric=metric, trControl=control)

# kNN
set.seed(7)
fit.knn <- train(class~., data=dataset, method="knn", metric=metric, trControl=control)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(class~., data=dataset, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(7)
fit.rf <- train(class~., data=dataset, method="rf", metric=metric, trControl=control)

#summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.rf)

# estimate skill of Random Forest on the validation dataset
predictions <- predict(fit.rf, validation)
confusionMatrix(predictions,validation$class)

