#Credit to Marie Charpignon

library(caret)

# Heart data
heart_df <- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
str(heart_df)
head(heart_df)

# Train/Test split
set.seed(3033)
intrain <- createDataPartition(y = heart_df$V14, p= 0.7, list = FALSE)
training <- heart_df[intrain,]
testing <- heart_df[-intrain,]

# Check dimensions
dim(training)
dim(testing)

# Check absence of NA
anyNA(heart_df)

# Feature summary
summary(heart_df)

# Factor the outcome variable
training[["V14"]] = factor(training[["V14"]])
testing[["V14"]] = factor(testing[["V14"]])

# Part 1 - Train linear SVM model
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)
svm_Linear <- train(V14 ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
# Model output
svm_Linear

# Test model
test_pred <- predict(svm_Linear, newdata = testing)
test_pred
confusionMatrix(test_pred,testing$V14)

# Part 2 - Model tuning
# Choose the value of C leading to best model performance
grid <- expand.grid(C = c(0.001, 0.01, 0.05, 0.1, 0.5, 1, 5, 20))
set.seed(3233)
svm_Linear_Grid <- train(V14 ~., data = training, method = "svmLinear",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 10)
# Model output
svm_Linear_Grid
plot(svm_Linear_Grid)

# Test model
test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid

# Model performance
confusionMatrix(test_pred_grid,testing$V14)
