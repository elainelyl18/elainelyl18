## import libraries
library(textir) ## needed to standardize the data
library(class) ## needed for knn
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(caTools)

# 1. Load Dataset
dataset <- read.csv("abalone.csv")
dataset$Age <- dataset$Rings + 1.5

# 2. Summarize Dataset
dim(dataset)
# list types for each attribute
sapply(dataset, class)

# summarize attribute distributions
summary(dataset)
# summarize correlations between input variables
cor(training_set)

# 3. Visualize Dataset
# a) Univariate
# boxplots for each attribute
par(mfrow=c(3,3))
for(i in 2:10) {
  boxplot(dataset[,i], main=names(dataset)[i])
}
# histograms each attribute
par(mfrow=c(3,3))
for(i in 2:10) {
  hist(dataset[,i], main=names(dataset)[i])
}
# density plot for each attribute
par(mfrow=c(3,3))
for(i in 2:10) {
  plot(density(dataset[,i]), main=names(dataset)[i])
}
# b) Multivariate
# scatterplot matrix
pairs(dataset)
# correlation plot
correlations <- cor(dataset[,2:10])
corrplot(correlations, method="circle")

# 4. Feature Selection
# a) remove redundant
set.seed(123)
data <- dataset[-c(1,9)] # drop Sex and Rings

# b) remove highly correlated

# 5. Data Transforms
# a) scale data
data[,1:7] <- scale(data[,1:7]) # scale numerical attributes

# b) split data
split = sample.split(data$Age, SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)
actual <- test_set$Age

# 6. Evaluate Algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "RMSE"

# a) linear algorithms
# LM
set.seed(123)
fit.lm <- train(Age~., data=training_set, method="lm", metric=metric, trControl=control)
p_lm <- predict(fit.lm, test_set)

# b) nonlinear algorithms
# kNN
set.seed(123)
grid <- expand.grid(.k=c(1,3,5,7))
fit.knn <- train(Age~., data=training_set, method="knn", metric=metric, tuneGrid=grid, trControl=control)
p_knn <- predict(fit.knn, test_set)

# c) compare algorithms
lm_score = data.frame( R2 = R2(p_lm, test_set$Age),
                       RMSE = RMSE(p_lm, test_set$Age),
                       MAE = MAE(p_lm, test_set$Age))
knn_score = data.frame( R2 = R2(p_knn, test_set$Age),
                        RMSE = RMSE(p_knn, test_set$Age),
                        MAE = MAE(p_knn, test_set$Age))
lm_score
knn_score