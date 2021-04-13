# 1. Load Dataset
dataset <- read.csv("germancredit.csv")
dataset$Default <- factor(dataset$Default)

# 2. Summarize Dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)

# summarize attribute distributions
summary(dataset)

# 3. Re-level variables
## re-level the credit history and a few other variables
dataset$history = factor(dataset$history,levels=c("A30","A31","A32","A33","A34"))
levels(dataset$history) = c("good","good","poor","poor","terrible")
dataset$foreign <- factor(dataset$foreign, levels=c("A201","A202"),labels=c("foreign","german"))
dataset$rent <- factor(dataset$housing=="A151")
dataset$purpose <- factor(dataset$purpose,levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(dataset$purpose) <-c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

# 4. Split data to train and test sets
library(caTools)
set.seed(123)
trainrows <- sample(nrow(dataset), nrow(dataset) * 0.8)
train <- dataset[trainrows, ]
test <- dataset[-trainrows,]

#Train the model
credglm = glm(formula = Default ~ ., family = binomial, data = train)
summary(credglm)

#Predict
predict <- predict(credglm, train, type = "response")
summary(predict)

#Plot ROC
library(ROCR)
ROC_pred = prediction(predict, train$Default)
ROC_perf = performance(ROC_pred, "tpr", "fpr")
#Adding threshold labels
plot(ROC_perf, colorize=TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
abline(a=0, b=1)
auc_train <- round(as.numeric(performance(ROC_pred, "auc")@y.values),2)
legend(.8, .2, auc_train, title = "AUC", cex=1)
