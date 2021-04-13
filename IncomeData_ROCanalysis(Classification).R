#Import dataset
income = read.csv('income.csv')

#Remove rows with missing values
table(income$workclass)
table(income$occupation)
income[income == "?"] <- NA
income <- na.omit(income)

#Encode income
income$income <- ifelse(income$income == "<=50K", 0, 1)
table(income$income)
as.factor(income$income)

#Combine workclass
income$workclass <- as.character(income$workclass)
income$workclass[income$workclass == "Without-pay" | 
                   income$workclass == "Never-worked"] <- "Jobless"
income$workclass[income$workclass == "State-gov" |
                   income$workclass == "Local-gov"]  <- "Govt" 
income$workclass[income$workclass == "Self-emp-inc" |
                   income$workclass == "Self-emp-not-inc"]  <- "Self-employed" 
table(income$workclass)

#Combine marital status
income$marital.status <- as.character(income$marital.status)
income$marital.status[income$marital.status == "Married-AF-spouse" |
                        income$marital.status == "Married-civ-spouse" |
                        income$marital.status == "Married-spouse-absent"] <- "Married"
income$marital.status[income$marital.status == "Divorced" |
                        income$marital.status == "Separated" |
                        income$marital.status == "Widowed"] <- "Not-Married"
table(income$marital.status)

#Combine Country
income$native.country <- as.character(income$native.country)
north.america <- c("Canada", "Cuba", "Dominican-Republic", "El-Salvador", "Guatemala",
                   "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
                   "Outlying-US(Guam-USVI-etc)", "Puerto-Rico", "Trinadad&Tobago",
                   "United-States")
asia <- c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
          "Philippines", "Taiwan", "Thailand", "Vietnam")
south.america <- c("Columbia", "Ecuador", "Peru")
europe <- c("England", "France", "Germany", "Greece", "Holand-Netherlands",
            "Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland",
            "Yugoslavia")
other <- c("South", "?")
income$native.country[income$native.country %in% north.america] <- "North-America"
income$native.country[income$native.country %in% asia]  <- "Asia"
income$native.country[income$native.country %in% south.america] <- "South-America" 
income$native.country[income$native.country %in% europe] <-  "Europe"  
income$native.country[income$native.country %in% other] <- "Other"
table(income$native.country)

#Split data
library(caTools)
set.seed(123)
split <- sample.split(income$income, SplitRatio = 0.84)
train <- subset(income, split == TRUE)
test <- subset(income, split == FALSE)

#Train the model
logit <- glm(income ~ ., family = binomial(), train)

#Predict
predict<- predict(logit, train, type = "response")

#Plot ROC
library(ROCR)
ROC_pred = prediction(predict, train$income)
ROC_perf = performance(ROC_pred, "tpr", "fpr")
#Adding threshold labels
plot(ROC_perf, colorize=TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
abline(a=0, b=1)
auc_train <- round(as.numeric(performance(ROC_pred, "auc")@y.values),2)
legend(.8, .2, auc_train, title = "AUC", cex=1)

# Making predictions on test set
Pred_Test <- predict(logit, type = "response", newdata = test)

# Convert probabilities to values using the below
## Based on ROC curve above, selected a threshold of 0.5
test_tab <- table(test$income, Pred_Test > 0.5)
test_tab
accuracy_test <- round(sum(diag(test_tab))/sum(test_tab),2)
sprintf("Accuracy on test set is %s", accuracy_test)