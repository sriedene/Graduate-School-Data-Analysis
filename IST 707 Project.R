library(arules)
library(arulesViz)
library(plyr)
library(rpart)
library(rpart.plot)
library(rattle)
library(stats)
library(ggplot2)
library(factoextra)
library(proxy)
library(naivebayes)
library(e1071)

graduate <- read.csv("C:/Users/User/Desktop/Syracuse/Samantha Riedener Portfolio/Project 2 IST 707/Admission_Predict_Ver1.1.csv")
str(graduate)
length(which(is.na(graduate)))
summary(graduate)

graduate$Research <- as.factor(graduate$Research)
graduate <- graduate[,-1]
graduate2 <- graduate
graduate2$Chance.of.Admit[graduate2$Chance.of.Admit >= 0.67] <- 1
graduate2$Chance.of.Admit[graduate2$Chance.of.Admit < 0.67] <- 0
graduate2$Chance.of.Admit <- as.factor(graduate2$Chance.of.Admit)
levels(graduate2$Chance.of.Admit)[levels(graduate2$Chance.of.Admit)=="1"] <- "Admit"
levels(graduate2$Chance.of.Admit)[levels(graduate2$Chance.of.Admit)=="0"] <- "Decline"


table(graduate$SOP)
table(graduate$LOR)
table(graduate$University.Rating)
table(graduate$Research)

hist(graduate$Chance.of.Admit, main = "Histogram of Admissions Percentage", xlab="Chance of Admission")
hist(graduate$GRE.Score, main = 'Histogram of GRE Scores', xlab="GRE Score")
hist(graduate$TOEFL.Score, main="Histogram of TOEFL Scores", xlab="TOEFL Score")
hist(graduate$CGPA, main="Histogram of CGPA", xlab="CGPA")
barplot(table(graduate$University.Rating), main ="University Ratings", ylab="Total", xlab="Rating")
barplot(table(graduate$SOP),main = "Statement of Purpose Rating", ylab='Total', xlab="Rating", ylim=c(0,100))
barplot(table(graduate$LOR),main = "Letters of Intent Rating", ylab='Total', xlab="Rating", ylim=c(0,100))
barplot(table(graduate$Research), main = 'Research Expereince', ylab = 'Total', xlab = 'Research?', ylim=c(0,300))

graduate_ar <- graduate

graduate_ar$SOP <- as.factor(graduate_ar$SOP)
graduate_ar$SOP <- revalue(graduate_ar$SOP, c("1"="SOP.1", "1.5"="SOP.1.5", "2"="SOP.2", "2.5"="SOP.2.5", "3"="SOP.3", "3.5"="SOP.3.5", "4"="SOP.4", "4.5"="SOP.4.5", "5"="SOP.5"))
graduate_ar$LOR <- as.factor(graduate_ar$LOR)
graduate_ar$LOR <- revalue(graduate_ar$LOR, c("1"="LOR.1", "1.5"="LOR.1.5", "2"="LOR.2", "2.5"="LOR.2.5", "3"="LOR.3", "3.5"="LOR.3.5", "4"="LOR.4", "4.5"="LOR.4.5", "5"="LOR.5"))
graduate_ar$University.Rating <- as.factor(graduate_ar$University.Rating)
graduate_ar$University.Rating <- revalue(graduate_ar$University.Rating, c("1"="UR.1", "2"="UR.2",  "3"="UR.3",  "4"="UR.4",  "5"="UR.5"))
graduate_ar$GRE.Score <- discretize(graduate_ar$GRE.Score, method = "interval")
graduate_ar$TOEFL.Score <- discretize(graduate_ar$TOEFL.Score, method = "interval")
graduate_ar$CGPA <- discretize(graduate_ar$CGPA, method = "interval")
graduate_ar$Chance.of.Admit[graduate_ar$Chance.of.Admit >= 0.67] <- 1
graduate_ar$Chance.of.Admit[graduate_ar$Chance.of.Admit < 0.67] <- 0
graduate_ar$Chance.of.Admit <- as.factor(graduate_ar$Chance.of.Admit)
levels(graduate_ar$Chance.of.Admit)[levels(graduate_ar$Chance.of.Admit)=="1"] <- "Admit"
levels(graduate_ar$Chance.of.Admit)[levels(graduate_ar$Chance.of.Admit)=="0"] <- "Decline"
levels(graduate_ar$Research)[levels(graduate_ar$Research)=="1"] <- "Yes"
levels(graduate_ar$Research)[levels(graduate_ar$Research)=="0"] <- "No"
 
graduate_ar <- graduate_ar[,-1]

#AR Mining
write.csv(graduate_ar, "C:/Users/User/Desktop/Syracuse/graduate_ar.csv")
graduate_transaction <- read.transactions("C:/Users/User/Desktop/Syracuse/graduate_ar.csv", format = "basket", sep = ",")
first.rules <- apriori(graduate_transaction, parameter = list(supp = 0.01, conf = 0.85), appearance = list(default="lhs", rhs="Admit"))
inspect(first.rules[1:20])
second.rules <- apriori(graduate_transaction, parameter = list(supp = 0.05, conf = 0.85), appearance = list(default="lhs", rhs="Admit"))
inspect(second.rules[1:20])
third.rules <- apriori(graduate_transaction, parameter = list(supp = 0.05, conf = 0.9), appearance = list(default="lhs", rhs="Admit"))
inspect(third.rules[1:20])
fourth.rules <- apriori(graduate_transaction, parameter = list(supp = 0.1, conf = 0.9), appearance = list(default="lhs", rhs="Admit"))
fourth.rules.sorted <- sort(fourth.rules, by="lift", decreasing=TRUE)
fourth.rules.sorted2 <- sort(fourth.rules, by="support", decreasing=TRUE)
fourth.rules.sorted3 <- sort(fourth.rules, by="confidence", decreasing=TRUE)
inspect(fourth.rules.sorted[1:10])
inspect(fourth.rules.sorted2[1:10])
inspect(fourth.rules.sorted3[1:10])
fourth.plot <- fourth.rules.sorted[1:20]
plot(fourth.plot, method="graph")

#Decision Tree
fit <- rpart(graduate_ar$Chance.of.Admit ~ ., data = graduate_ar, method="class")
fancyRpartPlot(fit)
fit2 <- rpart(graduate_ar$Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating + CGPA + Research, data = graduate_ar, method="class")
fancyRpartPlot(fit2)
fit3 <- rpart(graduate_ar$Chance.of.Admit ~ GRE.Score + University.Rating + Research, data = graduate_ar, method="class")
fancyRpartPlot(fit3)

#Clustering Analysis
graduate_scale <- graduate
graduate_scale$Research <- as.numeric(graduate_scale$Research)
graduate_scale <- scale(graduate_scale)
graduate_scale <- graduate_scale[,-1]

fviz_nbclust(graduate_scale, kmeans, method = 'wss')
fviz_nbclust(graduate_scale, kmeans, method = 'silhouette')

k2 <- kmeans(graduate_scale, centers=2, nstart= 25)
fviz_cluster(k2, data = graduate_scale)
k3 <- kmeans(graduate_scale, centers=3, nstart= 25)
fviz_cluster(k3, data = graduate_scale)
k4 <- kmeans(graduate_scale, centers=4, nstart= 25)
fviz_cluster(k4, data = graduate_scale)
print(k2)
print(k3)

graduate_matrix <- data.matrix(graduate_scale)
m  <- graduate_matrix
distMatrix_E <- dist(m, method="euclidean")
distMatrix_C <- dist(m, method='cosine')

# Euclidean
groups_E <- hclust(distMatrix_E,method="ward.D")
plot(groups_E, cex=0.9, hang=-1)
rect.hclust(groups_E, k=4)

# Cosine Similarity
groups_C <- hclust(distMatrix_C,method="ward.D")
plot(groups_C, cex=0.9, hang=-1)
rect.hclust(groups_C, k=4)

# Naive Bayes
N <- nrow(graduate2)
# Number of desired splits
kfolds <- 5
# Generate indices of holdout observations
holdout <- split(sample(1:N), 1:kfolds)

# All variables
AllResults<-list()
AllLabels<-list()
for (k in 1:kfolds){
  
  Kdigit_Test=graduate2[holdout[[k]], ]
  Kdigit_Train=graduate2[-holdout[[k]], ]
  
  
  ## Make sure you take the labels out of the testing data
  (head(Kdigit_Test))
  Kdigit_Test_noLabel<-Kdigit_Test[-c(8)]
  Kdigit_Test_justLabel<-Kdigit_Test$Chance.of.Admit
  
  
  # naivebayes
  ## formula is label ~ x1 + x2 + .  NOTE that label ~. is "use all to create model"
  NB<-naive_bayes(Chance.of.Admit~., data=Kdigit_Train, laplace=0, na.action = na.pass)
  NB_Pred <- predict(NB, Kdigit_Test_noLabel)
  
  
  ## Accumulate results from each fold
  AllResults<- c(AllResults,NB_Pred)
  AllLabels<- c(AllLabels, Kdigit_Test_justLabel)
  
}
table(unlist(AllResults),unlist(AllLabels))

# AR variables
AllResults2<-list()
AllLabels2<-list()
for (k in 1:kfolds){
  
  Kdigit_Test=graduate2[holdout[[k]], ]
  Kdigit_Train=graduate2[-holdout[[k]], ]
  
  
  ## Make sure you take the labels out of the testing data
  (head(Kdigit_Test))
  Kdigit_Test_noLabel<-Kdigit_Test[-c(8)]
  Kdigit_Test_noLabel<-Kdigit_Test_noLabel[-c(5)]
  Kdigit_Test_noLabel<-Kdigit_Test_noLabel[-c(4)]
  Kdigit_Test_justLabel<-Kdigit_Test$Chance.of.Admit
  
  
  # naivebayes
  ## formula is label ~ x1 + x2 + .  NOTE that label ~. is "use all to create model"
  NB2<-naive_bayes(Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating + CGPA + Research, data=Kdigit_Train, laplace=0, na.action = na.pass)
  NB_Pred2 <- predict(NB2, Kdigit_Test_noLabel)
  
  
  ## Accumulate results from each fold
  AllResults2<- c(AllResults2,NB_Pred2)
  AllLabels2<- c(AllLabels2, Kdigit_Test_justLabel)
  
}
table(unlist(AllResults2),unlist(AllLabels2))

# Decision Tree variables
AllResults3<-list()
AllLabels3<-list()
for (k in 1:kfolds){
  
  Kdigit_Test=graduate2[holdout[[k]], ]
  Kdigit_Train=graduate2[-holdout[[k]], ]
  
  
  ## Make sure you take the labels out of the testing data
  (head(Kdigit_Test))
  Kdigit_Test_noLabel<-Kdigit_Test[-c(8)]
  Kdigit_Test_noLabel<-Kdigit_Test_noLabel[-c(6)]
  Kdigit_Test_noLabel<-Kdigit_Test_noLabel[-c(2)]
  Kdigit_Test_justLabel<-Kdigit_Test$Chance.of.Admit
  
  
  # naivebayes
  ## formula is label ~ x1 + x2 + .  NOTE that label ~. is "use all to create model"
  NB3<-naive_bayes(Chance.of.Admit ~ GRE.Score + University.Rating + SOP + LOR + Research, data=Kdigit_Train, laplace=0, na.action = na.pass)
  NB_Pred3 <- predict(NB3, Kdigit_Test_noLabel)
  
  
  ## Accumulate results from each fold
  AllResults3<- c(AllResults3,NB_Pred3)
  AllLabels3<- c(AllLabels3, Kdigit_Test_justLabel)
  
}
table(unlist(AllResults3),unlist(AllLabels3))

# SVM
svm_graduate_fit_p <- svm(Chance.of.Admit~., data=graduate2, kernel="polynomial", cost=1, scale=FALSE)
pred_graduate_p <- predict(svm_graduate_fit_p, graduate2[-8], type="class")
table(pred_graduate_p, graduate2$Chance.of.Admit)

svm_graduate_fit_p2 <- svm(Chance.of.Admit~., data=graduate2, kernel="polynomial", cost=10, scale=FALSE)
pred_graduate_p2 <- predict(svm_graduate_fit_p2, graduate2[-8], type="class")
table(pred_graduate_p2, graduate2$Chance.of.Admit)

svm_graduate_fit_p3 <- svm(Chance.of.Admit~., data=graduate2, kernel="polynomial", cost=100, scale=FALSE)
pred_graduate_p3 <- predict(svm_graduate_fit_p3, graduate2[-8], type="class")
table(pred_graduate_p3, graduate2$Chance.of.Admit)

svm_graduate_fit_l <- svm(Chance.of.Admit~., data=graduate2, kernel="linear", cost=1, scale=FALSE)
pred_graduate_l <- predict(svm_graduate_fit_l, graduate2[-8], type="class")
table(pred_graduate_l, graduate2$Chance.of.Admit)

svm_graduate_fit_l2 <- svm(Chance.of.Admit~., data=graduate2, kernel="linear", cost=10, scale=FALSE)
pred_graduate_l2 <- predict(svm_graduate_fit_l2, graduate2[-8], type="class")
table(pred_graduate_l2, graduate2$Chance.of.Admit)

svm_graduate_fit_l3 <- svm(Chance.of.Admit~., data=graduate2, kernel="linear", cost=100, scale=FALSE)
pred_graduate_l3 <- predict(svm_graduate_fit_l3, graduate2[-8], type="class")
table(pred_graduate_l3, graduate2$Chance.of.Admit)

svm_graduate_fit_r <- svm(Chance.of.Admit~., data=graduate2, kernel="radial", cost=1, scale=FALSE)
pred_graduate_r <- predict(svm_graduate_fit_r, graduate2[-8], type="class")
table(pred_graduate_r, graduate2$Chance.of.Admit)

svm_graduate_fit_r2 <- svm(Chance.of.Admit~., data=graduate2, kernel="radial", cost=10, scale=FALSE)
pred_graduate_r2 <- predict(svm_graduate_fit_r2, graduate2[-8], type="class")
table(pred_graduate_r2, graduate2$Chance.of.Admit)

svm_graduate_fit_r3 <- svm(Chance.of.Admit~., data=graduate2, kernel="radial", cost=100, scale=FALSE)
pred_graduate_r3 <- predict(svm_graduate_fit_r3, graduate2[-8], type="class")
table(pred_graduate_r3, graduate2$Chance.of.Admit)