#Reading the data into R
library(readr)
df <- read.csv("D:/Trine University/Business Analytics/Data Science and Big data/Week 3/HMEQ_Scrubbed.csv")
View(df)

#Listing the structure of the data
str(df)

#Showcasing the summary of the data

summary(df)

#Printing the first six records
head(df)

#Removing the Target loss amount
df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

head(df_flag)

summary (df_flag)
#Loading the Required R packages

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(ROCR)



#setting the Decision tree depth
tr.set =rpart.control(maxdepth=3)

#creating the Decision tree

tree_flag = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr.set )
tree_flag

rpart.plot( tree_flag )

#importance of variables

tree_flag$variable.importance


#DecisionTree using Gini Score



tr_set = rpart.control( maxdepth = 3 )

#Using Gini metric to check the split
t1G = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
rpart.plot(t1G)


#Using Entropy metric to check the split

t1E = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )
rpart.plot(t1E)

#Important variables of Gini tree
t1G$variable.importance

#Important variables of Entropy tree
t1E$variable.importance

#Creating a ROC curve for both trees to check the accuracy

library(ROCR)
pG = predict(t1G, df)
pG2 = prediction(pG[,2], df$TARGET_BAD_FLAG)
pG3 = performance( pG2, "tpr", "fpr")




pE = predict(t1E, df)
pE2 = prediction( pE[,2], df$TARGET_BAD_FLAG)
pE3 = performance( pE2, "tpr", "fpr")
plot (pE3, col="blue", lwd = 2)
plot(pG3, col="red", lwd = 3, add =TRUE)

#Adding more details to the curve
legend("topleft", 
       c("GINI ACCURACY", "ENTROPY ACCURACY"), 
       col = 1:2,
       cex = 0.8,
       pch = 16,
       bty = "y")
abline(0,1,lty=2)

#Calculating area under the curve

accuracyGini = performance(pG2, "auc")@y.values
accuracyEntro = performance(pE2, "auc")@y.values
print(accuracyGini)
print(accuracyEntro)



#Creating dummy variable

df_dummyR = df
df_dummyR$TARGET_BAD_FLAG = NULL

#calculating the mean of target loss amount
mean(df$TARGET_LOSS_AMT)

#Setting tree depth
tree_depth = rpart.control(maxdepth = 3)

#Creating Regression Decision Tree using anova
ano_Tree = rpart(data = df_dummyR, TARGET_LOSS_AMT ~ ., control = tree_depth,
                 method = "anova")
rpart.plot(ano_Tree)

#Creating Regression Decision Tree using Poisson

poi_tree = rpart(data = df_dummyR, TARGET_LOSS_AMT ~ ., control = tree_depth,
                 method = "poisson")
rpart.plot (poi_tree)


#Creating a decision tree using poisson
poi_Tree = rpart(data = df_dummyR, TARGET_LOSS_AMT ~ ., control = tree_depth, 
                 method = "poisson")
rpart.plot(poi_Tree)

#Finding the important variables for both the trees

ano_Tree$variable.importance
poi_Tree$variable.importance



#Calculating the Root Mean Square Error (RMSE) for both trees
p_Ano = predict(ano_Tree, df)
p_Poi = predict(poi_Tree, df)
rmse_Anova = sqrt(mean((df$TARGET_LOSS_AMT - p_Ano)^2))
rmse_Poisson = sqrt(mean((df$TARGET_LOSS_AMT - p_Poi)^2))
print(rmse_Anova)
print(rmse_Poisson)

#Creating a dummy variable for the data set for Probability/Severity Model Decision Tree
df_ps = df
df_ps$TARGET_LOSS_AMT = NULL
summary(df_ps)




#setting the depth
ps_depth = rpart.control(maxdepth = 10)

#Creating Probability/Severity Model decision tree for predicting TARGET_BAD_FLAG
ps_t1 = rpart (data = df_ps, TARGET_BAD_FLAG ~ ., control = ps_depth, 
                   method = "class")
rpart.plot(ps_t1)

#Predicting the probability of loan being default

pred_df1 = predict(ps_t1, df)
print(pred_df1)

#Creating Probability/Severity Model decision tree for predicting TARGET_LOSS_AMT

df_ps2 = df

df_ps2 = subset(df, TARGET_BAD_FLAG == 1)
df_ps2$TARGET_BAD_FLAG = NULL
summary(df_ps2)

library(rpart)
library(rpart.plot)
ps_t2 <- rpart(TARGET_LOSS_AMT ~ ., data = df_ps2, method = "class", control = ps_depth)

rpart.plot(ps_t2)

#Predicting the probability of loss given default
pred_tlm = predict(ps_t2, df)
print(pred_tlm)

#Multiplying the predicted value of both for  actual prediction
pred_df1 <- as.numeric(pred_df1)
pred_tlm <- as.numeric(pred_tlm)
mValue = pred_df1 * pred_tlm
print(mValue)



#Finding the RMSE for both
rmse_tbf = sqrt(mean((df$TARGET_BAD_FLAG - mValue)^2))
rmse_tlm = sqrt(mean((df$TARGET_LOSS_AMT - mValue)^2))
print(rmse_tbf)
print(rmse_tlm)
