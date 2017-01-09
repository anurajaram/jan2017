# Author - Anupama Rajaram
# Date - Jan 8, 2017
# Description - Hypothesis Testing
# Dataset - birth dataset from Delaware Open Data


# Start by cleaning up memory of current R session:
rm(list=ls(all=TRUE))


# decimal places - only 4
options(digits = 4)

# call source file with all required packages.
source("workspace_prep.R")
library("data.table")
library(httr)
library(jsonlite)
library(rjson)
library(RJSONIO)


# ===================================================
# Step 1 - Prepare Workspace
# ===================================================
# a) Load files:
birthdf2 = data.frame(fread("Births.csv"), stringsAsFactors = FALSE)

birthdf = subset(birthdf2, STATE.OF.RESIDENCE == "DE")
# We remove ~4800 records of out-of-state moms.

rm(birthdf2)



# b) Basic exploration of data 
head(birthdf)

summary(birthdf[, c(1,18)])

table(birthdf$COUNTY.OF.RESIDENCE, birthdf$BIRTH.WEIGHT)
# Birthweight-> <1500   1500-2499     2500+
# Kent            161       778       10099
# New Castle      664       2293      30420
# Sussex          161       698       10397

table(birthdf$BIRTH.WEIGHT)
# categories:         <1500     1500-2499     2500+ 
# num of records:     986         3769         50916


# c) check how many unique values exist for each column.
sapply(birthdf, function(x) length(unique(x)))

# We remove the columns for "count" and "state of residence" since they are
# constant for all records. (1 and DE respectively)
birthdf$Count = NULL
birthdf$STATE.OF.RESIDENCE = NULL



# d) check for NAs - both datasets have 0 NA values, so we do not need
# to apply any corrections.
sapply(birthdf, function(x) sum(is.na(x)))
# None of the columns show missing values, so we do not need to process them.



# e) creating a test and train set:
set.seed(270)
seq_index = sample(seq_len(nrow(birthdf)), size = 8000)

traindf = birthdf[-seq_index,]
testdf = birthdf[seq_index,]

set.seed(1986)
seq_index2 = sample(seq_len(nrow(testdf)), size = 5000)
test1 = testdf[seq_index2,]
test2 = testdf[-seq_index2,]

rm(birthdf, testdf, seq_index, seq_index2)




# ========================================================================
# Step 2 - Initial Hypothesis testing for predictors affecting target 
# variable (birthweight)
# ========================================================================

# Put the relationship table here:

attach(traindf)

# 1) Relation between birth_weight and mom's_ethnicity
chisq.test(BIRTH.WEIGHT, MOM.s.RACE)
# relation exists:
# Pearson's Chi-squared test
# data:  BIRTH.WEIGHT and MOM.s.RACE
# X-squared = 600, df = 6, p-value < 2e-16

# post-hoc tests: 
# BONFERRONI adjustment p-val = 0.0833 (6 comparisons)
# a) between "black" and "other"
tdf = subset(traindf, (MOM.s.RACE == "BLACK" | MOM.s.RACE == "OTHER"))
p = prop.table(table( tdf$BIRTH.WEIGHT, tdf$MOM.s.RACE),2)*100
chisq.test(p)

# b) between "black" and "white"
tdf = subset(traindf, (MOM.s.RACE == "UNKNOWN" | MOM.s.RACE == "WHITE"))
p = prop.table(table( tdf$BIRTH.WEIGHT, tdf$MOM.s.RACE),2)*100
chisq.test(p)
# p-val = 0.0003, relation exists



# 2) Relation between birth_weight and when prenatal_care started
chisq.test(BIRTH.WEIGHT, START.OF.PRENATAL.CARE)
# no relation so far

# post-hoc tests:
# BONFERRONI adjustment p-val = 5e-3 (10 comparisons)
# a) between "DK" and "NO PRENATAL CARE"
tdf = subset(traindf, (START.OF.PRENATAL.CARE == "DK" | 
                         START.OF.PRENATAL.CARE == "NO PRENATAL CARE"))
p = prop.table(table( tdf$BIRTH.WEIGHT, tdf$START.OF.PRENATAL.CARE),2)*100
chisq.test(p)  # p-val = 0.9

# b) between "FIRST" and "THIRD"
tdf = subset(traindf, (START.OF.PRENATAL.CARE == "FIRST" | START.OF.PRENATAL.CARE == "THIRD"))
p = prop.table(table( tdf$BIRTH.WEIGHT, tdf$START.OF.PRENATAL.CARE),2)*100
chisq.test(p) # p-val = 0.5

#NONE of the segments show p-val < 0.005. the smallest p-val is between third and no-prenatal
# care at 0.03



# 3) Relation between birth_weight and gestation period
chisq.test(BIRTH.WEIGHT, GESTATION)
# P-val = 2e-16

# post-hoc tests:
# BONFERRONI adjustment p-val = 0.0833 (6 comparisons)
# a) between "42+ wks" and "<37 wks"
tdf = subset(traindf, (GESTATION == "POSTTERM 42+ WKS" | 
                         GESTATION == "PRETERM <37 WKS"))
p = prop.table(table( tdf$BIRTH.WEIGHT, tdf$GESTATION),2)*100
chisq.test(p)  # p-val = 1e-11 YAHOO!


# a) between "42+ wks" and "<37 wks"
tdf = subset(traindf, (GESTATION == "UNK" | 
                         GESTATION == "POSTTERM 42+ WKS"))
p = prop.table(table( tdf$BIRTH.WEIGHT, tdf$GESTATION),2)*100
chisq.test(p)  # p-val = 2e-11 YAHOO!


# overall we see that the groups POSTTERM 42+ WKS and TERM 37-41 WKS are similar.


# 3) Relation between birth_weight and multiple-babies
chisq.test(BIRTH.WEIGHT, PLURALITY)
# P-val = 2e-16. From table below we see that only 44.77% of multiples are likely to be
# 2500 gms or more, compared to 93.09% of singleton babies.
#           PLURAL SINGLE
# <1500     12.586  1.428
# 1500-2499 42.644  5.479
# 2500+     44.770 93.094


# 3) Relation between birth_weight and gender
chisq.test(BIRTH.WEIGHT, GENDER)
# but post-hoc tests are not statistically significant! 



# ========================================================================
# Step 2 - Model Creation
# ========================================================================
library(MASS)  # for using the formula for linear discriminant analysis i.e lda()
# and quadratic discriminant analysis i.e qda()
gmodel = lda(BIRTH.WEIGHT ~  MOM.s.RACE + GESTATION + PLURALITY ,
             data = traindf)
gmodel

# make predictions for test1
ldaprediction = predict(gmodel, test1)
newvals = ldaprediction$class
chkvalid = data.frame( new = newvals, old = test1$BIRTH.WEIGHT)
table(chkvalid$new, chkvalid$old)
# 86.96% correct predictions for test1

# make predictions for test2
ldaprediction = predict(gmodel, test2)
newvals = ldaprediction$class
chkvalid = data.frame( new = newvals, old = test2$BIRTH.WEIGHT)
table(chkvalid$new, chkvalid$old)
# 88.4% correct predictions for test2





# ======================= model 2 ==============================
ldamodel = lda(BIRTH.WEIGHT ~  MOM.s.RACE + GESTATION + PLURALITY +
                 MOM.S.AGE + START.OF.PRENATAL.CARE + MOM.S.MARITAL.STATUS +
                 MODE.OF.DELIVERY ,
             data = traindf)
#ldamodel

# make predictions for test1
ldaprediction2 = predict(ldamodel, test1)
newvals2 = ldaprediction2$class
# check how good our predictions are:
chkvalid2 = data.frame( new = newvals2, old = test1$BIRTH.WEIGHT)
table(chkvalid2$new, chkvalid2$old)
# 87.6% correct predictions.


# make predictions for test2
ldaprediction2 = predict(ldamodel, test2)
newvals2 = ldaprediction2$class
# check how good our predictions are:
chkvalid2 = data.frame( new = newvals2, old = test2$BIRTH.WEIGHT)
table(chkvalid2$new, chkvalid2$old)
# 88.57% correct predictions.




# ================== model 3 ====================
traindf$bw = as.integer(as.factor(traindf$BIRTH.WEIGHT))
test1$bw = as.integer(as.factor(test1$BIRTH.WEIGHT))
test2$bw = as.integer(as.factor(test2$BIRTH.WEIGHT))


library(tree)
tree1 <- tree(bw ~  MOM.s.RACE + GESTATION + PLURALITY + MOM.S.EDUCATION +
                MOM.S.AGE + START.OF.PRENATAL.CARE + MOM.S.MARITAL.STATUS +
                MODE.OF.DELIVERY , data = traindf)

# predict for test1
Pred1 <- predict(tree1, test1)
chkdf = data.frame(new = Pred1, old = test1$bw)
chkdf$new1 = round(chkdf$new, digits = 0)
table(chkdf$new1, chkdf$old)
# 91.16% correct.

# predict for test2
Pred1 <- predict(tree1, test2)
chkdf = data.frame(new = Pred1, old = test2$bw)
chkdf$new1 = round(chkdf$new, digits = 0)
table(chkdf$new1, chkdf$old)
# 91.6% correct.





# traindf$bw = as.factor(traindf$BIRTH.WEIGHT)
# test1$bw = as.factor(test1$BIRTH.WEIGHT)
# test2$bw = as.factor(test2$BIRTH.WEIGHT)
# 
# tree1 <- tree(bw ~  MOM.s.RACE + GESTATION + PLURALITY + START.OF.PRENATAL.CARE ,
#               data = traindf)
# 
# pred2 = predict(tree1, test1, type = "class")
# chkdf = data.frame(new = pred2, old = test1$bw)
# table(chkdf$new, chkdf$old)
# # 91.6% correct, same as above.





# ================== model 4 ====================
library(e1071)  # for using naive-bayes algorithm
traindf$bw = as.factor(traindf$BIRTH.WEIGHT)

fit2 <- naiveBayes(bw ~  MOM.s.RACE + GESTATION + PLURALITY +
                     MOM.S.AGE + START.OF.PRENATAL.CARE + MOM.S.MARITAL.STATUS +
                     MODE.OF.DELIVERY ,
                   data = traindf)
summary(fit2)


test1$bw = as.factor(test1$BIRTH.WEIGHT)
test2$bw = as.factor(test2$BIRTH.WEIGHT)


# predict for test1:
x = test1[,c("MOM.s.RACE" , "GESTATION" , "PLURALITY" ,
              "MOM.S.AGE" , "START.OF.PRENATAL.CARE"
              , "MOM.S.MARITAL.STATUS" , "MODE.OF.DELIVERY")]
prednb <- predict(fit2, x)
xdf = data.frame(new = prednb, old = test1$bw)
table(xdf$new, xdf$old)
# 91.16% correct. simply marked everything as "2500+"


# predict for test1:
x = test2[,c("MOM.s.RACE" , "GESTATION" , "PLURALITY" ,
             "MOM.S.AGE" , "START.OF.PRENATAL.CARE"
             , "MOM.S.MARITAL.STATUS" , "MODE.OF.DELIVERY")]
prednb <- predict(fit2, x)
xdf = data.frame(new = prednb, old = test2$bw)
table(xdf$new, xdf$old)
# 91.6% correct. simply marked everything as "2500+"


# Naive-Bayes and trees work in the same way, and mark everything as the "dominant" factor!






# ================== model 5 ====================

library(nnet) # for using neural network algo
set.seed(270)
fit2n <- nnet(bw ~  MOM.s.RACE + GESTATION + PLURALITY +
                     MOM.S.AGE + START.OF.PRENATAL.CARE + MOM.S.MARITAL.STATUS +
                     MODE.OF.DELIVERY , data = traindf,
             size=4, decay=0.0001, maxit=500)
summary(fit2n)



# predict for test1:
x = test1[,c("MOM.s.RACE" , "GESTATION" , "PLURALITY" ,
             "MOM.S.AGE" , "START.OF.PRENATAL.CARE"
             , "MOM.S.MARITAL.STATUS" , "MODE.OF.DELIVERY")]
prednn <- predict(fit2n, x, type = "class")
xndf = data.frame(new = prednn, old = test1$bw)
table(xndf$new, xndf$old)
# 91.84% correct data


# predict for test2:
x = test2[,c("MOM.s.RACE" , "GESTATION" , "PLURALITY" ,
             "MOM.S.AGE" , "START.OF.PRENATAL.CARE"
             , "MOM.S.MARITAL.STATUS" , "MODE.OF.DELIVERY")]
prednn <- predict(fit2n, x, type = "class")
xndf = data.frame(new = prednn, old = test2$bw)
table(xndf$new, xndf$old)
# 92.57% correct data


# adding too many variables can be detrimental!
# for example , adding variables like "PLACE.OF.BIRTH" &  "MOM.S.EDUCATION" reduces 
# the sensitivity of the model and we ended with around ~91% correct predictions,
# insted of 92%



