

```{r message=FALSE}
library(ggplot2)
library(plotly)
library(tidyr)
library(reshape2)
library(cowplot)
library(ggExtra)
library(ggcorrplot)
library(DataExplorer)
library(tidyr) #to seperate data into several columns
library(scales) #for percentages
library(gtable)
library(gridExtra)
library(grid)
library(MASS) #for LDA model fitting
library(rockchalk) #combine factor levels
library(knitr)
library(kableExtra)
library(randomForest)
library(e1071)
```
#Introduction & Executive Summary
This data set was donated by Prof. Hofmann and is available at the [UCI Machine Learning Respitory](https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)). 

> Given various categorical and numerical features, the aim is to build a predictive model that determines whether a bank loan applicant is a good or bad credit risk in order to assist banks in decision making when confronted with a loan applicant. These decisions are translated as profit consideration for the banks
The associated risks are as follows:

* If an applicant is credit worthy, then denying a loan application results in a loss in business profit. 
* If an applicant is not credit worthy, then approving the loan application results in financial loss. 

We assume a correct bank decision leads to 35% profit. Otherwise, if bank decises an applicant is credit worthy when in reality they are not, then the loss is 100%. Hence, the second risk has more weight than the first.

Source: Dua, D. and Karra Taniskidou, E. (2017). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

#User-Defined Function
*cost_profit_cal* This function calculates the profit per applicant based on the formula [True Positive]x.35 + [False Positive] x-1
The argument takes a two element vector first first elemenet one = True Positive and element two = False Positive

*create.plots* Function takes in a data frame and a plotting function as their arguments and uses lapply() to create a list of plots from a specifiec data frame. In addition, a for loop is used to paste the corresponding variable name from the data to the elements of plot list. The need for this function was neccessary in order to easily plot frequency distributions, histograms, dodged bars, etc of all variables at once and in a single page. This saves time meanwhile allowing me to analyze many variables at once with ease. 



```{r}
 
cost_profit_cal = function(x){
  profit = (x[1] * .35) + (-1*x[2]) #calculates profit
  row = c(x,profit) #appends to TN and FN vector
  return(row)
}



#function takes a data frame and a plot function as its arguments and 
#returns a list of ggplots with their corresponding variable name pasted. 
create.plots = function(data.frame, plot.function){
  
plot.list = lapply(data.frame, function(i) plot.function(i) ) #creates a list of plots

plots = list() #empty list to save updated plots
j=0
for( i in plot.list){
  j=j+1
  i = i + labs(x = names(plot.list)[j]) #paste corresponding names to each plot
  plots[[j]] = i #adds plot to list
}
return(plots)
}
```
#Importing & Preparing Data Set

To begin with, I'll import the data file from UCI Machine Learning Respitory. From prior inspection, we note that the data is in .data file type with columns seperated by spaces. I will also change column names to get a better representation of the columns. 
```{r}
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
df = read.table(url, sep = " ") #reads .data files


attr_info = 
"Attribute 1: (qualitative) 
StatusCheckingAcc
A11 : ... < 0 DM 
A12 : 0 <= ... < 200 DM 
A13 : ... >= 200 DM 
A14 : no checking account 

Attribute 2: (numerical) 
Duration

Attribute 3: (qualitative) 
CreditHistory
A30 : all credits paid back duly 
A31 : all credits at this bank paid back duly 
A32 : existing credits paid back duly till now 
A33 : delay in paying off in the past 
A34 : credits existing (not at this bank) 

Attribute 4: (qualitative) 
Purpose
A40 : car (new) 
A41 : car (used) 
A42 : furniture/equipment 
A43 : radio/television 
A44 : domestic appliances 
A45 : repairs 
A46 : education 
A47 : vacation 
A48 : retraining 
A49 : business 
A410 : others 

Attribute 5: (numerical) 
CreditAmount

Attribute 6: (qualitative) 
SavingsAccount
A61 : ... < 100 DM 
A62 : 100 <= ... < 500 DM 
A63 : 500 <= ... < 1000 DM 
A64 : .. >= 1000 DM 
A65 : no savings

Attribute 7: (qualitative) 
EmployedSince
A71 : unemployed 
A72 : ... < 1 year 
A73 : 1 <= ... < 4 years 
A74 : 4 <= ... < 7 years 
A75 : .. >= 7 years 

Attribute 8: (numerical) 
InstallmentRate

Attribute 9: (qualitative) 
SexPersonalStatus
A91 : male - divorced/separated 
A92 : female - divorced/separated/married 
A93 : male - single 
A94 : male - married/widowed 
A95 : female - single 

Attribute 10: (qualitative) 
OtherDebtors
A101 : none 
A102 : co-applicant 
A103 : guarantor 

Attribute 11: (numerical) 
PresentResidenceSince

Attribute 12: (qualitative) 
Property
A121 : real estate 
A122 : building  
A123 : car or other, not in attribute 6 
A124 : no property 

Attribute 13: (numerical) 
Age

Attribute 14: (qualitative) 
OtherInstallments
A141 : bank 
A142 : stores 
A143 : none 

Attribute 15: (qualitative) 
Housing
A151 : rent 
A152 : own 
A153 : for free 

Attribute 16: (numerical) 
NumberOfExistingCredits

Attribute 17: (qualitative) 
Job
A171 : unemployed/ unskilled 
A172 : unskilled - resident 
A173 : skilled employee / official 
A174 : management/ self-employed/ 
highly qualified employee/ officer 

Attribute 18: (numerical) 
NumberOfPeopleLiable

Attribute 19: (qualitative) 
Telephone
A191 : none 
A192 : yes 

Attribute 20: (qualitative) 
ForeignWorker
A201 : yes 
A202 : no 

Attribute 21: (quantitative)
GoodCredit
1: Good
2: Bad
"

attr_list = as.list(as.vector(strsplit(attr_info, "Attribute ")[[1]])[2:22])

attr_names = sapply(attr_list, function(attr_desc) strsplit(attr_desc, "\n")[[1]][2])

colnames(df) = attr_names

```
#Data Exploration


##Variable Identification

Let's check the first 6 rows of our data, along with its dimensions and data type of our variables.  
```{r}
dim(df); str(df); head(df)
```
This data set consists of 1000 rows and 21 columns. The first 20 are the predictors and the the last is the target variable which is a numerical data type. Being a binary classification problem, I will turn this variable into a factor. 

Furthermore, note that most of the columns are categorical predictors (13 of them) with several levels. In addition, these levels have been encoded into symbolic characters. Personally, before analysing data with various visual techniques, I prefer to have the factor levels with descriptive names for better interpretability. Hence, I will change the factor names as described in the German data set description, before proceeding any further.  
```{r}

for(i in 1:21){
  attr_desc = attr_list[[i]]
  if(length(grep("qualitative",attr_desc))>0){
    lines_all = as.vector(strsplit(attr_desc, "\n")[[1]])
    lines = lines_all[3:(length(lines_all)-1)]
    str_vals = sapply(lines, function(line) strsplit(line, " : ")[[1]][2])
    df[,i] = as.numeric(df[,i])
    
    for (val_i in 1:length(lines)){
      df[,i] <- ifelse(df[,i]==val_i, str_vals[val_i], df[,i])
    }
  }
}
```
Next, I will subset each predictor based on variable category and check the proportions of the target variable. 
```{r}
numerical = c(2,5,8,11,13,16,18)
factorials = setdiff(1:21,numerical)
df.cat = df[,factorials]  #categorical subset
df.num = df[,numerical] #numerical subset


prop.table(table(df.cat$GoodCredit)) #checking target level propotions
```
It seems that the target variable is somewhat imbalanced, with the majority class (credible applicants) having a 70% proportion and the minority class with 30%. This will be important to keep in mind since missclassifying a non-credit worthy applicant has a bigger penalty than missclassifying a credit worthy applicant. 



##Univariate Analysis
As previosly stated, the majority of the predictors are categorical variables with several levels. This may be a problem since a full cross-classification of all variables may lead to zero observations in many cells. Therefore, we need to eliminate the variables that have little influence. In addition, we can combine categorical levels on imbalanced predictors, where a level has a frequency less than 5%. To do this, lets get an idea of the distributions of factor levels in each categorical veriable. I will do this by plotting bar charts. 
```{r}
bar_chart = function(i){
  ggplot(df.cat, aes(x = i)) + 
          geom_bar(fill = "lightblue",stat = "count" ,aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent) + ylab("rel frequencies") + theme(axis.text.x = element_text(angle=25, vjust=0.6)) 
}

bar.charts = create.plots(df.cat, bar_chart)

do.call(grid.arrange,bar.charts[1:4] )
do.call(grid.arrange,bar.charts[5:8])
do.call(grid.arrange,bar.charts[8:11])
do.call(grid.arrange,bar.charts[12:14])

#(lapply(df.cat, function(x) margin.table(prop.table(table(x)),1 ))) 

```
Analysis of imbalanced predictors from bar plot and proportion table results:

1. $AccountStatus: 
Only 6.3% of applicant have an account balance above 200 DM (about 120 US dollars ). We can merge this with applicants that have a balance of 0 to 200 DM and name the new level as "some balance"

2. $CreditHistory:


3. $Purpose:
Most individuals tend to apply to credit for their car payments and furniture. We can generalize the furniture, radio/television, repairs, and domestic into a "home related" purpose. Education, vacation, and retraining make up less than 1% each, we can include it into an "others" level and include business along too.

3. $Account:
It seems most people have less than 100 DM (about 60 dollars) in their savings account. We can group 
individuals with savings from 100 to above 1000.

4. $EmploymentSince:
only 6.2% of individuals are unemployed, while other levels contain > 17%. We can bin unemployment and
below 1 year into a single level.

5. $PersonalStatus:
divorced males make up 5% of applicants, we may bin this with the single males.

6. $OtherDebtors:
guarantors and co applicant levels can be bined together to create a "yes" or "no" levels.

7. $OtherInstallPlans:
We can merge bank and store credit installments to make a new level since stores only have a 4.7% frequency. Either you have concurrent credit or you dont. 

8. $Job:
unskilled and unemployed/nonresident can be generalized to simply unskilled. The new levels can then become
unskilled/unemployed, skilled, and highly skilled/management. 

9. $ForeignWorker:
Only 3.7% of applicants are not foreign workers. Since there are only two levels, we can potentially eliminate this variable all together. 

Now for continuos variables, it is important to understand the spread and central tendency of the variables. 
```{r}
#function plots a histogram given a data frame column
histogram = function(i){
  ggplot(df.num, aes(x =i  )) + geom_histogram(fill = "lightblue", color= "black", bins = 10) 
}


histo.plots = create.plots(df.num, histogram)


do.call(grid.arrange,lapply(histo.plots[1:4], function(x) ggMarginal(x, type = "boxplot", fill="transparent", margins = "y") ))
do.call(grid.arrange,lapply(histo.plots[4:7], function(x) ggMarginal(x, type = "boxplot", fill="transparent", margins = "y") ))


```

##Bi-variate Analysis 
```{r}
require(grid)
bar = function(i){
   ggplot(df.cat, aes(x=i,group = GoodCredit ,fill=factor(GoodCredit))) +
   geom_bar(position="dodge" ,color="black", stat = "count", aes(y = ..prop..))+theme(axis.text.x = element_text(angle=10, vjust=.6)) +geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count")+scale_fill_brewer(palette = "Pastel1")
}

dodged.bars = create.plots(df.cat, bar)

do.call(grid.arrange,dodged.bars[1:2])


```
continuos variables



#Data Engineering

```{r results="hide"}


eng.df.cat = df.cat
eng.df.cat[] = lapply(eng.df.cat, factor)

eng.df.cat$StatusCheckingAcc = combineLevels(eng.df.cat$StatusCheckingAcc,levs = c("... >= 200 DM ","0 <= ... < 200 DM "), newLabel = c("Some Balance") )


eng.df.cat$Purpose = combineLevels(eng.df.cat$Purpose, levs= c("domestic appliances ",  "radio/television ","furniture/equipment "), newLabel = c("Home Related"))
eng.df.cat$Purpose = combineLevels(eng.df.cat$Purpose, levs=c("business " ,"repairs ","education ","vacation ","retraining "), newLabel = c("Other"))

eng.df.cat$SavingsAccount = combineLevels(eng.df.cat$SavingsAccount, levs = c(".. >= 1000 DM ","500 <= ... < 1000 DM "), newLabel = c(">= 500"))

eng.df.cat$EmployedSince = combineLevels(eng.df.cat$EmployedSince, levs = c("... < 1 year ", "unemployed "), newLabel = c("<1 or unemployed"))

eng.df.cat$SexPersonalStatus = combineLevels(eng.df.cat$SexPersonalStatus, levs = c("male - divorced/separated ","male - single " ), newLabel = "male - single/divorced" )

levels(eng.df.cat$OtherDebtors)
eng.df.cat$OtherDebtors =combineLevels( eng.df.cat$OtherDebtors, levs = c("guarantor ","co-applicant " ), newLabel = "yes")

eng.df.cat$OtherInstallments = combineLevels(eng.df.cat$OtherInstallments, levs = c("bank ","stores "), newLabel = c("bank/stores"))

eng.df.cat$Job = combineLevels(eng.df.cat$Job,levs = c("unemployed/ unskilled ", "unskilled - resident "), newLabel = c("unemployed/unskilled") )

eng.df.cat$CreditHistory = combineLevels(eng.df.cat$CreditHistory, levs = c("all credits at this bank paid back duly " ,"all credits paid back duly " ), newLabel = c("Paid"))

#sapply(eng.df.cat, function(x) margin.table(prop.table(table(x)),1) )

```

```{r}
eng.df.cat$ForeignWorker = NULL #drops the frgn column
```
#Machine Learning
It is time to fit our data with various statistical models. I will first recombine our numerical and categorical features to split the full data into two equal sets: (1) Training set that will serve to fit various statistical models and (2)Validation set that wll serve to test the accuracy of each model. 
```{r}

data = cbind(eng.df.cat,df.num)
set.seed(1)

train = sample(1:nrow(data), size = nrow(data)*.50 )
data.train = data[train,]
data.test = data[-train,]

GoodCreditity.test = data.test$GoodCredit


```
## Logistic Regression
```{r}
#Base Model
logistic.fit = glm(GoodCredit~., data = data, subset = train, family = binomial)
sig.level =data.frame(summary(logistic.fit)$coef)
sig.level


#Model 3
logistic.fit3 = glm(GoodCredit~. -SexPersonalStatus -Telephone  -NumberOfPeopleLiable -Job -Housing -PresentResidenceSince -CreditAmount, data = data, subset = train, family = binomial)

 
logistic.probs  = predict(logistic.fit3, data.test, type = "response")
#30% threshold
glm.predict1 = rep("credible", 500)
glm.predict1[logistic.probs > .30] = "non.credible"
#50% threshold
glm.predict2 = rep("credible", 500)
glm.predict2[logistic.probs > .50] = "non.credible"


glm.TN.FP1 = table(GoodCreditity.test, glm.predict1)[,1]/500
glm.TN.FP2 = table(GoodCreditity.test, glm.predict2)[,1]/500

```
##linear Discriminant Analysis

LDA assumes a multivariate normal distribution, hence we only use predictors that are either ordinal or continuos.
```{r}
library(MASS)
lda.fit = lda(GoodCredit~SavingsAccount+EmployedSince+Duration+OtherInstallments+InstallmentRate+NumberOfExistingCredits+Age+CreditAmount ,data=data, subset=train)

lda.pred = predict(lda.fit, data.test)
lda.TP.FP = table(GoodCreditity.test,lda.pred$class)[,1]/500

lda.fit2 = lda(GoodCredit~. ,data=data, subset=train)
lda.pred2 = predict(lda.fit2, data.test)
classification = lda.pred2$class
lda.TP.FP2 = table(GoodCreditity.test, classification)[,1]/500


#qda
qda.fit = qda(GoodCredit~., data=data, subset=train)
qda.pred = predict(qda.fit, data.test)
qda.TP.FP = table(GoodCreditity.test, qda.pred$class)[,1]/500
```
##Tree Based Methods
```{r}

library(tree)
tree.cred = tree(GoodCredit~., data, subset = train)
tree.pred = predict(tree.cred, data.test, type="class")
tree.TP.FP = table(GoodCreditity.test, tree.pred)[,1]/500

#summary(tree.cred)

```
###Prunning the tree
```{r}
set.seed(3)
cv.data = cv.tree(tree.cred, FUN = prune.misclass)
best.size = cv.data$size[which.min(cv.data$dev)]

prune.data = prune.misclass(tree.cred, best = best.size)
tree.pred2 = predict(prune.data, data.test, type="class")
tree.TP.FP2 = table(GoodCreditity.test, tree.pred2)[,1]/500


```
###Random Forest
```{r}

rf.data = randomForest(GoodCredit~., data= data , subset= train, mtry = 6, importance =T)
rf.predict = predict(rf.data, data.test)
Forest.TP.FP = table(GoodCreditity.test, rf.predict)[,1]/500

```
##SMC and SVM
```{r}

#svm.fit = svm(GoodCredit~., data = data.train, kernel = "linear", cost = 10, scale =FALSE)
tune.out = tune(svm, GoodCredit~., data = data.train, kernel = "linear", ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
bestmod = tune.out$best.model
#summary(bestmod)

svc.predict = predict(bestmod, data.test)
svc.TP.FP = table( GoodCreditity.test, svc.predict)[,1]/500




#radial kernel
svm.fit = svm(GoodCredit~., data = data.train, kernel = "radial", gamma = 1, cost = 1)
tune.out=tune(svm, GoodCredit~., data=data.train, kernel="radial",cranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4) ))
bestmod = tune.out$best.model

svm.predict = predict(bestmod,data.test) 
svm.TP.FP = table(GoodCreditity.test, svm.predict)[,1]/500




```
#Summary/ Cost-Profit Consideration
```{r}
cross.profit = data.frame(matrix(ncol = 9, nrow = 3))
rownames(cross.profit) = c("Credible", "Non-Credible", "Per Applicant Profit")


cross.profit$X1 = cost_profit_cal(glm.TN.FP1)
cross.profit$X2 = cost_profit_cal(glm.TN.FP2)
cross.profit$X3 = cost_profit_cal(lda.TP.FP2)
cross.profit$X4 = cost_profit_cal(qda.TP.FP)
cross.profit$X5 = cost_profit_cal(tree.TP.FP)
cross.profit$X6 = cost_profit_cal(tree.TP.FP2)
cross.profit$X7 = cost_profit_cal(Forest.TP.FP)
cross.profit$X8 = cost_profit_cal(svc.TP.FP)
cross.profit$X9 = cost_profit_cal(svm.TP.FP)



cross.profit %>%
  kable() %>%
  kable_styling() %>%
  add_header_above(c("Test ", "30% threshold" , "50% threshold", "Linear", "quadratic","Unpruned Tree", "Pruned Tree","Random Forest", "SVC", "SVM")) %>%
  add_header_above(c( "", "Logistic" = 2, "Discriminant Analysis" = 2, "Trees" = 2, "" , "","" ))
  
```

In conclusion, Logistic Regression at a 30% threshold turned out to have the biggest profit per applicant. 
