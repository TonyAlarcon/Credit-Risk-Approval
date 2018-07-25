Credit Risk Approval (German Dataset)
================
Pedro Alarcon
July 22, 2018

Introduction
============

This data set was donated by Prof. Hofmann and is available at the [UCI Machine Learning Respitory](https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)).

> Given various categorical and numerical features, the aim is to build a predictive model that determines whether a bank loan applicant is a good or bad credit risk in order to assist banks in decision making when confronted with a loan application. These decisions are translated as cost-profit consideration for the banks. The associated risks are as follows:

-   If an applicant is credit worthy, then denying a loan application results in a loss in business profit.
-   If an applicant is not credit worthy, then approving the loan application results in financial loss.

We assume a correct bank decision leads to 35% profit. Otherwise, if bank decides an applicant is credit worthy when in reality they are not, then the loss is 100%. Hence, the second risk has more weight than the first. Cost-Profit considerations will be assed by the following formula:

-   \[True Positive\] × .35 + \[False Positive\] × − 1

where true positive and false positive are elements of the confusion matrix.

Source: Dua, D. and Karra Taniskidou, E. (2017). UCI Machine Learning Repository \[<http://archive.ics.uci.edu/ml>\]. Irvine, CA: University of California, School of Information and Computer Science.

User-Defined Function
=====================

*cost\_profit\_cal()*: This function calculates the profit per applicant based on the formula \[True Positive\]x.35 + \[False Positive\] x-1 The argument takes a two element vector first first element one = True Positive and element two = False Positive

*create.plots()*: Function takes in a data frame and a plotting function as their arguments and uses lapply() to create a list of plots from a specific data frame. In addition, a for loop is used to paste the corresponding variable name from the data to the elements of plot list. The need for this function was necessary in order to easily plot frequency distributions, histograms, dodged bars, etc of all variables at once and in a single page. This saves time meanwhile allowing me to analyze many variables at once with ease.

``` r
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

Importing & Preparing Data Set
==============================

To begin with, I'll import the data file from UCI Machine Learning Repository. From prior inspection, we note that the data is in .data file type with columns separated by spaces. I will also change column names to get a better representation of the columns.

``` r
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

``` r
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
df = read.table(url, sep = " ") #reads .data files



attr_list = as.list(as.vector(strsplit(attr_info, "Attribute ")[[1]])[2:22]) #obtains attribute information 

attr_names = sapply(attr_list, function(attr_desc) strsplit(attr_desc, "\n")[[1]][2])

colnames(df) = attr_names
```

Data Exploration
================

Variable Identification
-----------------------

Let's view our data columns/rows, along with its dimensions and data type of our variables.

``` r
dim(df); str(df)
```

    ## [1] 1000   21

    ## 'data.frame':    1000 obs. of  21 variables:
    ##  $ StatusCheckingAcc      : Factor w/ 4 levels "A11","A12","A13",..: 1 2 4 1 1 4 4 2 4 2 ...
    ##  $ Duration               : int  6 48 12 42 24 36 24 36 12 30 ...
    ##  $ CreditHistory          : Factor w/ 5 levels "A30","A31","A32",..: 5 3 5 3 4 3 3 3 3 5 ...
    ##  $ Purpose                : Factor w/ 10 levels "A40","A41","A410",..: 5 5 8 4 1 8 4 2 5 1 ...
    ##  $ CreditAmount           : int  1169 5951 2096 7882 4870 9055 2835 6948 3059 5234 ...
    ##  $ SavingsAccount         : Factor w/ 5 levels "A61","A62","A63",..: 5 1 1 1 1 5 3 1 4 1 ...
    ##  $ EmployedSince          : Factor w/ 5 levels "A71","A72","A73",..: 5 3 4 4 3 3 5 3 4 1 ...
    ##  $ InstallmentRate        : int  4 2 2 2 3 2 3 2 2 4 ...
    ##  $ SexPersonalStatus      : Factor w/ 4 levels "A91","A92","A93",..: 3 2 3 3 3 3 3 3 1 4 ...
    ##  $ OtherDebtors           : Factor w/ 3 levels "A101","A102",..: 1 1 1 3 1 1 1 1 1 1 ...
    ##  $ PresentResidenceSince  : int  4 2 3 4 4 4 4 2 4 2 ...
    ##  $ Property               : Factor w/ 4 levels "A121","A122",..: 1 1 1 2 4 4 2 3 1 3 ...
    ##  $ Age                    : int  67 22 49 45 53 35 53 35 61 28 ...
    ##  $ OtherInstallments      : Factor w/ 3 levels "A141","A142",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ Housing                : Factor w/ 3 levels "A151","A152",..: 2 2 2 3 3 3 2 1 2 2 ...
    ##  $ NumberOfExistingCredits: int  2 1 1 1 2 1 1 1 1 2 ...
    ##  $ Job                    : Factor w/ 4 levels "A171","A172",..: 3 3 2 3 3 2 3 4 2 4 ...
    ##  $ NumberOfPeopleLiable   : int  1 1 2 2 2 2 1 1 1 1 ...
    ##  $ Telephone              : Factor w/ 2 levels "A191","A192": 2 1 1 1 1 2 1 2 1 1 ...
    ##  $ ForeignWorker          : Factor w/ 2 levels "A201","A202": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ GoodCredit             : int  1 2 1 1 2 1 1 1 1 2 ...

``` r
kable(df[1:6,]) %>%
kable_styling() 
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
StatusCheckingAcc
</th>
<th style="text-align:right;">
Duration
</th>
<th style="text-align:left;">
CreditHistory
</th>
<th style="text-align:left;">
Purpose
</th>
<th style="text-align:right;">
CreditAmount
</th>
<th style="text-align:left;">
SavingsAccount
</th>
<th style="text-align:left;">
EmployedSince
</th>
<th style="text-align:right;">
InstallmentRate
</th>
<th style="text-align:left;">
SexPersonalStatus
</th>
<th style="text-align:left;">
OtherDebtors
</th>
<th style="text-align:right;">
PresentResidenceSince
</th>
<th style="text-align:left;">
Property
</th>
<th style="text-align:right;">
Age
</th>
<th style="text-align:left;">
OtherInstallments
</th>
<th style="text-align:left;">
Housing
</th>
<th style="text-align:right;">
NumberOfExistingCredits
</th>
<th style="text-align:left;">
Job
</th>
<th style="text-align:right;">
NumberOfPeopleLiable
</th>
<th style="text-align:left;">
Telephone
</th>
<th style="text-align:left;">
ForeignWorker
</th>
<th style="text-align:right;">
GoodCredit
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
A11
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
A34
</td>
<td style="text-align:left;">
A43
</td>
<td style="text-align:right;">
1169
</td>
<td style="text-align:left;">
A65
</td>
<td style="text-align:left;">
A75
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
A93
</td>
<td style="text-align:left;">
A101
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
A121
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:left;">
A143
</td>
<td style="text-align:left;">
A152
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
A173
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
A192
</td>
<td style="text-align:left;">
A201
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
A12
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
A32
</td>
<td style="text-align:left;">
A43
</td>
<td style="text-align:right;">
5951
</td>
<td style="text-align:left;">
A61
</td>
<td style="text-align:left;">
A73
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
A92
</td>
<td style="text-align:left;">
A101
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
A121
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
A143
</td>
<td style="text-align:left;">
A152
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
A173
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
A191
</td>
<td style="text-align:left;">
A201
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
A14
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
A34
</td>
<td style="text-align:left;">
A46
</td>
<td style="text-align:right;">
2096
</td>
<td style="text-align:left;">
A61
</td>
<td style="text-align:left;">
A74
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
A93
</td>
<td style="text-align:left;">
A101
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
A121
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:left;">
A143
</td>
<td style="text-align:left;">
A152
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
A172
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
A191
</td>
<td style="text-align:left;">
A201
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
A11
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:left;">
A32
</td>
<td style="text-align:left;">
A42
</td>
<td style="text-align:right;">
7882
</td>
<td style="text-align:left;">
A61
</td>
<td style="text-align:left;">
A74
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
A93
</td>
<td style="text-align:left;">
A103
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
A122
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
A143
</td>
<td style="text-align:left;">
A153
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
A173
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
A191
</td>
<td style="text-align:left;">
A201
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
A11
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
A33
</td>
<td style="text-align:left;">
A40
</td>
<td style="text-align:right;">
4870
</td>
<td style="text-align:left;">
A61
</td>
<td style="text-align:left;">
A73
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
A93
</td>
<td style="text-align:left;">
A101
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
A124
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:left;">
A143
</td>
<td style="text-align:left;">
A153
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
A173
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
A191
</td>
<td style="text-align:left;">
A201
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
A14
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
A32
</td>
<td style="text-align:left;">
A46
</td>
<td style="text-align:right;">
9055
</td>
<td style="text-align:left;">
A65
</td>
<td style="text-align:left;">
A73
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
A93
</td>
<td style="text-align:left;">
A101
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
A124
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
A143
</td>
<td style="text-align:left;">
A153
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
A172
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
A192
</td>
<td style="text-align:left;">
A201
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>
This data set consists of 1000 rows and 21 columns. The first 20 are the predictors and the the last is the target variable which is a numerical data type. Being a binary classification problem, I will turn this variable into a factor.

Furthermore, note that most of the columns are categorical predictors (13 of them) with several levels. In addition, these levels have been encoded into symbolic characters. Personally, before analyzing data with various visual techniques, I prefer to have the factor levels with descriptive names for better interpretability. Hence, I will change the factor names as described in the attributeinfo.txt file before proceeding any further.

``` r
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

``` r
numerical = c(2,5,8,11,13,16,18)
factorials = setdiff(1:21,numerical)
df.cat = df[,factorials]  #categorical subset
df.num = df[,numerical] #numerical subset


prop.table(table(df.cat$GoodCredit)) #checking target level propotions
```

    ## 
    ##   1   2 
    ## 0.7 0.3

It seems that the target variable is somewhat imbalanced, with the majority class (credible applicants) having a 70% proportion and the minority class with 30%. This will be important to keep in mind since miss-classifying a non-credit worthy applicant has a bigger penalty than miss-classifying a credit worthy applicant.

Univariate Analysis
-------------------

As previously stated, the majority of the predictors are categorical variables with several levels. This may be a problem since a full cross-classification of all variables may lead to zero observations in many cells. Therefore, we need to eliminate the variables that have little influence. In addition, we can combine categorical levels on imbalanced predictors, where a level has a frequency less than 5%. To do this, lets get an idea of the distributions of factor levels in each categorical and discrete variables. I will do this by plotting bar charts.

``` r
bar_chart = function(i){
  ggplot(df.cat, aes(x = i)) + 
          geom_bar(fill = "lightblue",stat = "count" ,aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent) + ylab("rel frequencies") + theme(axis.text.x = element_text(angle=25, vjust=0.6)) 
}

bar.charts = create.plots(df.cat, bar_chart)

do.call(grid.arrange,bar.charts[1:4] )
```

![](credit_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
do.call(grid.arrange,bar.charts[5:8])
```

![](credit_files/figure-markdown_github/unnamed-chunk-8-2.png)

``` r
do.call(grid.arrange,bar.charts[8:11])
```

![](credit_files/figure-markdown_github/unnamed-chunk-8-3.png)

``` r
do.call(grid.arrange,bar.charts[12:14])
```

![](credit_files/figure-markdown_github/unnamed-chunk-8-4.png)

``` r
#(lapply(df.cat, function(x) margin.table(prop.table(table(x)),1 ))) #uncomment to obtain numerical values of proortions in each categorical variable
```

Analysis of imbalanced predictors from bar plot and proportion table results:

1.  $AccountStatus: Only 6.3% of applicant have an account balance above 200 DM (about 120 US dollars ). We can merge this with applicants that have a balance of 0 to 200 DM and name the new level as "some balance"

2.  $CreditHistory:

3.  $Purpose: Most individuals tend to apply to credit for their car payments and furniture. We can generalize the furniture, radio/television, repairs, and domestic into a "home related" purpose. Education, vacation, and retraining make up less than 1% each, we can include it into an "others" level and include business along too.

4.  $Account: It seems most people have less than 100 DM (about 60 dollars) in their savings account. We can group individuals with savings from 100 to above 1000.

5.  $EmploymentSince: only 6.2% of individuals are unemployed, while other levels contain &gt; 17%. We can bin unemployment and below 1 year into a single level.

6.  $PersonalStatus: divorced males make up 5% of applicants, we may bin this with the single males.

7.  $OtherDebtors: guarantors and co applicant levels can be binned together to create a "yes" or "no" levels.

8.  $OtherInstallPlans: We can merge bank and store credit installments to make a new level since stores only have a 4.7% frequency. Either you have concurrent credit or you don't.

9.  $Job: unskilled and unemployed/nonresident can be generalized to simply unskilled. The new levels can then become unskilled/unemployed, skilled, and highly skilled/management.

10. $ForeignWorker: Only 3.7% of applicants are not foreign workers. Since there are only two levels, we can potentially eliminate this variable all together.

Now for continuous variables, it is important to understand the spread and central tendency of the variables.

``` r
#function plots a histogram given a data frame column
histogram = function(i){
  ggplot(df.num, aes(x =i  )) + geom_histogram(fill = "lightblue", color= "black", bins = 10) 
}


histo.plots = create.plots(df.num, histogram)


do.call(grid.arrange,lapply(histo.plots[1:4], function(x) ggMarginal(x, type = "boxplot", fill="transparent", margins = "y") ))
```

![](credit_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
do.call(grid.arrange,lapply(histo.plots[4:7], function(x) ggMarginal(x, type = "boxplot", fill="transparent", margins = "y") ))
```

![](credit_files/figure-markdown_github/unnamed-chunk-9-2.png) \#\#Bi-variate Analysis In addition, let's visualize the proportion of each factor level that belongs to response levels.

``` r
require(grid)
bar = function(i){
   ggplot(df.cat, aes(x=i,group = factor(GoodCredit) ,fill=factor(GoodCredit))) +
   geom_bar(position="dodge" ,color="black", stat = "count", aes(y = ..prop..))+theme(axis.text.x = element_text(angle=10, vjust=.6)) +geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count")+scale_fill_brewer(palette = "Pastel1")
}

dodged.bars = create.plots(df.cat, bar)

do.call(grid.arrange,dodged.bars[1:2])
```

![](credit_files/figure-markdown_github/unnamed-chunk-10-1.png)

50% of the the observations that were classified as good credit risk belonged to the factor level having no checkings account, while if you had a checkings account but did not have any balance, then the majority were classified as bad credit risk. Such proportion leads me to think that this variable will have a big significance in predictibility of credit risk. The other only variable level with such disproportionateresponse is existing credits in other banks. Those who did were seen as more likely to be a good credit risk.

Data Engineering
================

As a result of analysis above, we will combine some factor levels that seems appropriate.

``` r
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

#sapply(eng.df.cat, function(x) margin.table(prop.table(table(x)),1) ) #new proportion of each variable
```

``` r
eng.df.cat$ForeignWorker = NULL #drops the frgn column
```

Machine Learning
================

It is time to fit our data with various statistical models. I will first recombine our numerical and categorical features to split the full data into two equal sets: (1)a training set that will serve to fit various statistical models and (2) a validation set that will serve to test the accuracy of each model.

``` r
data = cbind(eng.df.cat,df.num)
set.seed(1)

train = sample(1:nrow(data), size = nrow(data)*.50 )
data.train = data[train,]
data.test = data[-train,]

GoodCreditity.test = data.test$GoodCredit 
```

Logistic Regression
-------------------

We will first fit a logistic regression using all variables as a base model and eliminate those that do not posses a signigicant level.

``` r
#Base Model
logistic.fit = glm(GoodCredit~., data = data, subset = train, family = binomial)
sig.level =data.frame(summary(logistic.fit)$coef)

kable(sig.level) %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "200px")
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Estimate
</th>
<th style="text-align:right;">
Std..Error
</th>
<th style="text-align:right;">
z.value
</th>
<th style="text-align:right;">
Pr...z..
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
(Intercept)
</td>
<td style="text-align:right;">
-4.0224919
</td>
<td style="text-align:right;">
1.4319594
</td>
<td style="text-align:right;">
-2.8090823
</td>
<td style="text-align:right;">
0.0049683
</td>
</tr>
<tr>
<td style="text-align:left;">
StatusCheckingAccno checking account
</td>
<td style="text-align:right;">
-1.1774806
</td>
<td style="text-align:right;">
0.3274609
</td>
<td style="text-align:right;">
-3.5957899
</td>
<td style="text-align:right;">
0.0003234
</td>
</tr>
<tr>
<td style="text-align:left;">
StatusCheckingAccSome Balance
</td>
<td style="text-align:right;">
-0.2421332
</td>
<td style="text-align:right;">
0.3027951
</td>
<td style="text-align:right;">
-0.7996601
</td>
<td style="text-align:right;">
0.4239077
</td>
</tr>
<tr>
<td style="text-align:left;">
CreditHistorydelay in paying off in the past
</td>
<td style="text-align:right;">
0.8279743
</td>
<td style="text-align:right;">
0.4828560
</td>
<td style="text-align:right;">
1.7147437
</td>
<td style="text-align:right;">
0.0863922
</td>
</tr>
<tr>
<td style="text-align:left;">
CreditHistoryexisting credits paid back duly till now
</td>
<td style="text-align:right;">
1.5613483
</td>
<td style="text-align:right;">
0.3971152
</td>
<td style="text-align:right;">
3.9317266
</td>
<td style="text-align:right;">
0.0000843
</td>
</tr>
<tr>
<td style="text-align:left;">
CreditHistoryPaid
</td>
<td style="text-align:right;">
1.8154400
</td>
<td style="text-align:right;">
0.4851522
</td>
<td style="text-align:right;">
3.7420013
</td>
<td style="text-align:right;">
0.0001826
</td>
</tr>
<tr>
<td style="text-align:left;">
Purposecar (used)
</td>
<td style="text-align:right;">
-2.4483506
</td>
<td style="text-align:right;">
0.6077786
</td>
<td style="text-align:right;">
-4.0283594
</td>
<td style="text-align:right;">
0.0000562
</td>
</tr>
<tr>
<td style="text-align:left;">
PurposeHome Related
</td>
<td style="text-align:right;">
-0.9486702
</td>
<td style="text-align:right;">
0.3076256
</td>
<td style="text-align:right;">
-3.0838467
</td>
<td style="text-align:right;">
0.0020434
</td>
</tr>
<tr>
<td style="text-align:left;">
PurposeOther
</td>
<td style="text-align:right;">
-0.7228379
</td>
<td style="text-align:right;">
0.3686220
</td>
<td style="text-align:right;">
-1.9609191
</td>
<td style="text-align:right;">
0.0498885
</td>
</tr>
<tr>
<td style="text-align:left;">
SavingsAccount100 &lt;= ... &lt; 500 DM
</td>
<td style="text-align:right;">
-0.6143958
</td>
<td style="text-align:right;">
0.4134954
</td>
<td style="text-align:right;">
-1.4858588
</td>
<td style="text-align:right;">
0.1373165
</td>
</tr>
<tr>
<td style="text-align:left;">
SavingsAccountno savings
</td>
<td style="text-align:right;">
-1.0992783
</td>
<td style="text-align:right;">
0.3790153
</td>
<td style="text-align:right;">
-2.9003536
</td>
<td style="text-align:right;">
0.0037274
</td>
</tr>
<tr>
<td style="text-align:left;">
SavingsAccount&gt;= 500
</td>
<td style="text-align:right;">
-0.7287839
</td>
<td style="text-align:right;">
0.4483301
</td>
<td style="text-align:right;">
-1.6255522
</td>
<td style="text-align:right;">
0.1040449
</td>
</tr>
<tr>
<td style="text-align:left;">
EmployedSince1 &lt;= ... &lt; 4 years
</td>
<td style="text-align:right;">
0.2397797
</td>
<td style="text-align:right;">
0.3490028
</td>
<td style="text-align:right;">
0.6870423
</td>
<td style="text-align:right;">
0.4920560
</td>
</tr>
<tr>
<td style="text-align:left;">
EmployedSince4 &lt;= ... &lt; 7 years
</td>
<td style="text-align:right;">
-0.5411413
</td>
<td style="text-align:right;">
0.4459416
</td>
<td style="text-align:right;">
-1.2134802
</td>
<td style="text-align:right;">
0.2249463
</td>
</tr>
<tr>
<td style="text-align:left;">
EmployedSince&lt;1 or unemployed
</td>
<td style="text-align:right;">
0.4318977
</td>
<td style="text-align:right;">
0.3792139
</td>
<td style="text-align:right;">
1.1389288
</td>
<td style="text-align:right;">
0.2547329
</td>
</tr>
<tr>
<td style="text-align:left;">
SexPersonalStatusmale - married/widowed
</td>
<td style="text-align:right;">
-0.3532891
</td>
<td style="text-align:right;">
0.4601406
</td>
<td style="text-align:right;">
-0.7677851
</td>
<td style="text-align:right;">
0.4426149
</td>
</tr>
<tr>
<td style="text-align:left;">
SexPersonalStatusmale - single/divorced
</td>
<td style="text-align:right;">
-0.1185437
</td>
<td style="text-align:right;">
0.3105588
</td>
<td style="text-align:right;">
-0.3817111
</td>
<td style="text-align:right;">
0.7026756
</td>
</tr>
<tr>
<td style="text-align:left;">
OtherDebtorsyes
</td>
<td style="text-align:right;">
-0.6973300
</td>
<td style="text-align:right;">
0.4619844
</td>
<td style="text-align:right;">
-1.5094234
</td>
<td style="text-align:right;">
0.1311906
</td>
</tr>
<tr>
<td style="text-align:left;">
Propertycar or other, not in attribute 6
</td>
<td style="text-align:right;">
-0.2050620
</td>
<td style="text-align:right;">
0.3212607
</td>
<td style="text-align:right;">
-0.6383039
</td>
<td style="text-align:right;">
0.5232759
</td>
</tr>
<tr>
<td style="text-align:left;">
Propertyno property
</td>
<td style="text-align:right;">
0.6415387
</td>
<td style="text-align:right;">
0.5547211
</td>
<td style="text-align:right;">
1.1565067
</td>
<td style="text-align:right;">
0.2474740
</td>
</tr>
<tr>
<td style="text-align:left;">
Propertyreal estate
</td>
<td style="text-align:right;">
-0.5818103
</td>
<td style="text-align:right;">
0.3570635
</td>
<td style="text-align:right;">
-1.6294311
</td>
<td style="text-align:right;">
0.1032218
</td>
</tr>
<tr>
<td style="text-align:left;">
OtherInstallmentsbank/stores
</td>
<td style="text-align:right;">
0.9459467
</td>
<td style="text-align:right;">
0.2965678
</td>
<td style="text-align:right;">
3.1896475
</td>
<td style="text-align:right;">
0.0014245
</td>
</tr>
<tr>
<td style="text-align:left;">
Housingown
</td>
<td style="text-align:right;">
0.5354320
</td>
<td style="text-align:right;">
0.6205264
</td>
<td style="text-align:right;">
0.8628674
</td>
<td style="text-align:right;">
0.3882104
</td>
</tr>
<tr>
<td style="text-align:left;">
Housingrent
</td>
<td style="text-align:right;">
0.8317049
</td>
<td style="text-align:right;">
0.6625823
</td>
<td style="text-align:right;">
1.2552477
</td>
<td style="text-align:right;">
0.2093889
</td>
</tr>
<tr>
<td style="text-align:left;">
Jobskilled employee / official
</td>
<td style="text-align:right;">
0.4048622
</td>
<td style="text-align:right;">
0.4211094
</td>
<td style="text-align:right;">
0.9614181
</td>
<td style="text-align:right;">
0.3363420
</td>
</tr>
<tr>
<td style="text-align:left;">
Jobunemployed/unskilled
</td>
<td style="text-align:right;">
0.2346147
</td>
<td style="text-align:right;">
0.5003296
</td>
<td style="text-align:right;">
0.4689204
</td>
<td style="text-align:right;">
0.6391266
</td>
</tr>
<tr>
<td style="text-align:left;">
Telephoneyes
</td>
<td style="text-align:right;">
-0.1021899
</td>
<td style="text-align:right;">
0.2937343
</td>
<td style="text-align:right;">
-0.3478990
</td>
<td style="text-align:right;">
0.7279160
</td>
</tr>
<tr>
<td style="text-align:left;">
Duration
</td>
<td style="text-align:right;">
0.0383847
</td>
<td style="text-align:right;">
0.0139227
</td>
<td style="text-align:right;">
2.7569916
</td>
<td style="text-align:right;">
0.0058336
</td>
</tr>
<tr>
<td style="text-align:left;">
CreditAmount
</td>
<td style="text-align:right;">
0.0001020
</td>
<td style="text-align:right;">
0.0000741
</td>
<td style="text-align:right;">
1.3762157
</td>
<td style="text-align:right;">
0.1687549
</td>
</tr>
<tr>
<td style="text-align:left;">
InstallmentRate
</td>
<td style="text-align:right;">
0.2347496
</td>
<td style="text-align:right;">
0.1281782
</td>
<td style="text-align:right;">
1.8314306
</td>
<td style="text-align:right;">
0.0670363
</td>
</tr>
<tr>
<td style="text-align:left;">
PresentResidenceSince
</td>
<td style="text-align:right;">
0.1255694
</td>
<td style="text-align:right;">
0.1222451
</td>
<td style="text-align:right;">
1.0271938
</td>
<td style="text-align:right;">
0.3043292
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
-0.0161311
</td>
<td style="text-align:right;">
0.0126555
</td>
<td style="text-align:right;">
-1.2746381
</td>
<td style="text-align:right;">
0.2024374
</td>
</tr>
<tr>
<td style="text-align:left;">
NumberOfExistingCredits
</td>
<td style="text-align:right;">
0.6187392
</td>
<td style="text-align:right;">
0.2714515
</td>
<td style="text-align:right;">
2.2793725
</td>
<td style="text-align:right;">
0.0226449
</td>
</tr>
<tr>
<td style="text-align:left;">
NumberOfPeopleLiable
</td>
<td style="text-align:right;">
0.0937085
</td>
<td style="text-align:right;">
0.3554298
</td>
<td style="text-align:right;">
0.2636483
</td>
<td style="text-align:right;">
0.7920509
</td>
</tr>
</tbody>
</table>

``` r
#Model 2: only significant predictors
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

linear Discriminant Analysis
----------------------------

LDA assumes a multivariate normal distribution, hence we only use predictors that are either ordinal or continuous.

``` r
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

Tree Based Methods
------------------

``` r
library(tree)
tree.cred = tree(GoodCredit~., data, subset = train)
tree.pred = predict(tree.cred, data.test, type="class")
tree.TP.FP = table(GoodCreditity.test, tree.pred)[,1]/500

#summary(tree.cred)
```

### Prunning the tree

We will use cross-validation to check for the best size which minimizes the MSE to prune the tree

``` r
set.seed(3) 
cv.data = cv.tree(tree.cred, FUN = prune.misclass)
best.size = cv.data$size[which.min(cv.data$dev)]

prune.data = prune.misclass(tree.cred, best = best.size)
tree.pred2 = predict(prune.data, data.test, type="class")
tree.TP.FP2 = table(GoodCreditity.test, tree.pred2)[,1]/500
```

### Random Forest

``` r
rf.data = randomForest(GoodCredit~., data= data , subset= train, mtry = 6, importance =T)
rf.predict = predict(rf.data, data.test)
Forest.TP.FP = table(GoodCreditity.test, rf.predict)[,1]/500
```

SMC and SVM
-----------

``` r
tune.out = tune(svm, GoodCredit~., data = data.train, kernel = "linear", ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100))) #check a range of parameters and choose best model
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

Summary/ Cost-Profit Consideration
==================================

Compute the cost-profit consideration table with the formula described in the Introduction section.

``` r
table(GoodCreditity.test, glm.predict1)
table(GoodCreditity.test, tree.pred2)
table(GoodCreditity.test, rf.predict)
table(GoodCreditity.test, svm.predict)
table( GoodCreditity.test, svc.predict)
```

``` r
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

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="border-bottom:hidden" colspan="1">
</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2">
Logistic

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2">
Discriminant Analysis

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2">
Trees

</th>
<th style="border-bottom:hidden" colspan="1">
</th>
<th style="border-bottom:hidden" colspan="1">
</th>
<th style="border-bottom:hidden" colspan="1">
</th>
</tr>
<tr>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">
Test

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">
30% threshold

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">
50% threshold

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">
Linear

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">
quadratic

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">
Unpruned Tree

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">
Pruned Tree

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">
Random Forest

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">
SVC

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">
SVM

</th>
</tr>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
X1
</th>
<th style="text-align:right;">
X2
</th>
<th style="text-align:right;">
X3
</th>
<th style="text-align:right;">
X4
</th>
<th style="text-align:right;">
X5
</th>
<th style="text-align:right;">
X6
</th>
<th style="text-align:right;">
X7
</th>
<th style="text-align:right;">
X8
</th>
<th style="text-align:right;">
X9
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Credible
</td>
<td style="text-align:right;">
0.500
</td>
<td style="text-align:right;">
0.600
</td>
<td style="text-align:right;">
0.6040
</td>
<td style="text-align:right;">
0.5780
</td>
<td style="text-align:right;">
0.5760
</td>
<td style="text-align:right;">
0.6180
</td>
<td style="text-align:right;">
0.6440
</td>
<td style="text-align:right;">
0.6160
</td>
<td style="text-align:right;">
0.6660
</td>
</tr>
<tr>
<td style="text-align:left;">
Non-Credible
</td>
<td style="text-align:right;">
0.102
</td>
<td style="text-align:right;">
0.176
</td>
<td style="text-align:right;">
0.1780
</td>
<td style="text-align:right;">
0.1520
</td>
<td style="text-align:right;">
0.1880
</td>
<td style="text-align:right;">
0.2020
</td>
<td style="text-align:right;">
0.2120
</td>
<td style="text-align:right;">
0.2120
</td>
<td style="text-align:right;">
0.2900
</td>
</tr>
<tr>
<td style="text-align:left;">
Per Applicant Profit
</td>
<td style="text-align:right;">
0.073
</td>
<td style="text-align:right;">
0.034
</td>
<td style="text-align:right;">
0.0334
</td>
<td style="text-align:right;">
0.0503
</td>
<td style="text-align:right;">
0.0136
</td>
<td style="text-align:right;">
0.0143
</td>
<td style="text-align:right;">
0.0134
</td>
<td style="text-align:right;">
0.0036
</td>
<td style="text-align:right;">
-0.0569
</td>
</tr>
</tbody>
</table>
In conclusion, Logistic Regression at a 30% threshold turned out to have the biggest profit per applicant.
