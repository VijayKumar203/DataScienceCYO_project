#' ---
#' title: 'Capstone CYO Project Report'
#' subtitle: ' Prediction of Credit Risk for German Bank'
#' author: 'Endri Raco'
#' output:
#'   pdf_document:
#'     df_print: kable
#'     toc: yes
#' documentclass: report
#' classoption: a4paper
#' fig_height: 5
#' fig_width: 5
#' fontsize: 10pt
#' highlight: zenburn
#' latex_engine: xelatex
#' mainfont: Arial
#' mathfont: LiberationMono
#' monofont: DejaVu Sans Mono
#' urlcolor: blue
#' bibliography: references.bib
#' ---
------
## specify the packages needed
 if(!require(rpart)) install.packages('rpart', 
repos = 'http://cran.us.r-project.org')
## specify the packages needed
 if(!require(gmodels)) install.packages('gmodels', 
repos = 'http://cran.us.r-project.org')
 if(!require(ROCR)) install.packages('ROCR', 
repos = 'http://cran.us.r-project.org')
if(!require(epiDisplay)) install.packages('epiDisplay', 
repos = 'http://cran.us.r-project.org')
if(!require(kableExtra)) install.packages('kableExtra', 
repos = 'http://cran.us.r-project.org')
if(!require(dataCompareR)) install.packages('dataCompareR', 
repos = 'http://cran.us.r-project.org')
if(!require(tidyverse)) install.packages('tidyverse', 
repos = 'http://cran.us.r-project.org')
if(!require(caret)) install.packages('caret', 
repos = 'http://cran.us.r-project.org')
if(!require(data.table)) install.packages('data.table', 
repos = 'http://cran.us.r-project.org')

# For all project calculations is used the following PC:

print('Operating System:')
version

## Methods and Analysis

### Importing data

# South German Credit Dataset:
# https://data.ub.uni-muenchen.de/23/2/kredit.asc
# Code for creating SouthGermanCredit.asc as described in @gromping2019
temp <- read.table('https://data.ub.uni-muenchen.de/23/2/kredit.asc', header=TRUE)

# recode pers and gastarb to the stated P2 coding

temp$pers <- 3 - temp$pers
temp$gastarb <- 3 - temp$gastarb

# put credit_risk is last

temp <- cbind(temp[,-1], kredit=temp$kredit)
write.table(temp, file='./data/SouthGermanCredit.asc',
row.names = FALSE, quote=FALSE)

# save dataset in our data folder
data_credit <- read.table('./data/SouthGermanCredit.asc', header=TRUE)

# remove temp file
remove(temp)

# Check structure of downloaded dataset

str(data_credit)


### Data Processing

# show-attributes

split(names(data_credit),sapply(data_credit, function(x) paste(class(x), collapse=' ')))

# Rename columns

data_credit  <- setNames(data_credit, 
c('account_status', 'duration_month', 'credit_history', 'credit_purpose', 
'credit_amount', 'savings_account', 'employment_present', 'installment_rate_pct', 'status_sex', 'other_debtors_guar', 
'residence_duration', 'property', 'age_years', 'other_install_plans', 
'housing', 'exist_credits_nr', 'job', 'dependents_nr', 'telephone_nr', 'foreign_worker', 'customer_good_bad'))


# Check structure of downloaded dataset

str(data_credit)

# First five lines using the function head

kable_styling(
              kable(head(data_credit[, 1:3], 5), digits = 3, row.names = FALSE, align = 'c',
              caption = NULL, format = 'latex'),
        latex_options = c('striped', 'basic'),
        position = 'center', full_width = FALSE) 


# Check data_credit dataframe for NA values

sapply(data_credit, function(x) sum(is.na(x)))


# convert variables to class factor

variables <- c('account_status','credit_history', 'credit_purpose', 'savings_account',      'employment_present', 'installment_rate_pct', 'status_sex', 'other_debtors_guar',   'residence_duration', 'property', 'other_install_plans', 'housing' ,  'exist_credits_nr',    
'job',  'dependents_nr',  'telephone_nr', 'foreign_worker' ,'customer_good_bad')
data_credit[,variables]  <- lapply(data_credit[,variables] , factor)


# Factors vs numeric variables

split(names(data_credit),sapply(data_credit, function(x) paste(class(x), collapse=' ')))

str(data_credit)


## Exploring Data


# summary customer_good_bad

tab1(data_credit$customer_good_bad, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'frequency',  cex = 1, cex.names = 1, main = 'Distribution of data', xlab = 'customer_good_bad', 
    ylab = 'count', col=c('red','yellow','blue')) 


# summary account_status
tab1(data_credit$account_status, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent',  cex = 1, cex.names = 1, main = 'Distribution of data', xlab = 'account_status', 
    ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# recode account_status
account_status_temp <- recode(data_credit$account_status, '3' ='positive_acc', '4'='positive_acc', '1' = 'no_account', '2' = 'no_money_acc')
data_credit$account_status <- account_status_temp

# recoding results
tab1(data_credit$account_status, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent',  cex = 1, cex.names = 1, main = 'Distribution of data', xlab = 'account_status', 
    ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 

# summary duration_month
summary(data_credit$duration_month)
data_credit %>% ggplot(aes(duration_month)) + geom_density()


#summary credit_history
tab1(data_credit$credit_history, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'frequency',  cex = 1, cex.names = 1, main = 'Distribution of data', xlab = 'credit_history', 
ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 

# recode credit_history
credit_history_temp           <- recode(data_credit$credit_history, '0' = 'pay_problems', '1' = 'pay_problems', '2' = 'all_paid', '3' = 'no_prob_currbank', '4' = 'no_prob_currbank')
data_credit$credit_history    <- credit_history_temp

# recoding results
tab1(data_credit$credit_history, sort.group = 'decreasing', cum.percent = FALSE, 
bar.values = 'percent',  cex = 1, cex.names = 1, main = 'Distribution of data', 
xlab = 'credit_history', 
ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary credit_purpose
tab1(data_credit$credit_purpose, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'credit_purpose', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 

# recode credit_purpose
credit_purpose_temp <- recode(data_credit$credit_purpose, '0' = 'services', '1' = 'new_car', '2' = 'used_car', '3' = 'domestic', '4' = 'domestic', '5' = 'domestic','6' = 'services', '7' = 'services', '8' = 'services',
                              '9' = 'services', '10' = 'services')
data_credit$credit_purpose <- credit_purpose_temp


# recoding results
tab1(data_credit$credit_purpose, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent',  cex = 1, cex.names = 1, main = 'Distribution of data', xlab = 'credit_purpose', 
ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 

# summary of credit_amount

summary(data_credit$credit_amount)
data_credit %>% ggplot(aes(credit_amount)) + geom_density()

# summary savings_account
tab1(data_credit$savings_account, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'savings_account', 
ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# recoding results
tab1(data_credit$savings_account, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent',  cex = 1, cex.names = 1, main = 'Distribution of data', xlab = 'savings_account', 
ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary employment_present
tab1(data_credit$employment_present, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'employment_present', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 

# recode employment_present
employment_present_temp <- recode(data_credit$employment_present, '1' = 'unemp_less1year', '2' = 'unemp_less1year', '3' = '1to4', '4' = '4to7', '5' = '7plus')
data_credit$employment_present <- employment_present_temp


# recoding results
tab1(data_credit$employment_present, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent',  cex = 1, cex.names = 1, main = 'Distribution of data', xlab = 'employment_present', 
ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary installment_rate_pct
tab1(data_credit$installment_rate_pct, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'installment_rate_pct', 
ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary status_sex
tab1(data_credit$status_sex, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'status_sex', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# recode status_sex
status_sex_temp <- recode(data_credit$status_sex, '1' = 'm_single_divorced', '2' = 'm_single_divorced', '3' = 'm_married_wid', '4' = 'female')
data_credit$status_sex <- status_sex_temp


# recoding results
tab1(data_credit$status_sex, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent',  cex = 1, cex.names = 1, main = 'Distribution of data', xlab = 'status_sex', 
ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary other_debtors_guar
tab1(data_credit$other_debtors_guar, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'other_debtors_guar', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 

# recode other_debtors_guar
other_debtors_guar_temp <- recode(data_credit$other_debtors_guar, '1' = 'no', '2' = 'yes', '3' = 'yes')
data_credit$other_debtors_guar <- other_debtors_guar_temp

# recoding results
tab1(data_credit$other_debtors_guar, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent',  cex = 1, cex.names = 1, main = 'Distribution of data', xlab = 'other_debtors_guar', 
ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary residence_duration
tab1(data_credit$residence_duration, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'residence_duration', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary property
tab1(data_credit$property, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'property', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary age_years
summary(data_credit$age_years)
data_credit %>% ggplot(aes(age_years)) + geom_density()


# summary other_install_plans
tab1(data_credit$other_install_plans, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'other_install_plans', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# recode other_install_plans
other_install_plans_temp <- recode(data_credit$other_install_plans, '1' = 'yes', '2' = 'yes', '3' = 'no')
data_credit$other_install_plans <- other_install_plans_temp


# recoding results
tab1(data_credit$other_install_plans, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent',  cex = 1, cex.names = 1, main = 'Distribution of data', xlab = 'other_install_plans', 
ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary housing
tab1(data_credit$housing, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'housing', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary exist_credits_nr
tab1(data_credit$exist_credits_nr, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'exist_credits_nr', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# recode exist_credits_nr
exist_credits_nr_temp <- recode(data_credit$exist_credits_nr, '1' = 'one', '2' = 'morethan1', '3' = 'morethan1', '4' = 'morethan1')
data_credit$exist_credits_nr <- exist_credits_nr_temp


# recoding results
tab1(data_credit$exist_credits_nr, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent',  cex = 1, cex.names = 1, main = 'Distribution of data', xlab = 'exist_credits_nr', 
ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary job
tab1(data_credit$job, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'job', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 

# summary dependents_nr
tab1(data_credit$dependents_nr, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'dependents_nr', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary telephone_nr
tab1(data_credit$telephone_nr, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'telephone_nr', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


# summary foreign_worker
tab1(data_credit$foreign_worker, sort.group = 'decreasing', cum.percent = FALSE, bar.values = 'percent', main = 'Distribution of data', xlab = 'foreign_worker', 
     ylab = 'count', col=c('red','yellow','blue'), horiz = TRUE) 


### Relationship between variables


# Crosstables of outcome vs account_status
CrossTable(data_credit$customer_good_bad,data_credit$account_status, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T, dnn = c('customer_good_bad', 'account_status'))
# Crosstables of outcome vs credit_history
CrossTable(data_credit$customer_good_bad,data_credit$credit_history,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'credit_history'))
# Crosstables of outcome vs credit_purpose
CrossTable(data_credit$customer_good_bad,data_credit$credit_purpose,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'credit_purpose'))
# Crosstables of outcome vs savings_account
CrossTable(data_credit$customer_good_bad,data_credit$savings_account,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'savings_account'))
# Crosstables of outcome vs employment_present
CrossTable(data_credit$customer_good_bad,data_credit$employment_present, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'employment_present'))
# Crosstables of outcome vs installment_rate_pct
CrossTable(data_credit$customer_good_bad,data_credit$installment_rate_pct,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'installment_rate_pct'))
# Crosstables of outcome vs status_sex
CrossTable(data_credit$customer_good_bad,data_credit$status_sex,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'status_sex'))
# Crosstables of outcome vs other_debtors_guar
CrossTable(data_credit$customer_good_bad,data_credit$other_debtors_guar,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'other_debtors_guar'))
# Crosstables of outcome vs residence_duration
CrossTable(data_credit$customer_good_bad,data_credit$residence_duration,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'residence_duration'))
# Crosstables of outcome vs property
CrossTable(data_credit$customer_good_bad,data_credit$property,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'property'))
# Crosstables of outcome vs other_install_plans
CrossTable(data_credit$customer_good_bad,data_credit$other_install_plans,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'other_install_plans'))
# Crosstables of outcome vs housing
CrossTable(data_credit$customer_good_bad,data_credit$housing,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'housing'))
# Crosstables of outcome vs exist_credits_nr
CrossTable(data_credit$customer_good_bad,data_credit$exist_credits_nr,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'exist_credits_nr'))
# Crosstables of outcome vs job
CrossTable(data_credit$customer_good_bad,data_credit$job,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'job'))
# Crosstables of outcome vs dependents_nr
CrossTable(data_credit$customer_good_bad,data_credit$dependents_nr,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'dependents_nr'))
# Crosstables of outcome vs telephone_nr
CrossTable(data_credit$customer_good_bad,data_credit$telephone_nr,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'telephone_nr'))
# Crosstables of outcome vs foreign_worker
CrossTable(data_credit$customer_good_bad,data_credit$foreign_worker,  digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T,  dnn = c('customer_good_bad', 'foreign_worker'))


## Building models

### Scaling numerical variables


# normalization of numeric features
data_credit %>%
    mutate_if(is.numeric, scale)

### Data Splitting

# Validation set will be 50% of South German Credit data
# Set seed as a starting point
set.seed(1, sample.kind='Rounding')
# Store row numbers for train set: test_index
test_index <- createDataPartition(y = data_credit$customer_good_bad, 
times = 1, p = 0.5, list = FALSE)


# Create the train set 
train <- data_credit[-test_index,]
# Create the validation set 
validation <- data_credit[test_index,]


# Save our data as R objects
save(train, file = './data/train.RData')
save(validation, file = './data/validation.RData')

# compare train and validation
library(dataCompareR)
comp_train_val <- rCompare(train, validation)
comp_summ <- summary(comp_train_val)
comp_summ[c('datasetSummary', 'ncolInAOnly', 'ncolInBOnly', 'ncolCommon', 'rowsInAOnly', 'rowsInBOnly', 'nrowCommon')] 


### Logistic Regression model

# Fitting initial model
glm_model <- glm(customer_good_bad ~ ., family = 'binomial', data = train)
# Obtain significance levels using summary()
summary(glm_model)


# Filter significant values
sig <- summary(glm_model)$coeff[-1,4] < 0.05
names(sig)[sig == T]


# Predictions
pred_logit <- predict(glm_model, newdata = validation, type = 'response')


# convert pred_logit to a vector of binary values : put as cut pred_logit > 0.5
y_hat_glm <- factor(ifelse(pred_logit > 0.5, 1, 0))
# print confusion matrix
confusionMatrix(y_hat_glm,
                reference=validation$customer_good_bad,
                positive='1')


# Print only accuracy
confusionMatrix(y_hat_glm, reference=validation$customer_good_bad,
                positive='1')$overall['Accuracy']




### Classification Tree model

# Decision Tree model
credit_tree <- rpart(customer_good_bad ~ . , method='class', data=train)
# Plot the decision tree
plot(credit_tree, uniform = TRUE)
#Add labels
text(credit_tree)


# Predictions
pred_tree <- predict(credit_tree, newdata = validation, type = 'class')


# print confusion matrix
confusionMatrix(pred_tree,
                reference=validation$customer_good_bad,
                positive='1')


# Print only accuracy
confusionMatrix(pred_tree, reference=validation$customer_good_bad,
                positive='1')$overall['Accuracy']


