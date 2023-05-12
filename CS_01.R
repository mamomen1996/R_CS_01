# Case-Study Title: Survey Analysis
# Data Analysis methodology: CRISP-DM
# Dataset: Customer survey of an USA paper company
# Case Goal: Which variables affect on business income variables?


### Required Libraries ----
install.packages('moments')
install.packages('MASS')
install.packages('mice')
library(moments)
library(MASS)
library(mice)


### Read Data from File ----
data <- read.csv('CS_01_01.csv', header = T)
dim(data)  # 200 records, 24 variables

data_mv <- read.csv('CS_01_02.csv', header = T)  # second version of above dataset with Missing-Values
dim(data_mv)  # 70 records, 15 varibles


### Step 1: Business Understanding ----
 # know business process and issues
 # know the context of the problem
 # know the order of numbers in the business


### Step 2: Data Understanding ----
### Step 2.1: Data Inspection (Data Understanding from Free Perspective) ----
## Dataset variables definition
colnames(data)

# part 1) Demographics variables (conrtolling variables)
#"customer_type": Length of time a particular customer has been buying from company:
                    #1 = less than 1 year
                    #2 = between 1 and 5 years
                    #3 = longer than 5 years

#"industry_type": Type of industry that purchases paper products:
                    #0 = magazine industry
                    #1 = newsprint industry

#"firm_size":     Employee (organization) size:
                    #0 = small firm, fewer than 500 employees
                    #1 = large firm, 500 or more employees

#"region":        Customer location:
                    #0 = USA/North America
                    #1 = outside North America

#"dist_system":   How paper products are sold to customers (distribution system):
                    #0 = sold indirectly through a broker
                    #1 = sold directly

# part 2) Perception of the company (predictor variables)
#"prod_quality":         Perceived level of quality of the company's paper products (product quality)

#"e_commerce":           Overall image of HBAT's Web site, especially user-friendliness (online platform)

#"tech_support":         Extent to which technical support is offered to help solve product/service issues

#"complaint_resolution": Extent to which any complaints are resolved in a timely and complete manner

#"advertising":          Perceptions of the company's advertising campaigns in all types of media

#"prod_line":            Depth and breadth of company's product line to meet customer needs

#"salesforce_image":     Overall image of company's salesforce

#"competitive_pricing":  Extent to which the company offers competitive prices

#"warranty":             Extent to which the company stands behind its product/service warranties and claims

#"new_products":         Extent to which the company develops and sells new products

#"ordering_billing":     Perception that ordering and billing is handled efficiently and correctly

#"price_flexibility":    Perceived willingness of the company sales reps to negotiate price on purchases of paper products

#"delivery_speed":       Amount of time it takes to deliver the paper products once an order has been confirm

# part 3) Purchase outcome section (target variables)
#"customer_satisfaction": Customer satisfaction with past purchases from the company, measured on a 10-point graphic rating scale

#"likelihood_recom":      Likelihood of recommending the company to other firms as a supplier of paper products, measured on a 10-point graphic rating scale

#"likelihood_purchase":   Likelihood of purchasing paper products from the company in the future, measured on a 10-point graphic rating scale 

#"percentage_purchase":   Percentage of the responding firm's paper needs purchased from the company, measured on a 100-point percentage scale

#"future_relationship":   Extent to which the customer/respondent perceives his or her firm would engage in strategic alliance/partnership with the company:
                            #0 = Would not consider
                            #1 = Yes, would consider strategic alliance or partnership


### Step 2.2: Data Exploring (Data Understanding from Statistical Perspective) ----
## Overview of Dataframe
class(data)
dim(data)
head(data)
tail(data, 2)
View(head(data,20))
str(data)

## Categorical variables should be stored as factor
data$customer_type <- factor(data$customer_type)
data$industry_type <- factor(data$industry_type)
data$firm_size <- factor(data$firm_size)
data$region <- factor(data$region)
data$dist_system <- factor(data$dist_system)
data$future_relationship <- factor(data$future_relationship)

summary(data)

## Univariate Profiling (check each variable individually)
# Categorical variables
#customer_type
summary(data$customer_type)
table(data$cutomer_type, useNA = 'ifany')
sum(is.na(data$customer_type))
barplot(table(data$customer_type))

#industry_type
summary(data$industry_type)
table(data$industry_type, useNA = 'ifany')
sum(is.na(data$industry_type))
barplot(table(data$industry_type))

# Continuous variables
#ID
length(unique(data$ID)) == nrow(data)  # check ID uniqueness
rownames(data) <- data$ID
data <- data[,-1]

#prod_quality
summary(data$prod_quality)
range(data$prod_quality)
quantile(data$prod_quality)
sum(is.na(data$prod_quality))  # missing value

hist(data$prod_quality, freq=F)  # distribution
x_fit <- seq(min(data$prod_quality), max(data$prod_quality), by=0.1)
y_fit <- dnorm(x_fit, mean=mean(data$prod_quality), sd=sd(data$prod_quality))
lines(x_fit, y_fit, col='red')  # draw a normal distribution density curve with same mean and sd to data

boxplot(data$prod_quality)  # outliers detection by Tukey method

#test of distribution Normality
hist(data$prod_quality, freq=F, breaks=15)
lines(density(data$prod_quality), col='blue')
x_fit <- seq(min(data$prod_quality), max(data$prod_quality), by=0.1)
y_fit <- dnorm(x_fit, mean=mean(data$prod_quality), sd=sd(data$prod_quality))
lines(x_fit, y_fit, col='red')

qqnorm(data$prod_quality, main='QQ Plot of Product Quality', pch=20)
qqline(data$prod_quality, col='red')

shapiro.test(data$prod_quality)  # Normality test
#H0 (Null) assumption: data is from normal distribution
#H1 (Alternative) assumption: data is not from normal distribution
#p-value < 0.05 reject normality assumption (reject H0)

jarque.test(data$prod_quality)  # Skewness test
#H0 (Null) assumption: sample skewness is equal to 0
#H1 (Alternative) assumption: sample skewness is not equal to 0
#p-value < 0.05 reject normality assumption

anscombe.test(data$prod_quality)  # Kurtosis test
#H0 (Null) assumption: sample kurtosis is equal to 3 (normal distribution kurtosis)
#H1 (Alternative) assumption: sample kurtosis is not equal to 3
#p-value < 0.05 reject normality assumption

#Conclusion: reject normality assumption

#e_commerce
summary(data$e_commerce)
range(data$e_commerce)
quantile(data$e_commerce)
sum(is.na(data$e_commerce))  # missing value

hist(data$e_commerce, freq=F)  # distribution
x_fit <- seq(min(data$e_commerce), max(data$e_commerce), by=0.1)
y_fit <- dnorm(x_fit, mean=mean(data$e_commerce), sd=sd(data$e_commerce))
lines(x_fit, y_fit, col='red')  # draw a normal distribution density curve with same mean and sd to data

boxplot(data$e_commerce)  # outliers detection by Tukey method

#test of distribution Normality
hist(data$e_commerce, freq=F, breaks=15)
lines(density(data$e_commerce), col='blue')
x_fit <- seq(min(data$e_commerce), max(data$e_commerce), by=0.1)
y_fit <- dnorm(x_fit, mean=mean(data$e_commerce), sd=sd(data$e_commerce))
lines(x_fit, y_fit, col='red')

qqnorm(data$e_commerce, main='QQ Plot of Product Quality', pch=20)
qqline(data$e_commerce, col='red')

shapiro.test(data$e_commerce)  # Normality test
#H0 (Null) assumption: data is from normal distribution
#H1 (Alternative) assumption: data is not from normal distribution
#p-value < 0.05 reject normality assumption (reject H0)

jarque.test(data$e_commerce)  # Skewness test
#H0 (Null) assumption: sample skewness is equal to 0
#H1 (Alternative) assumption: sample skewness is not equal to 0
#p-value < 0.05 reject normality assumption

anscombe.test(data$e_commerce)  # Kurtosis test
#H0 (Null) assumption: sample kurtosis is equal to 3 (normal distribution kurtosis)
#H1 (Alternative) assumption: sample kurtosis is not equal to 3
#p-value < 0.05 reject normality assumption

#Conclusion: reject normality assumption

# Plot all 13 measuring perception variables in one plot
par(mfrow = c(4,4), mar = c(2,2,2,2), cex.main = 0.8)
for(i in 6:18){
	hist(data[,i],
		xlab = "",
		main = colnames(data)[i],
		freq = F)

	lines(density(data[,i]), col = 'blue')

	x_fit <- seq(min(data[,i]), max(data[,i]), length.out = 50)
	y_fit <- dnorm(x_fit, mean = mean(data[,i]), sd = sd(data[,i]))
	lines(x_fit, y_fit, col = 'red')
}

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, cex.main = 1)

## Bivariate Profiling (measure 2-2 relationships between variables)
# Two Continuous variables (Correlation Analysis)
cor(data$prod_quality, data$likelihood_purchase, method = 'pearson')
plot(data$prod_quality, data$likelihood_purchase)

#Hypothesis Test: Spearman Correlation test
cor.test(data$prod_quality, data$likelihood_purchase, method = 'spearman', exact = F)
#H0(Null hypothesis): cor = 0 in population
#H1(Alternative hypothesis): cor in population is not 0 and is = what we calculated for cor based-on our statistical sample
#p-value < 0.05 reject H0 assumption
#result: cor is not 0, and is 0.4482

#Correlation table
cor_table <- round(cor(data[,c(21, 6:18)]), 2)
View(cor_table)
#result: the prod_line has the most linear effect on likelihood_purchase

# Two Categorical variables (Cross-Tabulation Analysis)
#firm_size vs. future_relationship (are independent or not?)
#our hypothesis: big firm_size companies, would like to have future_relasionship with us
cross_tab <- table(firm_size = data$firm_size, future_relationship = data$future_relationship)
cross_tab

chisq.test(cross_tab)
#H0: future_relationship is independent of firm_size
#H1: future_relationship is related to firm_size
#If p-value < 0.05 reject H0

#proportion table
prop.table(cross_tab)  # total percentage
prop.table(cross_tab,1)  # marginal percentage on row
prop.table(cross_tab,2)  # marginal percentage on column

#result: it seems that distributions are different and there is a relationship between firm_size and future_relationship (they are related to each other)

# Categorical vs. Continuous variables
#prod_quality vs. future_relationship
boxplot(prod_quality ~ future_relationship, data = data, names = levels(data$future_relationship))  # descriptive way
means <- tapply(data$prod_quality, data$future_relationship, mean)
points(x = c(1,2), y = means, pch = 19, col = 'red', cex = 1.25)

#measure if Median of two groups are equal or not?
wilcox.test(prod_quality ~ future_relationship, data = data, alternative = 'two.sided', paired = F, mu = 0)

#result: the median of two groups is statistically significant different -> product_quality has effect on future_relationship


### Step 3: Data PreProcessing ----
### Step 3.1: Data Manipulation (Changing variable) ----
# Min-Max Normalization
summary(data$percentage_purchase)
hist(data$percentage_purchase, breaks = 20)

mn_normalize <- function(x){
	return((x - min(x)) / (max(x) - min(x))
}

data$percentage_purchase_mn_normalized <- mn_normalize(data$percentage_purchase)
hist(data$percentage_purchase_mn_normalized, breaks = 20)

# Z-score Normalization
z_normalize <- function(x){
	return((x - mean(x)) / sd(x))
}

data$percentage_purchase_z_normalized <- z_normalize(data$percentage_purchase)
hist(data$percentage_purchase_z_normalized, breaks = 20)

# Linear Transformation
#aggregate likelihood_recom and likelihood_purchase (combine to one variable) into future_action: measure prediction of customer behavior in future
summary(data[,c(20,21)])
data$future_action <- 1 / 2 * (data$likelihood_recom + data$likelihood_purchase)  # linear combination
summary(data$future_action)

# Discretize data
summary(data$percentage_purchase)
data$purchase_vol <- ifelse(data$percentage_purchase > median(data$percentage_purchase), 'High', 'Low')
data$purchase_vol


# Transform Skewed data to Normal data
data$prod_quality  # our data is skewed to left and is not normal, we want to close it to normal

#Manual transformation (to make a variable Normal: decrease its skewness)
log_prod_quality <- log(max(data$prod_quality) + 0.001 - data$prod_quality)  # classic changing variable technique

#test of distribution Normality
hist(log_prod_quality, freq=F, breaks=15)
lines(density(log_prod_quality), col='blue')
x_fit <- seq(min(log_prod_quality), max(log_prod_quality), by=0.1)
y_fit <- dnorm(x_fit, mean=mean(log_prod_quality), sd=sd(log_prod_quality))
lines(x_fit, y_fit, col='red')

qqnorm(log_prod_quality, main='QQ Plot of Product Quality', pch=20)
qqline(log_prod_quality, col='red')

shapiro.test(log_prod_quality)  # Normality test
#H0 (Null) assumption: data is from normal distribution
#H1 (Alternative) assumption: data is not from normal distribution
#p-value < 0.05 reject normality assumption (reject H0)

jarque.test(log_prod_quality)  # Skewness test
#H0 (Null) assumption: sample skewness is equal to 0
#H1 (Alternative) assumption: sample skewness is not equal to 0
#p-value < 0.05 reject normality assumption

anscombe.test(log_prod_quality)  # Kurtosis test
#H0 (Null) assumption: sample kurtosis is equal to 3 (normal distribution kurtosis)
#H1 (Alternative) assumption: sample kurtosis is not equal to 3
#p-value < 0.05 reject normality assumption

#Conclusion: this transformation could not close prod_quality to normal distribution -> keep going prod_quality transformation manually to see which transformation close it to normal (not recommended)

#Box-Cox transformation (to make a variable Normal: decrease its skewness)
box_results <- boxcox(data$prod_quality ~ 1, lambda = seq(-6, 6, by = 0.1))
class(box_results)
box_results  # x:searched lambdas, y:log(likelihood)
lambda <- box_results$x[which(box_results$y == max(box_results$y))]  # find the lambda which have maximum log_likelihood
boxcox_prod_quality <- (data$prod_quality ^ lambda - 1) / lambda

#test of distribution Normality
hist(boxcox_prod_quality, freq=F, breaks=15)
lines(density(boxcox_prod_quality), col='blue')
x_fit <- seq(min(boxcox_prod_quality), max(boxcox_prod_quality), by=0.1)
y_fit <- dnorm(x_fit, mean=mean(boxcox_prod_quality), sd=sd(boxcox_prod_quality))
lines(x_fit, y_fit, col='red')

qqnorm(boxcox_prod_quality, main='QQ Plot of Product Quality', pch=20)
qqline(boxcox_prod_quality, col='red')

shapiro.test(boxcox_prod_quality)  # Normality test
#H0 (Null) assumption: data is from normal distribution
#H1 (Alternative) assumption: data is not from normal distribution
#p-value < 0.05 reject normality assumption (reject H0)

jarque.test(boxcox_prod_quality)  # Skewness test
#H0 (Null) assumption: sample skewness is equal to 0
#H1 (Alternative) assumption: sample skewness is not equal to 0
#p-value < 0.05 reject normality assumption

anscombe.test(boxcox_prod_quality)  # Kurtosis test
#H0 (Null) assumption: sample kurtosis is equal to 3 (normal distribution kurtosis)
#H1 (Alternative) assumption: sample kurtosis is not equal to 3
#p-value < 0.05 reject normality assumption

#Conclusion: this transformation could not close prod_quality to normal distribution


### Step 3.2: Recognize and Dealing with Missing Values ----
# Correlation Analysis with Missing Data (we can not calculate Correlation when have NAs)
#Determine the type of MVs in our sample (know the cause of having NAs)
View(data_mv)

#Determine the Extent of MVs in our data
summary(data_mv)
data_mv[data_mv == '.'] <- NA  # replace '.' with NA (Not-Available)
data_mv[,2:10] <- lapply(data_mv[,2:10], as.numeric)  # convert columns 2 to 10 to numeric
data_mv[,11:15] <- lapply(data_mv[,11:15], as.factor)  # convert columns 11 to 15 to factor
summary(data_mv)

#Variable aspect: NA count by columns
mv_summary_1 <- data.frame(variable_names = colnames(data_mv))
mv_summary_1$mvs_freq <- apply(data_mv, 2, function(x) sum(is.na(x)))  # frequency of NAs in each variable
mv_summary_1$mvs_percent <- round(mv_summary_1$mvs_freq / nrow(data_mv), 3) * 100
View(mv_summary_1)  # NA count and percentage in each variable

#Case aspect: NA count by rows
mv_summary_2 <- as.data.frame(table(apply(data_mv, 1, function(x) sum(is.na(x)))))  # frequencey of NAs in each row
colnames(mv_summary_2) <- c('mvs_per_case', 'mvs_freq')
mv_summary_2$mvs_percent <- round(mv_summary_2$mvs_freq / nrow(data_mv), 3) * 100
mv_summary_2$mvs_per_case <- as.numeric(levels(mv_summary_2$mvs_per_case))
mv_summary_2$mvs_per_case_percent <- round(mv_summary_2$mvs_per_case / (ncol(data_mv) - 1), 3) * 100
View(mv_summary_2[c(1,4,2,3)]

#Check Patterns of Missing Data by Case (Observation)
data_w_mvs <- data_mv[apply(data_mv, 1, function(x) any(is.na(x))), ]  # choose observation with at-least one NA
data_w_mvs$mvs_count <- apply(data_w_mvs, 1, function(x) sum(is.na(x)))
View(data_w_mvs)

#Decision: remove variable V1 (30% NAs)                  -> systematic NAs
#          and cases with 50% and more NAs from dataset
#      ID: 210, 214, 233, 245, 261, 263                  -> have not enough quality for analysis

data_mv_1 <- data_mv[- which(data_mv$ID %in% data_w_mvs[which(data_w_mvs$mvs_count >= 7), 1]), ]
data_mv_1 <- data_mv_1[,-2]
dim(data_mv_1)
View(data_mv_1)

#Select and Use the Imputation method
#method 1: Complete Case Approach (just use complete rows)
data_1_comp <- data_mv_1[apply(data_mv_1, 1, function(x) any(is.na(x))) == F, ]
View(data_1_comp)

mean_imput_comp <- data.frame(apply(data_1_comp[2:9], 2, mean))
colnames(mean_imput_comp) <- 'all_case'
mean_imput_comp

sd_imput_comp <- data.frame(apply(data_1_comp[2:9], 2, sd))
colnames(sd_imput_comp) <- 'all_case'
sd_imput_comp

#method 2: Mean Substitution (replace NA with mean(column))
data_1_mean_sub <- data_mv_1
for (i in 2:9){
	data_1_mean_sub[is.na(data_1_mean_sub[,i]),i] <- mean(data_1_mean_sub[,i], na.rm = T)
}
View(data_1_mean_sub)

mean_imput_comp$mean_sub <- apply(data_1_mean_sub[2:9], 2, mean)
mean_imput_comp

sd_imput_comp$mean_sub <- apply(data_1_mean_sub[2:9], 2, sd)
sd_imput_comp

#method 3: Regression Imputation
data_1_reg <- data_mv_1
imp_reg <- mice(data_1_reg[,2:9], method = 'norm.predict', m = 5)
class(imp_reg)
data_1_reg[,2:9] <- complete(imp_reg)
View(data_1_reg)

mean_imput_comp$reg <- apply(data_1_reg[2:9], 2, mean)
mean_imput_comp

sd_imput_comp$reg <- apply(data_1_reg[2:9], 2, sd)
sd_imput_comp

#Correlation Analysis
View(cor(data_1_comp[2:9]))  # All Complete Case
View(cor(data_1_mean_sub[2:9]))  # Mean Substitution
View(cor(data_1_reg[2:9]))  # Regression


### Step 3.3: Recognize and Dealing with Outliers ----
# Classic method (based-on normal distribution)
(data$percentage_purchase - mean(data$percentage_purchase)) / sd(data$percentage_purchase) > 3

# Tukey method (based-on quantile logic)
boxplot(data$percentage_purchase)$out

# MAD-Median Rule method
abs(data$percentage_purchase - median(data$percentage_purchase))/mad(data$percentage_purchase) > 2.5
