# Import two data sets from the "FIles" menu at bottom-right corner, 
# which are prsent in the working directory
library(readr)
alcohol <- read_csv("alcohol.csv")
View(alcohol)
str(alcohol)

CrimeRate <- read_csv("CrimeRate.csv")
View(CrimeRate)
str(CrimeRate)

# Load Alcohol consumption data and CrimeRate data to a dataframe
alcohol_ds <- data.frame(alcohol)
crimerate_ds <- data.frame(CrimeRate)
crimerate_ds <- crimerate_ds[-1]

View(alcohol_ds)
View(crimerate_ds)

# check if there are any NULL values in the dataframes
sum(is.na(alcohol_ds))
sum(is.na(crimerate_ds))

# All the data for the crime types is in quarters. Add them to make the data simple

crimerate_ds$'2003T' <- sum(crimerate_ds$X2003Q1, crimerate_ds$X2003Q2, crimerate_ds$X2003Q3, 
                            crimerate_ds$X2003Q4)

crimerate_ds$'2004T' <- sum(crimerate_ds$X2004Q1, crimerate_ds$X2004Q2, crimerate_ds$X2004Q3, 
                            crimerate_ds$X2004Q4)

crimerate_ds$'2005T' <- sum(crimerate_ds$X2005Q1, crimerate_ds$X2005Q2, crimerate_ds$X2005Q3, 
                            crimerate_ds$X2005Q4)

crimerate_ds$'2006T' <- sum(crimerate_ds$X2006Q1, crimerate_ds$X2006Q2, crimerate_ds$X2006Q3, 
                            crimerate_ds$X2006Q4)

crimerate_ds$'2007T' <- sum(crimerate_ds$X2007Q1, crimerate_ds$X2007Q2, crimerate_ds$X2007Q3, 
                            crimerate_ds$X2007Q4)

crimerate_ds$'2008T' <- sum(crimerate_ds$X2008Q1, crimerate_ds$X2008Q2, crimerate_ds$X2008Q3, 
                            crimerate_ds$X2008Q4)

crimerate_ds$'2009T' <- sum(crimerate_ds$X2009Q1, crimerate_ds$X2009Q2, crimerate_ds$X2009Q3, 
                            crimerate_ds$X2009Q4)

crimerate_ds$'2010T' <- sum(crimerate_ds$X2010Q1, crimerate_ds$X2010Q2, crimerate_ds$X2010Q3, 
                            crimerate_ds$X2010Q4)

crimerate_ds$'2011T' <- sum(crimerate_ds$X2011Q1, crimerate_ds$X2011Q2, crimerate_ds$X2011Q3, 
                            crimerate_ds$X2011Q4)

crimerate_ds$'2012T' <- sum(crimerate_ds$X2012Q1, crimerate_ds$X2012Q2, crimerate_ds$X2012Q3, 
                            crimerate_ds$X2012Q4)

crimerate_ds$'2013T' <- sum(crimerate_ds$X2013Q1, crimerate_ds$X2013Q2, crimerate_ds$X2013Q3, 
                            crimerate_ds$X2013Q4)

crimerate_ds$'2014T' <- sum(crimerate_ds$X2014Q1, crimerate_ds$X2014Q2, crimerate_ds$X2014Q3, 
                            crimerate_ds$X2014Q4)

crimerate_ds$'2015T' <- sum(crimerate_ds$X2015Q1, crimerate_ds$X2015Q2, crimerate_ds$X2015Q3, 
                            crimerate_ds$X2015Q4)

crimerate_ds$'2016T' <- sum(crimerate_ds$X2016Q1, crimerate_ds$X2016Q2, crimerate_ds$X2016Q3, 
                            crimerate_ds$X2016Q4)

# create a seperate dataset for the created columns

crimerate_ds_total <- data.frame(crimerate_ds[65:78])
View(crimerate_ds_total)

years <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
totals <- c(45040, 44174, 50679, 52441, 57848, 59011, 53962, 49964, 46050, 42647, 38152, 37548, 37596,
            34730)

# with the obtained values, create a new dataset so that the data will be in columns.
# as it will be easy to compare

modified_crimerate <- data.frame(years, totals)


str(alcohol_ds)

final_dataset <- data.frame(modified_crimerate$years, modified_crimerate$totals,
                            alcohol_ds$CONSUMPTION.IN.LITRES.PER.PERSON, 
                            alcohol_ds$TOTAL.CONSUMPTION.IN.LITRES)

column_names <- c('Year', 'Total_Crime_Rate', 'Avg_Consumption_per_person',
                  'Total_Consumption')
colnames(final_dataset) <- column_names
View(final_dataset)

# linear regression model with Cime rate in ireland and alcohol dataset 
linear_model <- lm(Total_Crime_Rate ~ Total_Consumption, data = final_dataset)
linear_model
summary(linear_model)

# plot ALcohol and Crime rate variable to see relationship between the response(crime) and
# predictor (alcohol) variable

plot(final_dataset$Total_Crime_Rate,final_dataset$Total_Consumption,
     xlab="Crime Rate", ylab="Alcohol Consumption",
     main = "Scatter plot showing regression line for Crime, predicted from Alcohol consumption")
abline(linear_model)

# Graph shows a there is **no** relationship between rent and income variable

cor(final_dataset$Total_Crime_Rate,final_dataset$Total_Consumption)

# Examining the 95% confidence intervals of the model

confint(linear_model)

# Scatter plots helps to visualise any linear relationships between the dependent variable 
# and independent variables

scatter.smooth(x = final_dataset$Total_Consumption, 
               y = final_dataset$Total_Crime_Rate, 
               main = "Alcohol ~ Crime",
               xlab = "Crime",
               ylab = "Alcohol")

# Box Plot
par(mfrow = c(1, 2)) # divide graph area in 2 columns
# box plot for ‘Alcohol’
boxplot(final_dataset$Total_Consumption, main = "Alcohol", 
        sub = paste("Outlier rows: ", boxplot.stats(final_dataset$Total_Consumption)$out))
# box plot for 'Crime Rate'
boxplot(final_dataset$Total_Crime_Rate, main = "Crime Rate", 
        sub = paste("Outlier rows: ", boxplot.stats(final_dataset$Total_Crime_Rate)$out)) 

# Skewness function to examine normality of data
#install.packages("e1071")
library(e1071)
# Density Plot
# Divide graph area in 2 columns
par(mfrow = c(1, 2))

# Density plot for ALcohol
plot(density(final_dataset$Total_Consumption), main = "Density Plot :Alcohol",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(final_dataset$Total_Consumption), 2)))

# fill the area with blue
polygon(density(final_dataset$Total_Consumption), col = "blue")

# Density plot for Alcohol
plot(density(final_dataset$Total_Crime_Rate), main = "Density Plot :Crime Rate",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(final_dataset$Total_Crime_Rate), 2)))

# Filling the area with blue
polygon(density(final_dataset$Total_Crime_Rate), col = "blue")

#------------------------------------------------------------------

# Now, calculating correlation test between Acohol and Crime Rate
cor(final_dataset$Total_Consumption, final_dataset$Total_Crime_Rate)

# to build linear regression model on full data
linearMod <- lm(Total_Crime_Rate ~ Total_Consumption, data = final_dataset)
linearMod

# summary of model
summary(linearMod)

model_summary <- summary(linearMod)

# model coefficients
model_coeffs <- model_summary$coefficients
model_coeffs

# get beta estimate for Income
beta.estimate <- model_coeffs["Total_Consumption", "Estimate"]
beta.estimate
# get std.error for Income
std_error <- model_coeffs["Total_Consumption", "Std. Error"]
std_error
# calc t statistic
t_value <- beta.estimate / std_error
p_value <- 2 * pt(-abs(t_value), df = nrow(final_dataset) - ncol(final_dataset)) # calc p Value
f_statistic <- linearMod$fstatistic[1] # fstatistic
f <- summary(linearMod)$fstatistic # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower = FALSE)

# sample chooses a random sample
# from 1:all records from cars, 80% of rows
no_of_records <- sample(1:nrow(final_dataset), 0.8 * nrow(final_dataset))
# model training data
training_data <- final_dataset[no_of_records,]
training_data
# test data
testing_data <- final_dataset[-no_of_records,]
testing_data

# Build the model on training data
# lm(formula, data) where
# formula describes the model to be fit
lm_model <- lm(Total_Crime_Rate ~ Total_Consumption, data = training_data)

# model summary
summary(lm_model)
#-----------------------------------------------------------------------

# predict  from testing data
lm_predicted <- predict(lm_model, testing_data)
summary(lm_predicted)
# make actuals_predicteds dataframe.
lm_actuals_preds <- data.frame(cbind(actuals = testing_data$Rent, 
                                     predicted = lm_predicted))
head(lm_actuals_preds)

AIC(linearMod)

BIC(linearMod)

correlation_accuracy <- cor(lm_actuals_preds)
correlation_accuracy

# Min - max accuracy
lm_min_max_accuracy <- mean(apply(lm_actuals_preds, 1, min) / apply(lm_actuals_preds, 1, max))
lm_min_max_accuracy


# MAPE
lm_mape <- mean(abs((lm_actuals_preds$predicted - lm_actuals_preds$actuals)) / lm_actuals_preds$actuals)
lm_mape

# Global validation of linear model assumption
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(linearMod)
summary(gvmodel)
plot(gvmodel)

#-----------------------------------------------------------------------------------------------------------------
