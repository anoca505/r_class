#This data contains average tuition fees across universities.
#Do the following with the data using R,
# 1. Get the mean of the fees in 2005-06 year and 2004-05 year
# 2. calculate the median school fees for the year 2007-08
# 3. Find the quartile and interquartile values for the year 2006-07 
# 4. Using tTest, find the difference of the mean for the year 2005-06  and 2004-05. Also calculate the mean difference for the first 4 years usning ANOVA
# 5. Plot the relationship of the fees for the year 2004-05 and 2005-06
# 6. Download the codes you used and also send dowload the plots you did for submission.


## Loading the data
df = read.csv('usAvg_data.csv', header = T)

### Checking data statistics
head(df)
str(df)
dim(df)



df2 = df



library(dplyr)

# Remove $ symbol from all columns using mutate_all
df2 <- df2 %>% 
  mutate_all(~ gsub("\\$", "", .))

# Remove "," symbol from all columns using mutate_all
df2 <- df2 %>% 
  mutate_all(~ gsub("\\,", "", .))

print(typeof(df2$X2004.05))

##converting the columns to numberic
df2$X2005.06 <- as.numeric(df2$X2005.06) 
df2$X2004.05 <- as.numeric(df2$X2004.05)
df2$X2006.07 <- as.numeric(df2$X2006.07)
df2$X2007.08 <- as.numeric(df2$X2007.08)
df2$X2008.09 <- as.numeric(df2$X2008.09)
df2$X2009.10 <- as.numeric(df2$X2009.10)
df2$X2010.11 <- as.numeric(df2$X2010.11)
df2$X2011.12 <- as.numeric(df2$X2011.12)
df2$X2012.13 <- as.numeric(df2$X2012.13)
df2$X2013.14 <- as.numeric(df2$X2013.14)
df2$X2014.15 <- as.numeric(df2$X2014.15)
df2$X2015.16 <- as.numeric(df2$X2015.16)

# Check the data types after conversion
print(sapply(df2, class))

head(df2)

# 1. Get the mean of the fees in 2005-06 year and 2004-05 year
mean(df2$X2005.06). #6654.16
mean(df2$X2004.05).  ## 6409.6


# 2. calculate the median school fees for the year 2007-08
median_df = median(df2$X2007.08)
print(paste ('the median school fees for the year 2007-08 is:', median_df) ) 
## "the median school fees for the year 2007-08 is: 6616"


# 3. Find the quartile and interquartile values for the year 2006-07 
qual = quantile(df2$X2006.07)
inqual = IQR(df2$X2006.07)
print(paste("the quartile of 2006-07 are: ", qual))
print(paste("the inter-quartile of 2006-07 are: ", inqual))

## [1] "the quartile of 2006-07 are:  3888"    "the quartile of 2006-07 are:  5203.25"
## [3] "the quartile of 2006-07 are:  6348"    "the quartile of 2006-07 are:  8265.75"
## [5] "the quartile of 2006-07 are:  11473" 

##[1] "the inter-quartile of 2006-07 are:  3062.5"


# 4. Using tTest, find the difference of the mean for the year 2005-06  and 2004-05. 
     #Also calculate the mean difference for the first 4 years usning ANOVA
t.test(df2$X2005.06, data = df2)

#	One Sample t-test

# data:  df2$X2005.06
# t = 23.791, df = 49, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#  6092.103 7216.217
#. sample estimates:
#  mean of x 
# 6654.16 

##one way anova
model_one_way <- aov(X2004.05 ~ State, data = df2)
summary(model_one_way)
#####             Df    Sum Sq Mean Sq
##### State       49 186261464 3801254

## four way anova
model_four_way <- aov(State ~ X2004.05 * X2005.06 * X2006.07 * X2007.08, data = df)
summary(model_four_way)
### The data are not categorical, hence 4-way anova is inconclusive


# 5. Plot the relationship of the fees for the year 2004-05 and 2005-06
library(dplyr)
df_cor = cor(df2$X2004.05, df2$X2005.06)
print (paste("The relationship of the fees for the year 2004-05 and 2005-06 ie correlation is: ", df_cor))
### "The relationship of the fees for the year 2004-05 and 2005-06 ie correlation is:  0.995475271130395"

hist(df2$X2004.05, main = 'Histogram Plot', xlab = "year 2004-05", ylab = "frequency", col = "blue", border = "black")


library(ggplot2)

# Create a scatter plot of two years
ggplot(df2, aes(x = X2004.05, y = X2005.06)) +
  geom_point() +
  labs(x = "Year 2004-05", y = "Year 2005-06", title = "Scatter Plot of X vs Y")


# create a line plot of 2 years against the label
# Reshape the data into long format for ggplot
library(tidyr)
data_long <- pivot_longer(df2, cols = c(X2004.05, X2005.06), names_to = "Variable", values_to = "Value")

# Create a line plot
ggplot(data_long, aes(x = State, y = Value, color = Variable, group = Variable)) +
  geom_line() +
  labs(x = "States", y = "Two years", title = "Relationship of years 2004.05 and 2005.06 against State")

