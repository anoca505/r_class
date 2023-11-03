x=5+3
y=30
ans = (x+y)*3

#creating sector
#you use c binding object, or concatenation binding object to define a vector
num = c(1,2,3)
char = c('apple', 'banana', 'mango', 'cherry')
logical = c(T,F,T,F,F,T)

char[3]


#matrices
m = matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3)
m
m[2,3]


#dataframe
df = data.frame(name=c('Alice','Bob','Charlie'),
                age=c(25,30,22),
                gender=c('F','M','M'))
df
### access the variable

df$name
df[1,2]


#list
my_list = list(name='Alice', age=25, scores = c(85,90,92))
my_list$scores[2]



## loading data
df = read.csv('data.csv', header = T)

###checking the top of the data
head(df)


### look at the structure of the data
str(df)

###loading dplyr library
library(dplyr)

##### check dimention of data
dim(df)
View(df)

####filter rows based on condition (ctrl+shift+m)
filtered_data = df %>% filter(var1 > 2)
head(filtered_data)

filtered_data = df %>% filter (var4 != "NA")
dim(filtered_data)

df2=df
##another method
df_clean = df2[!is.na(df2$var4), ]

##remove all rows with NA at the same time
df_clean = na.omit(df2)
head(df_clean)

## replace all NA with 0 or any figure
df2[is.na(df)] = 0

#### computing mean
mean(df2$var4, na.rm = T)
## to compute NA with mean
df2$var4[is.na(df2$var4)] = mean(df2$var4, na.rm = T)
head(df2)

##compute for all columns
df2[] = lapply(df2, function(col){
  col[is.na(col)] = mean(col,na.rm = T)
  return(col)
} )
head(df2)

write.csv(df2, file = "clean_data.csv")

#### round up 
df2 = round(df2, 2)


#####descriptive statistics
## mean
mean_df = mean(df2$var5)
mean_df

median_df = median(df2$var5)
median_df

#### Mode : how to creat a mode function
Mode = function(x){
  ux = unique(x)
  ux[which.max(tabulate(x,ux))]
}


## to create a function
calc = function(x,y){
  ans = x+y
  print (ans)
}


#### function to convert celsius to fahrenheit
### F = (C x 9/5) + 32

c_to_f = function(c){
  f = (c * (9/5)) + 32
  print(f)
}

c_to_f(100)
c_to_f(0)


#### measure of dispersion
## variance
var1 = var(df2$var4)
var1

## standard deviation
df_sd = sd(df2$var5)
df_sd

## range
df_rng = range(df2$var4)
df_rng
print(paste("range is: ", df_rng))


## Qartiles and percentiles
qual = quantile(df2$var6)
print(paste('the value for my quartiles is: ', qual))

### inter-quartile range
iqr_val = IQR(df$var6, na.rm = T)
print(iqr_val)

perc_50 = quantile(df2$var5, 0.5)
perc_50

head(df2)

#### visualization
hist(df2$var1, main = 'Histogram', xlab = 'Values', ylab = "Frequency", col = "skyblue", border = "black")

hist(df2$var4, main = 'Histogram', xlab = 'Values', ylab = "Frequency", col = "skyblue", border = "black")


### boxplot
boxplot(df2$var1, main = "Boxplot", ylab = "Values", col = "red", border = "black")


boxplot(df2, main = "boxplot of each column", xlab = "columns", ylab = "scores", col = rainbow(ncol(df2)))

barplot(df2, main = "barblot", xlab = "values", ylab = 'freq')
##### we are assuming response variable is var1
## make var6 categories
df2$group = sample (c('group1', 'group2', 'group3'), size = nrow(df2), replace = TRUE)
head(df2)
df2$rep = sample(c('high','low'), size=nrow(df2), replace = TRUE)
str(df2)
df2$rep = rnorm(nrow(df2), mean = 20, sd=5)
#### perform a one way anova
anova_results = aov(var1 ~ group, data = df2)
summary(anova_results)

#### two way anova with interaction
anova_results2 = aov(var1 ~ var4 * var6, data = df2)
summary(anova_results2)


df2$group = as.factor(df2$group)

### two way anova with interaction term
anova_results = aov(rep ~ group * var6, data = df2)
summary(anova_results)


### correlation
df_cor = cor(df2$var2, df2$var6)
df_cor


## regression
model = lm(var2 ~ var6, data = df2)
summary(model)
plot(df2$var1, df2$var6, main = "scatter plot with regression line", xlab = 'var1', ylab = "var6")
abline(model, col = "red")
