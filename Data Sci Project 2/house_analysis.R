# Exploratory Analysis #

# Feature Selection - Check correlation between two variables, if similar then remove a variable #

plot(clean_house$sqft_living, clean_house$sqft_lot)
reg1 <- lm(sqft_living ~ sqft_lot, data = clean_house)
summary(reg1)

plot(clean_house$sqft_living, clean_house$sqft_above)
reg2 <- lm(sqft_living ~ sqft_lot, data = clean_house)
summary(reg2)

# We can see that there's a correlation between sqft_living and sqft_above however the data is biased towards the tail end
# This is why the R^2 value for the regression model is so low between these variables #

samp1 <- clean_house[clean_house$sqft_living < 3000 & clean_house$sqft_above < 2500,]
samp2 <- clean_house[clean_house$sqft_living > 3000 & clean_house$sqft_above > 2500,]

library(dplyr)
set.seed(10)
test1 <- sample_n(samp1, 250)

test2 <- sample_n(samp2, 250)
samp2[sample(nrow(samp2), 250),]

# Now we have a subset of the dataframe which is more unbiased than the entire dataframe 
# Here we can check if there are any correlations more closely #
all_test <- rbind(test1, test2)

# If we run the following code, we can tell there is a correlation between these two variables because the R^2 value is much higher #
plot(all_test$sqft_living, all_test$sqft_above)
sub_reg2 <- lm(sqft_living ~ sqft_above, data = all_test)
summary(sub_reg2)

# There are some variables that have little to no useful information, so I will be eliminating those columns - waterfront, view and sqft_basement #

# I have also combined the Yr_renovated and yr_built columns for efficiency #
all_test$yr_houseupdated <- ifelse(all_test$yr_renovated == 0, all_test$yr_built, all_test$yr_renovated)
clean_house$yr_house_maintenance <- ifelse(clean_house$yr_renovated == 0, clean_house$yr_built, clean_house$yr_renovated)

# The new dataframe new_cleanhouse contains all the relevant columns for analysis #
new_cleanhouse <- clean_house[-c(7,8,10,11,12,13)]

# Multi-Linear Regression #

plot(new_cleanhouse)
multiple.regression <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition + city + yr_house_maintenance, data = new_cleanhouse)
summary(multiple.regression)

# From the initial multiple regression, some variables have a high p value, therefore they are insignificant to the model #

multiple.regression2 <- lm(price ~ bedrooms + bathrooms + sqft_living + condition, data = new_cleanhouse)
summary(multiple.regression2)

# The second multiple regression contains all the relevant variables as they all have low p values #


# PCA Analysis #
# We will use PCA Analysis to see which city has the most expensive house #

relevant_var <- clean_house[c(1,4,9,14)]
city_agg <- aggregate(relevant_var, by = list(relevant_var$city), FUN = mean)
city_aggclean <- city_agg[-5]

# Change the column of the cities to an index in order for PCA analysis to occur #
index_city <- city_aggclean[,-1]
rownames(index_city) <- city_aggclean[,1]

pc_city <- prcomp(index_city, center = T, scale. = T)

# The following code below shows the proportion of explained variance which means the amount of variance that can be explained by the model #
# The higher the proportion, the better the model and here since we are going to be using a biplot, we will only look at PC1 & PC2. This explains around 90% of variance from the model #
pc_city_var <- pc_city$sdev^2
pc_city_PEV <- pc_city_var / sum(pc_city_var)

opar <- par()
plot(
  cumsum(pc_city_PEV),
  ylim = c(0,1),
  xlab = 'PC',
  ylab = 'cumulative PEV',
  pch = 20,
  col = 'orange'
)

# The abline function shows the minimum amount of variance required for the model to be sufficient using a dashed line #
abline(h = 0.8, col = 'red', lty = 'dashed')
par(opar)


biplot(
  pc_city,
  choices = c(1,2),
  scale = 0,
  col = c('grey40','orange')
)

# The bottom and left of the graph represent the PCA scores of each city and the top and right represent the loadings plot which is the influence of each variable #
# Price and sqft_living are positively correlated. If we look at the cities which are the most influenced by price
# we see that Mercer Island and Yarrow Point are heavily influenced.

head(city_aggclean[order(city_aggclean$price,decreasing = T),])

# If we look at this, we can see that Yarrow Point and Mercer Island are among the most (Top 6) expensive houses # 

