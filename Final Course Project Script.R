# Prepare Environment
rm(list = ls()) # Clear environment
gc()            # Clear memory
cat("\f") 

packages <- c("ISLR" # Datasets that are used in our textbook
              , "stargazer" # Nice summary tables
              , "ggplot2" # Best plotting
              , "glmnet"
              , "gridExtra"
              , "leaps"
)
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i]
                     , repos = "http://cran.rstudio.com/"
                     , dependencies = TRUE
    )
  }
  library(packages[i], character.only=TRUE)
}
rm(packages)
library(dplyr)

load("C:/Users/jshrb/Downloads/iowa.full.RData")

# Summary

summary(iowa)

#Creating new variables

iowa$per.capita.sales = iowa$sale.dollars / iowa$population
iowa$category.misc <- ifelse(iowa$category == "Misc", 1, 0)
iowa$category.vodka <- ifelse(iowa$category == "Vodka", 1, 0)
iowa$category.tequila <- ifelse(iowa$category == "Tequila", 1, 0)
iowa$category.amaretto <- ifelse(iowa$category == "Amaretto", 1, 0)
iowa$category.gin <- ifelse(iowa$category == "Gin", 1, 0)
iowa$category.brandy <- ifelse(iowa$category == "Brandy", 1, 0)
iowa$category.schnapps <- ifelse(iowa$category == "Schnapps", 1, 0)
iowa$category.rum <- ifelse(iowa$category == "Rum", 1, 0)
iowa$category.whisky <- ifelse(iowa$category == "Whisky", 1, 0)
iowa$category.spirits <- ifelse(iowa$category == "Distilled Spirits", 1, 0)
iowa$income.thousands = iowa$income / 1000
iowa$percent.high.school = iowa$high.school / 100
iowa$percent.bachelor = iowa$bachelor / 100 
iowa$percent.unemployment = iowa$unemployment / 100

stargazer(iowa 
          , summary = TRUE   # We want summary only
          , type = "html" # Type of output - text, HTML or LaTeX
          , title = "Descriptive statistics" #Title of my output
          , digits = 2
          , out = "summaryfinalvideo.html" # File name
)


iowa <- na.omit(iowa)

# Graphs

# INCOME

iowa.byincome <- aggregate(iowa$income, by=list(city=iowa$city), FUN=mean)

ggplot(iowa.byincome, aes(x = x)) +
  geom_histogram(bins = 30
                 , color = "darkblue"
                 , fill = "lightblue"
  ) +
  # Adjust y-axis ticks
  scale_y_continuous(breaks = seq(from = 0        
                                  , to = 300
                                  , by = 50
  )
  ) +
  # Adjust x-axis ticks
  scale_x_continuous(breaks = seq(from = 0        
                                  , to = 100000
                                  , by = 10000
  )
  ) +
  labs(title = "Distribution of Median Income"
       , subtitle = ""
       , x = "Median Income"
       , y = "Number of Cities"
  ) +
  # Apply black-and-white theme
  theme_bw()


# HIGH SCHOOL

iowa.byhs <- aggregate(iowa$high.school, by=list(city=iowa$city), FUN=mean)

ggplot(iowa.byhs, aes(x = x)) +
  geom_histogram(bins = 30
                 , color = "darkblue"
                 , fill = "lightblue"
  ) +
  # Adjust y-axis ticks
  scale_y_continuous(breaks = seq(from = 0        
                                  , to = 400
                                  , by = 50
  )
  ) +
  # Adjust x-axis ticks
  scale_x_continuous(breaks = seq(from = 0        
                                  , to = 100
                                  , by = 10
  )
  ) +
  labs(title = "Distribution of High School Degrees"
       , subtitle = ""
       , x = "Percentage of High School Degrees"
       , y = "Number of Cities"
  ) +
  # Apply black-and-white theme
  theme_bw()


# Liquor Category Scatter Plot

iowa.liquor <- aggregate(iowa$sale.dollars, by=list(category = iowa$category), FUN=mean)

ggplot(iowa.liquor, aes(x = x, y = category)) +
  geom_point(color = "darkgreen"  # Color of points' outline
             , fill = "darkblue"       # Color of points' fill
             , shape = 21         # Shape of points
             , size = 2           # Size of points
             , alpha = 1          # Transparency of points
  ) +
  labs(title = "Most Popular Liquor Categories"
       , subtitle = ""
       , x = "Mean Sales (dollars)"
       , y = "Type of Liquor"
  ) +
  theme_bw()


# Population Scatter Plot

iowa.pop <- aggregate(iowa$sale.dollars, by=list(population = iowa$population), FUN=mean)

ggplot(iowa.pop, aes(x = x, y = population)) +
  geom_point(color = "darkgreen"  # Color of points' outline
             , fill = "darkblue"       # Color of points' fill
             , shape = 21         # Shape of points
             , size = 2           # Size of points
             , alpha = 1          # Transparency of points
  ) +
  labs(title = "Liquor Sales by Population"
       , subtitle = ""
       , x = "Mean Sales (dollars)"
       , y = "Population"
  ) +
  theme_bw()

#Inference model 

inference = lm(per.capita.sales ~ percent.high.school + percent.bachelor 
               + percent.unemployment + income.thousands + category.vodka 
               + category.tequila + category.amaretto + category.gin 
               + category.brandy + category.schnapps + category.rum
               + category.whisky + category.spirits
               , data = iowa)

#Prediction Model 

drop.vars <- c("sale.dollars", "population", "city", "county", "zipcode", "category", "income"
               , "bachelor", "high.school", "unemployment", "pop.white", "pop.black", "pop.indian",
               "pop.asian", "pop,multi", "pop.other", "pop.hawai"
) 

iowa[, drop.vars] <- NULL

rows.train <- sample(nrow(iowa), 0.8*nrow(iowa))
rows.train
data.train <- iowa[rows.train,]
data.test <- iowa[-rows.train,]

data.full.train <- model.matrix(per.capita.sales ~ .^2, data.train)
View(data.full.train)
data.full.train <- as.data.frame(data.full.train[,-1]) # -1 to remove the intercept
data.full.train$per.capita.sales <- data.train$per.capita.sales

alc.reg5 <- regsubsets(per.capita.sales ~ ., data.full.train
                       , nvmax = NULL, method = "forward"
)
reg5.summary <- summary(alc.reg5)
reg5.summary$adjr2
which.max(reg5.summary$adjr2)
round(coef(alc.reg5, which.max(reg5.summary$adjr2)),5)

alc.reg6 <- regsubsets(per.capita.sales ~ ., data.full.train
                       , nvmax = NULL, method = "backward"
)
reg6.summary <- summary(alc.reg6)
reg6.summary$adjr2
which.max(reg6.summary$adjr2)
round(coef(alc.reg6, which.max(reg6.summary$adjr2)),5)

# How can we select best models from each and have them be estimated directly?
# So that we can get all the stats, not just coeffs?
# We could extract those manually given what we know
reg6.summary$which[which.max(reg6.summary$adjr2),-1] # All X variables except intercept from model with highest R^2adj
vars <- which(reg6.summary$which[which.max(reg6.summary$adjr2),-1] == TRUE)
vars
names(vars)
formula <- paste(names(vars), collapse = " + ")
formula

get.model.formula <- function(id, reg, outcome){
  # Identify all variables 
  vars <- summary(reg)$which[id,-1]
  # Get model predictors used in model with id
  predictors <- names(which(vars == TRUE))
  predictors <- paste(predictors, collapse = " + ")
  # Build model formula
  formula <- as.formula(paste0(outcome, " ~ ", predictors))
  return(formula)
}
get.model.formula(which.max(reg6.summary$adjr2)
                  , alc.reg6
                  , "per.capita.sales"
)

# Now let's compare forward vs backward vs standard OLS
reg.best.forward <- lm(get.model.formula(which.max(reg5.summary$adjr2)
                                         , alc.reg5
                                         , "per.capita.sales"
)
, data.full.train
)
summary(reg.best.forward)
reg.best.backward <- lm(get.model.formula(which.max(reg6.summary$adjr2)
                                          , alc.reg6
                                          , "per.capita.sales"
)
, data.full.train
)
summary(reg.best.backward)
# Standard OLS model with all variables
reg.best.OLS <- lm(per.capita.sales ~ ., data.full.train)
summary(reg.best.OLS)

# Compare models
stargazer(reg.best.OLS, reg.best.forward, reg.best.backward
          , column.labels = c("OLS", "Forward", "Backward")
          , dep.var.labels.include = FALSE
          , dep.var.caption  = ""
          , type = "html", style = "default", digits = 2
          , no.space = TRUE, report = "vc*", omit.stat = c("ser","f", "rsq")
          , align = TRUE, single.row = TRUE, multicolumn = TRUE
          , out = "backandfregs.html"
)


results.ols <- data.frame(feature = names(reg.best.OLS$coefficients)[-1] # -1 to exclude intercept
                          , ols = round(coef(reg.best.OLS)[-1], 5) # -1 to exclude intercept
)
View(results.ols)
results.best.f <- data.frame(feature = names(reg.best.forward$coefficients)[-1]
                             , best.f = round(coef(reg.best.forward)[-1], 5)
)
View(results.best.f)
results.best.b <- data.frame(feature = names(reg.best.backward$coefficients)[-1]
                             , best.b = round(coef(reg.best.backward)[-1], 5)
)
View(results.best.b)
# How can we combine these together?
# We could create a loop to run through one variable at a time
# So that we can account for different sets of variables between models
# But a much better way is to do a merge
results <- merge(results.ols, results.best.f
                 , by = "feature"
                 , all.x = TRUE, all.y = TRUE
)
View(results)
# And another merge, this time with backward
results <- merge(results, results.best.b
                 , by = "feature"
                 , all.x = TRUE, all.y = TRUE
)
View(results) 
# Need to remove unnecessary single quotes from feature names
results$feature <- gsub("`", "", results$feature)

data.ridge1 <- model.matrix(per.capita.sales ~ ., data.train)[,-1]
str(data.ridge1)
View(data.ridge1) 

# Special syntax for glmnet():
# x - matrix/dataframe with X variables or a model.matrix object
# y - vector/dataframe with the outcome variable
alc.reg.ridge1 <- glmnet(x = data.ridge1
                         , y = data.train$per.capita.sales # Need to specify outcome variable directly
                         , alpha = 0 # alpha = 0 means Ridge regression
)

coef(alc.reg.ridge1)

print(alc.reg.ridge1)
# Here are coefficient values as function of lambda
plot(alc.reg.ridge1, xvar = "lambda", label = TRUE)
# As expected, the higher the lambda, the higher the ridge penalty,
# the higher the shrinkage effect

# To extract coefficients for a specific model,
# we need to specify which value of labmbda we want
# Take, for example, value of 4016
coef(alc.reg.ridge1, s = 4016)
# Or value 1864000
coef(alc.reg.ridge1, s = 1864000)

# Which value to choose? Which model is the best?
# This can only be answered using CV

# glmnet has a built-in CV routine to select best lambda values
alc.reg.ridge2 <- cv.glmnet(x = data.ridge1
                            , y = data.train$per.capita.sales
                            , alpha = 0 # alpha = 0 means Ridge regression, default is 1 for Lasso
)
plot(alc.reg.ridge2)  

# The leftmost vertical line corresponds to labmda that minimizes CV error:
alc.reg.ridge2$lambda.min
# Model's coefficients with this lambda are 
coef(alc.reg.ridge2, s = "lambda.min")

# Note: nowhere in glmnet() output do we see significance results
# That's because estimates' distributions are non-standard
# There are some theoretical results, but the most common way is to do bootsrap

# Let's try running the model with all possible interactions via ridge
# And then compare the results to backward/forward selection
data.ridge2 <- model.matrix(per.capita.sales ~ .^2, data.train)[,-1] #-1 to remove an intercept
str(data.ridge2) # This time we don't convert it to dataframe, since glmnet() doesn't like it
View(data.ridge2)  
reg.best.ridge <- cv.glmnet(x = data.ridge2
                            , y = data.train$per.capita.sales
                            , alpha = 0
)
plot(reg.best.ridge)
reg.best.ridge$lambda.min
coef(reg.best.ridge, s = "lambda.min")
# Let's create a dataframe with ridge coefficients
# Note that coef() for ridge outputs a special object, not a usual vector
results.best.ridge <- data.frame(feature = names(coef(reg.best.ridge, s = "lambda.min")[-1,1])
                                 , ridge = round(coef(reg.best.ridge, s = "lambda.min")[-1,1], 5)
)
# And another merge, this time with ridge
results <- merge(results, results.best.ridge
                 , by = "feature"
                 , all.x = TRUE, all.y = TRUE
)

data.lasso1 <- model.matrix(per.capita.sales ~ ., data.train)[,-1] #-1 to remove an intercept
alc.reg.lasso1 <- glmnet(x = data.lasso1
                         , y = data.train$per.capita.sales # Need to specify outcome variable directly
                         , alpha = 1 # alpha = 1 means lasso regression
)
print(alc.reg.lasso1)
# Here are coefficient values as function of lambda
plot(alc.reg.lasso1, xvar = "lambda", label = TRUE) 
# The difference is that now some of them are shrunk to exactly zero

# Just like with ridge, choosing the correct lamba is crucial
# So we use CV:
alc.reg.lasso2 <- cv.glmnet(x = data.lasso1
                            , y = data.train$per.capita.sales
                            , alpha = 1
)
plot(alc.reg.lasso2)  
# The leftmost vertical line corresponds to labmda that minimizes CV error:
alc.reg.lasso2$lambda.min
# Model's coefficients with this lambda are 
coef(alc.reg.lasso2, s = "lambda.min")
# As we can see, one coefficient has been shrunk exactly to zero

# Let's try running the model with all possible interactions via lasso
# And then compare the results to backward/forward selection and ridge
data.lasso2 <- model.matrix(per.capita.sales ~ .^2, data.train)[,-1] #-1 to remove an intercept
reg.best.lasso <- cv.glmnet(x = data.lasso2
                            , y = data.train$per.capita.sales
                            , alpha = 1
)
plot(reg.best.lasso)
reg.best.lasso$lambda.min
coef(reg.best.lasso, s = "lambda.min")
# Store coefficients
# Unlike before, we do not round lasso coefficients,
# Because otherwise we cannot distinguish between exactly zero and near zero
results.best.lasso <- data.frame(feature = names(coef(reg.best.lasso, s = "lambda.min")[-1,1])
                                 , lasso = coef(reg.best.lasso, s = "lambda.min")[-1,1]
)
View(results.best.lasso)
# Once we saved them, we can filter out exact zeros
results.best.lasso$lasso[results.best.lasso$lasso == 0] <- NA
# And then round the remaining one to 5 digits
results.best.lasso$lasso <- round(results.best.lasso$lasso, 5)

# Final merge
results <- merge(results, results.best.lasso
                 , by = "feature"
                 , all.x = TRUE, all.y = TRUE
)
# Nice output table
stargazer(results, summary = FALSE
          , type = "html", style = "default", digits = 5
          , no.space = TRUE, report = "vc*", omit.stat = c("ser","f", "rsq")
          , align = TRUE, single.row = TRUE, multicolumn = TRUE
          , out = "lassoandridge.html"
)

data.full.test <- model.matrix(per.capita.sales ~ .^2, data.test)[,-1]
prediction <- data.frame(per.capita.sales = data.test$per.capita.sales)
prediction$alc.OLS <- predict(reg.best.OLS, newdata = as.data.frame(data.full.test))
prediction$alc.b <- predict(reg.best.backward, newdata = as.data.frame(data.full.test))
prediction$alc.f <- predict(reg.best.forward, newdata = as.data.frame(data.full.test))
prediction$alc.ridge <- as.numeric(predict(reg.best.ridge
                                           , newx = data.full.test
                                           , s = "lambda.min"
                                           , type = "response"
)
)
prediction$alc.lasso <- as.numeric(predict(reg.best.lasso
                                           , newx = data.full.test
                                           , s = "lambda.min"
                                           , type = "response"
)
)
# Calculate MSE:




accuracy <- data.frame(model = c("OLS"
                                 , "Backward"
                                 , "Forward"
                                 , "Ridge"
                                 , "Lasso"
)
, MSE = NA
)



accuracy$MSE[accuracy$model == "OLS"] <- mean((prediction$per.capita.sales - prediction$alc.OLS)^2)
accuracy$MSE[accuracy$model == "Backward"] <- mean((prediction$per.capita.sales - prediction$alc.b)^2)
accuracy$MSE[accuracy$model == "Forward"] <- mean((prediction$per.capita.sales - prediction$alc.f)^2)
accuracy$MSE[accuracy$model == "Ridge"] <- mean((prediction$per.capita.sales - prediction$alc.ridge)^2)
accuracy$MSE[accuracy$model == "Lasso"] <- mean((prediction$per.capita.sales - prediction$alc.lasso)^2)

accuracy

# Relative MSE of everybody with OLS as a baseline
accuracy$MSE.relative <- accuracy$MSE/accuracy$MSE[accuracy$model == "OLS"]
accuracy

stargazer(accuracy
          , column.labels = c("OLS", "Forward", "Backward", "Ridge", "Lasso")
          , dep.var.labels.include = FALSE
          , dep.var.caption  = ""
          , type = "html", style = "default", digits = 2
          , no.space = TRUE, report = "vc*", omit.stat = c("ser","f", "rsq")
          , align = TRUE, single.row = TRUE, multicolumn = TRUE
          , out = "everything.html"
)
