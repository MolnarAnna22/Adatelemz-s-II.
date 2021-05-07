library(haven)
library(ROCR)
library(grid)
library(broom)
library(caret)
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(data.table)
#install.packages("data.table", repos="http://R-Forge.R-project.org")

telco <- read_sav("C:/Users/THINKPAD/Downloads/telco_0507.sav")
str(telco)

df <- telco[c("employ", "equipmon", "tenure_bin5", "churn")]

findCorrelation(cor(df), cutoff = .75, names = TRUE )
prop.table(table(df$churn))

# convert to factor variables
df$tenure_bin5 <- factor(df$tenure_bin5)

set.seed(4321)
test <- createDataPartition(df$churn, p = .4, list = FALSE)
data_train <- df[-test, ]
data_test  <- df[test, ]
rm(df)

# traing logistic regression model
model_glm <- glm(churn ~ . , data = data_train, family = binomial(logit) )
model_glm
summary_glm <- summary(model_glm)

fit <- model_glm$fitted
hist(fit)

list( summary_glm$coefficient, 
      round( 1 - ( summary_glm$deviance / summary_glm$null.deviance ), 2 ) )

# prediction
data_train$prediction <- predict(model_glm, newdata = data_train, type = "response" )
data_test$prediction  <- predict(model_glm, newdata = data_test , type = "response" )

# distribution of the prediction score grouped by known outcome
ggplot( data_train, aes( prediction, color = as.factor(churn) ) ) + 
  geom_density( size = 1 ) +
  ggtitle( "Training Set's Predicted Score" ) + 
  scale_color_economist( name = "data", labels = c( "negative", "positive" ) ) + 
  theme_economist()

#continuous
df <- telco[c("employ", "equipmon", "tenure", "churn")]

findCorrelation(cor(df), cutoff = .75, names = TRUE )
prop.table(table(df$churn))

set.seed(4321)
test <- createDataPartition(df$churn, p = .4, list = FALSE)
data_train <- df[-test, ]
data_test  <- df[test, ]
rm(df)

# traing logistic regression model
model_glm <- glm(churn ~ . , data = data_train, family = binomial(logit) )
model_glm
summary_glm <- summary(model_glm)

fit <- model_glm$fitted
hist(fit)

list( summary_glm$coefficient, 
      round( 1 - ( summary_glm$deviance / summary_glm$null.deviance ), 2 ) )

# prediction
data_train$prediction <- predict(model_glm, newdata = data_train, type = "response" )
data_test$prediction  <- predict(model_glm, newdata = data_test , type = "response" )

# distribution of the prediction score grouped by known outcome
ggplot( data_train, aes( prediction, color = as.factor(churn) ) ) + 
  geom_density( size = 1 ) +
  ggtitle( "Training Set's Predicted Score" ) + 
  scale_color_economist( name = "data", labels = c( "negative", "positive" ) ) + 
  theme_economist()
