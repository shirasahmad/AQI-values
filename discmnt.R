India=discmnt
India
library(tidyverse)
library(MASS)
str(India)
head(India)


library(psych)
pairs.panels(India[3:7],gap=0)

set.seed(123)
train_index=sample(1:nrow(India),0.7 * nrow(India))
train_data=India[train_index, ]
test_data=India[-train_index, ]

AQIcat=India$`AQI Category`
AQI=India$`AQI Value`
co=India$`CO AQI Value`
no2=India$`NO2 AQI Value`
ozone=India$`Ozone AQI Value`
pm=India$`PM2.5 AQI Value`

names(train_data)
lda_model=lda(`AQI Category`~`AQI Value`+`CO AQI Value`+`Ozone AQI Value`+`NO2 AQI Value`+`PM2.5 AQI Value`,data = train_data)
summary(lda_model)

lda_predictions=predict(lda_model,newdata = test_data)
lda_predictions

confusion_matrix=table(lda_predictions$class,test_data$`AQI Category`)
confusion_matrix

accuracy=sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

coefficients=coef(lda_model)
coefficients

variable_importance=lda_model$svd^2 / sum(lda_model$svd^2)
variable_importance

plot(lda_model)
