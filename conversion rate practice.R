library(dplyr)
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)
library(DT)
library(GGally)
library(randomForest)
library(magrittr)
library(dplyr)
library(rpart)
library(rpart.plot)



c_data <- read.csv("/Users/OOO/Downloads/DS_Challenges/Conversion_Rate/conversion_project.csv")
summary(c_data)
sort(unique(c_data))
subset(c_data, age>80)
c_data = subset(c_data, age < 80)
summary(c_data)
data_country <- c_data %>% 
               group_by(country)%>%
               summarise(converted = mean(converted))
ggplot(data = data_country, aes(x = country, y = converted))+geom_bar(stat = "identity", aes(fill = country))
data_pages = c_data %>%
             group_by(total_pages_visited)%>%
             summarise(converted = mean(converted))
qplot(total_pages_visited, converted, data = data_pages, geom = "line")

#model
c_data$converted = as.factor(c_data$converted)
c_data$new_user = as.factor(c_data$new_user)
train_sample = sample(nrow(c_data), size = nrow(c_data)*0.7)
train_data = c_data[train_sample,]
test_data = c_data[-train_sample,]

rf = randomForest(y = train_data$converted, x = train_data[,-ncol(train_data)],
                  y_test = test_data$converted, xtest = test_data[, -ncol(test_data)],
                  ntree = 100, mtry = 3, keep.forest = TRUE)
rf

varImpPlot(rf, type = 2)

rf2 = randomForest(y = train_data$converted, x = train_data[, -c(5, ncol(train_data))],
                   y_test = test_data$converted, xtest = test_data[, -c(5, ncol(train_data))],
                   ntree = 100, mtry = 3, keep.forest = TRUE, classwt = c(0.7,0.3))
rf2
varImpPlot(rf2, type = 2)
op <- par(mfrow = c(2,2))
partialPlot(rf2, train_data, country, 1) 
partialPlot(rf2, train_data, age, 1) 
partialPlot(rf2, train_data, new_user, 1) 
partialPlot(rf2, train_data, source, 1)
#build tree for rf2 important features
tree = rpart(c_data$converted ~ ., c_data[, -c(5, ncol(c_data))],
             control = rpart.control(maxdepth = 3),
             parms = list(prior = c(0.7, 0.3)))
tree
rpart.plot(tree)
