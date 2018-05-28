library(tidyverse)
library(stringr)
library(jsonlite)
library(data.table)
library(rwunderground)
library(rpart)
library(randomForest)


joined <- 1
weather.read <- function(start,end){
  if (is.null(joined)){
    cp <- history_range("OH/Cincinnati", date_start = start, date_end = end,
                                 limit = 10, no_api = FALSE, use_metric = FALSE, key = "bb0b92b68f5a48e9",
                                 raw = FALSE, message = TRUE)
    cp$precip <- ifelse(is.na(cp$precip),0,cp$precip)
    cp$day <- vapply(str_split(cp$date," "),`[`, 1, FUN.VALUE=character(1))
    cp$time <- vapply(str_split(cp$date," "),`[`, 2, FUN.VALUE=character(1))
    cp <- distinct(cp, day, substr(cp$time,1,2) ,.keep_all=TRUE)
    colnames(cp)[ncol(cp)] <- "hour"
    cp <- unite(cp,index,day,hour)
    cp <- cp[,c(2:10,12,15,22)]
    cp <- select(cp, index,everything())
    colnames(cp) <- c("index", "cp.temp", "cp.dew_pt","cp.hum","cp.wind_speed","cp.wind_gust","cp.dir","cp.vis",
                         "cp.pressure","cp.wind_chill","cp.precip","cp.cond")
    
    c1 <- history_range("KY/Lexington", date_start = start, date_end = end,
                        limit = 10, no_api = FALSE, use_metric = FALSE, key = "bb0b92b68f5a48e9",
                        raw = FALSE, message = TRUE)
    c1$precip <- ifelse(is.na(c1$precip),0,c1$precip)
    c1$day <- vapply(str_split(c1$date," "),`[`, 1, FUN.VALUE=character(1))
    c1$time <- vapply(str_split(c1$date," "),`[`, 2, FUN.VALUE=character(1))
    c1 <- distinct(c1, day, substr(c1$time,1,2) ,.keep_all=TRUE)
    colnames(c1)[ncol(c1)] <- "hour"
    c1 <- unite(c1,index,day,hour)
    c1 <- c1[,c(2:10,12,15,22)]
    c1 <- select(c1, index,everything())
    colnames(c1) <- c("index", "c1.temp", "c1.dew_pt","c1.hum","c1.wind_speed","c1.wind_gust","c1.dir","c1.vis",
                      "c1.pressure","c1.wind_chill","c1.precip","c1.cond")
    
    c2 <- history_range("KY/Louisville", date_start = start, date_end = end,
                        limit = 10, no_api = FALSE, use_metric = FALSE, key = "bb0b92b68f5a48e9",
                        raw = FALSE, message = TRUE)
    c2$precip <- ifelse(is.na(c2$precip),0,c2$precip)
    c2$day <- vapply(str_split(c2$date," "),`[`, 1, FUN.VALUE=character(1))
    c2$time <- vapply(str_split(c2$date," "),`[`, 2, FUN.VALUE=character(1))
    c2 <- distinct(c2, day, substr(c2$time,1,2) ,.keep_all=TRUE)
    colnames(c2)[ncol(c2)] <- "hour"
    c2 <- unite(c2,index,day,hour)
    c2 <- c2[,c(2:10,12,15,22)]
    c2 <- select(c2, index,everything())
    colnames(c2) <- c("index", "c2.temp", "c2.dew_pt","c2.hum","c2.wind_speed","c2.wind_gust","c2.dir","c2.vis",
                      "c2.pressure","c2.wind_chill","c2.precip","c2.cond")
    
    c3 <- history_range("OH/Columbus", date_start = start, date_end = end,
                        limit = 10, no_api = FALSE, use_metric = FALSE, key = "bb0b92b68f5a48e9",
                        raw = FALSE, message = TRUE)
    c3$precip <- ifelse(is.na(c3$precip),0,c3$precip)
    c3$day <- vapply(str_split(c3$date," "),`[`, 1, FUN.VALUE=character(1))
    c3$time <- vapply(str_split(c3$date," "),`[`, 2, FUN.VALUE=character(1))
    c3 <- distinct(c3, day, substr(c3$time,1,2) ,.keep_all=TRUE)
    colnames(c3)[ncol(c3)] <- "hour"
    c3 <- unite(c3,index,day,hour)
    c3 <- c3[,c(2:10,12,15,22)]
    c3 <- select(c3, index,everything())
    colnames(c3) <- c("index", "c3.temp", "c3.dew_pt","c3.hum","c3.wind_speed","c3.wind_gust","c3.dir","c3.vis",
                      "c3.pressure","c3.wind_chill","c3.precip","c3.cond")
    
    c4 <- history_range("IN/Indianapolis", date_start = start, date_end = end,
                        limit = 10, no_api = FALSE, use_metric = FALSE, key = "bb0b92b68f5a48e9",
                        raw = FALSE, message = TRUE)
    c4$precip <- ifelse(is.na(c4$precip),0,c4$precip)
    c4$day <- vapply(str_split(c4$date," "),`[`, 1, FUN.VALUE=character(1))
    c4$time <- vapply(str_split(c4$date," "),`[`, 2, FUN.VALUE=character(1))
    c4 <- distinct(c4, day, substr(c4$time,1,2) ,.keep_all=TRUE)
    colnames(c4)[ncol(c4)] <- "hour"
    c4 <- unite(c4,index,day,hour)
    c4 <- c4[,c(2:10,12,15,22)]
    c4 <- select(c4, index,everything())
    colnames(c4) <- c("index", "c4.temp", "c4.dew_pt","c4.hum","c4.wind_speed","c4.wind_gust","c4.dir","c4.vis",
                      "c4.pressure","c4.wind_chill","c4.precip","c4.cond")
    
    joined <- left_join(cp,c1,by = "index") %>% left_join(c2,by = "index") %>% 
      left_join(c3,by = "index") %>% left_join(c4,by = "index")
    
    write_csv(joined,"combined.csv",append=FALSE, col_names = TRUE)
    return(joined)
  } else {
    cp <- history_range("OH/Cincinnati", date_start = start, date_end = end,
                        limit = 10, no_api = FALSE, use_metric = FALSE, key = "bb0b92b68f5a48e9",
                        raw = FALSE, message = TRUE)
    cp$precip <- ifelse(is.na(cp$precip),0,cp$precip)
    cp$day <- vapply(str_split(cp$date," "),`[`, 1, FUN.VALUE=character(1))
    cp$time <- vapply(str_split(cp$date," "),`[`, 2, FUN.VALUE=character(1))
    cp <- distinct(cp, day, substr(cp$time,1,2) ,.keep_all=TRUE)
    colnames(cp)[ncol(cp)] <- "hour"
    cp <- unite(cp,index,day,hour)
    cp <- cp[,c(2:10,12,15,22)]
    cp <- select(cp, index,everything())
    colnames(cp) <- c("index", "cp.temp", "cp.dew_pt","cp.hum","cp.wind_speed","cp.wind_gust","cp.dir","cp.vis",
                      "cp.pressure","cp.wind_chill","cp.precip","cp.cond")
    
    c1 <- history_range("KY/Lexington", date_start = start, date_end = end,
                        limit = 10, no_api = FALSE, use_metric = FALSE, key = "bb0b92b68f5a48e9",
                        raw = FALSE, message = TRUE)
    c1$precip <- ifelse(is.na(c1$precip),0,c1$precip)
    c1$day <- vapply(str_split(c1$date," "),`[`, 1, FUN.VALUE=character(1))
    c1$time <- vapply(str_split(c1$date," "),`[`, 2, FUN.VALUE=character(1))
    c1 <- distinct(c1, day, substr(c1$time,1,2) ,.keep_all=TRUE)
    colnames(c1)[ncol(c1)] <- "hour"
    c1 <- unite(c1,index,day,hour)
    c1 <- c1[,c(2:10,12,15,22)]
    c1 <- select(c1, index,everything())
    colnames(c1) <- c("index", "c1.temp", "c1.dew_pt","c1.hum","c1.wind_speed","c1.wind_gust","c1.dir","c1.vis",
                      "c1.pressure","c1.wind_chill","c1.precip","c1.cond")
    
    c2 <- history_range("KY/Louisville", date_start = start, date_end = end,
                        limit = 10, no_api = FALSE, use_metric = FALSE, key = "bb0b92b68f5a48e9",
                        raw = FALSE, message = TRUE)
    c2$precip <- ifelse(is.na(c2$precip),0,c2$precip)
    c2$day <- vapply(str_split(c2$date," "),`[`, 1, FUN.VALUE=character(1))
    c2$time <- vapply(str_split(c2$date," "),`[`, 2, FUN.VALUE=character(1))
    c2 <- distinct(c2, day, substr(c2$time,1,2) ,.keep_all=TRUE)
    colnames(c2)[ncol(c2)] <- "hour"
    c2 <- unite(c2,index,day,hour)
    c2 <- c2[,c(2:10,12,15,22)]
    c2 <- select(c2, index,everything())
    colnames(c2) <- c("index", "c2.temp", "c2.dew_pt","c2.hum","c2.wind_speed","c2.wind_gust","c2.dir","c2.vis",
                      "c2.pressure","c2.wind_chill","c2.precip","c2.cond")
    
    c3 <- history_range("OH/Columbus", date_start = start, date_end = end,
                        limit = 10, no_api = FALSE, use_metric = FALSE, key = "bb0b92b68f5a48e9",
                        raw = FALSE, message = TRUE)
    c3$precip <- ifelse(is.na(c3$precip),0,c3$precip)
    c3$day <- vapply(str_split(c3$date," "),`[`, 1, FUN.VALUE=character(1))
    c3$time <- vapply(str_split(c3$date," "),`[`, 2, FUN.VALUE=character(1))
    c3 <- distinct(c3, day, substr(c3$time,1,2) ,.keep_all=TRUE)
    colnames(c3)[ncol(c3)] <- "hour"
    c3 <- unite(c3,index,day,hour)
    c3 <- c3[,c(2:10,12,15,22)]
    c3 <- select(c3, index,everything())
    colnames(c3) <- c("index", "c3.temp", "c3.dew_pt","c3.hum","c3.wind_speed","c3.wind_gust","c3.dir","c3.vis",
                      "c3.pressure","c3.wind_chill","c3.precip","c3.cond")
    
    c4 <- history_range("IN/Indianapolis", date_start = start, date_end = end,
                        limit = 10, no_api = FALSE, use_metric = FALSE, key = "bb0b92b68f5a48e9",
                        raw = FALSE, message = TRUE)
    c4$precip <- ifelse(is.na(c4$precip),0,c4$precip)
    c4$day <- vapply(str_split(c4$date," "),`[`, 1, FUN.VALUE=character(1))
    c4$time <- vapply(str_split(c4$date," "),`[`, 2, FUN.VALUE=character(1))
    c4 <- distinct(c4, day, substr(c4$time,1,2) ,.keep_all=TRUE)
    colnames(c4)[ncol(c4)] <- "hour"
    c4 <- unite(c4,index,day,hour)
    c4 <- c4[,c(2:10,12,15,22)]
    c4 <- select(c4, index,everything())
    colnames(c4) <- c("index", "c4.temp", "c4.dew_pt","c4.hum","c4.wind_speed","c4.wind_gust","c4.dir","c4.vis",
                      "c4.pressure","c4.wind_chill","c4.precip","c4.cond")
    
    joined <- left_join(cp,c1,by = "index") %>% left_join(c2,by = "index") %>% 
      left_join(c3,by = "index") %>% left_join(c4,by = "index")
    
    write_csv(joined,"combined.csv",append=TRUE, col_names = F)
  }
}

#start with "20180201", end with "20180401"

weather.read("20180331","20180401")

train <- read.csv("combined.csv")


train[train == "Fog"] <- "Overcast"
train[train == "Haze"] <- "Overcast"
train[train == "Heavy Rain"] <- "Rain"
train[train == "Heavy Snow"] <- "Snow"
train[train == "Light Freezing Rain"] <- "Light Rain"
train$c1.cond <- as.character(train$c1.cond)
train[train == "Light Snow"] <- "Snow"
train$c1.cond <- as.factor(train$c1.cond)
train[train == "Scattered Clouds"] <- "Partly Cloudy"
train[train == "Thunderstorm"] <- "Rain"
train[train == "Unknown"] <- "Overcast"
train[train == "Light Freezing Fog"] <- "Overcast"
train[train == "Mist"] <- "Light Rain"
train[train == "Heavy Thunderstorms and Rain"] <- "Rain"
train[train == "Freezing Rain"] <- "Rain"
train[train == "Ice Pellets"] <- "Snow"

train <- train[complete.cases(train[,"c1.cond"]),]

##########################################################
colSums(is.na(train))
train <- train[complete.cases(train[,c("c1.temp","c2.temp","c3.temp","c4.temp","cp.dew_pt","cp.hum","cp.vis")]),]
just.temp <- lm(cp.temp ~ c1.temp + c2.temp + c3.temp + c4.temp, data = train)
summary(just.temp)
temp.full <- lm(cp.temp ~ c1.temp + c2.temp + c3.temp + c4.temp + c1.hum + c2.hum + 
                  c3.hum + c4.hum + c1.pressure + 
                  c2.pressure + c3.pressure + c4.pressure + c1.dew_pt +
                  c2.dew_pt + c3.dew_pt + c4.dew_pt, data = train)
summary(temp.full)
temp.null <- lm(cp.temp ~ 1, data = train)
model.step.s<- step(temp.null, scope=list(lower=temp.null, upper=temp.full), direction='both')
summary(model.step.s)
#all temps, all hum, c1,2,3.dewpt

summary(train.tree$cp.cond)
train.tree <- train[,-(2:11)]
tree <- randomForest(cp.cond ~ ., data = data.tree)
print(tree)
err <- tree$err.rate
oob_err <- err[nrow(err), "OOB"]
print(oob_err)
plot(tree)
legend(x = "right", 
       legend = colnames(err),
       fill = 1:ncol(err))

data.tree <- train[,c(12:15,18,22:26,29,33:37,40,44:48,51,55,56)]
colSums(is.na(data.tree))
# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(data.tree) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(data.tree) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(formula = cp.cond ~ ., 
                        data = data.tree,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])

finaltree <- randomForest(formula = cp.cond ~ ., data = data.tree, mtry = 4, nodesize = 3, sampsize = 1132.8)
print(finaltree)

###############################################################################3
cp.forecast <- hourly10day("OH/Cincinnati",key = "bb0b92b68f5a48e9")
c1.forecast <- hourly10day("KY/Lexington",key = "bb0b92b68f5a48e9")
c2.forecast <- hourly10day("KY/Louisville",key = "bb0b92b68f5a48e9")
c3.forecast <- hourly10day("OH/Columbus",key = "bb0b92b68f5a48e9")
c4.forecast <- hourly10day("IN/Indianapolis",key = "bb0b92b68f5a48e9")

colnames(c1.forecast) <- paste("c1",colnames(c1.forecast), sep = ".")
colnames(c2.forecast) <- paste("c2",colnames(c2.forecast), sep = ".")
colnames(c3.forecast) <- paste("c3",colnames(c3.forecast), sep = ".")
colnames(c4.forecast) <- paste("c4",colnames(c4.forecast), sep = ".")
colnames(cp.forecast) <- paste("cp",colnames(cp.forecast), sep = ".")

test <- cbind(cp.forecast, c1.forecast,c2.forecast,c3.forecast,c4.forecast)
write_csv(test,"testset.csv", col_names = T)
write_csv(cp.forecast, "cpforecast.csv",col_names = T)
test <- read.csv("testset.csv")
cp.test <- read.csv("cpforecast.csv")

colnames(test)[colnames(test)=="c1.humidity"] <- "c1.hum"
colnames(test)[colnames(test)=="c2.humidity"] <- "c2.hum"
colnames(test)[colnames(test)=="c3.humidity"] <- "c3.hum"
colnames(test)[colnames(test)=="c4.humidity"] <- "c4.hum"
colnames(test)[colnames(test)=="c1.wind_dir"] <- "c1.dir"
colnames(test)[colnames(test)=="c2.wind_dir"] <- "c2.dir"
colnames(test)[colnames(test)=="c3.wind_dir"] <- "c3.dir"
colnames(test)[colnames(test)=="c4.wind_dir"] <- "c4.dir"
colnames(test)[colnames(test)=="c1.rain"] <- "c1.precip"
colnames(test)[colnames(test)=="c2.rain"] <- "c2.precip"
colnames(test)[colnames(test)=="c3.rain"] <- "c3.precip"
colnames(test)[colnames(test)=="c4.rain"] <- "c4.precip"
test$c1.cond <- as.character(test$c1.cond)
test$c1.cond[test$c1.cond == "Chance of Rain"] <- "Light Rain"
test$c1.cond <- as.factor(test$c1.cond)
test$c2.cond <- as.character(test$c2.cond)
test$c2.cond[test$c2.cond == "Chance of Rain"] <- "Light Rain"
test$c2.cond <- as.factor(test$c2.cond)
test$c3.cond <- as.character(test$c3.cond)
test$c3.cond[test$c3.cond == "Chance of Rain"] <- "Light Rain"
test$c3.cond <- as.factor(test$c3.cond)
test$c4.cond <- as.character(test$c4.cond)
test$c4.cond[test$c4.cond == "Chance of Rain"] <- "Light Rain"
test$c4.cond <- as.factor(test$c4.cond)
cp.test$cond <- as.character(cp.test$cond)
cp.test$cond[cp.test$cond == "Chance of Rain"] <- "Light Rain"
cp.test$cond <- as.factor(cp.test$cond)
test[test == "E"] <- "East"
test[test == "N"] <- "North"
test[test == "S"] <- "South"
test[test == "W"] <- "West"



predicted.temps <- predict(model.step.s, newdata = test)
test$c1.dir <- as.factor(test$c1.dir)
levels(test$c1.dir) <- levels(train$c1.dir)
test$c2.dir <- as.factor(test$c2.dir)
levels(test$c2.dir) <- levels(train$c2.dir)
test$c3.dir <- as.factor(test$c3.dir)
levels(test$c3.dir) <- levels(train$c3.dir)
test$c4.dir <- as.factor(test$c4.dir)
levels(test$c4.dir) <- levels(train$c4.dir)

test$c1.cond <- as.factor(test$c1.cond)
levels(test$c1.cond) <- levels(train$c1.cond)
test$c2.cond <- as.factor(test$c2.cond)
levels(test$c2.cond) <- levels(train$c2.cond)
test$c3.cond <- as.factor(test$c3.cond)
levels(test$c3.cond) <- levels(train$c3.cond)
test$c4.cond <- as.factor(test$c4.cond)
levels(test$c4.cond) <- levels(train$c4.cond)

predicted.cond <- predict(finaltree, newdata = test.2)
str(test)
str(train.tree)


#############################################################333
library(e1071)

model.svm <- svm(cp.cond ~ ., data = data.tree, cost = 1, kernel = "linear")
summary(model.svm)
pred <- predict(model.svm, data = data.tree)
table(pred = pred, true = data.tree[,1])
classAgreement(table(pred = pred, true = data.tree[,1]))
test.pred <- predict(model.svm, newdata = test.2)
table(pred = test.pred, true = cp.test$cond)
test.2 <- test[,c(2:4,6,8,12,17:19,21,23,27,32:34,36,38,42,47:49,51,53,57)]

svm_tune <- tune(svm, cp.cond ~ ., data = data.tree, kernel = "linear",
                  ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
mod.svm.cv <- svm(cp.cond ~ ., data = data.tree, cost = .1, gamma = .5)
pred <- predict(mod.svm.cv, data = data.tree)
table(pred = pred, true = data.tree[,1])
classAgreement(table(pred = pred, true = data.tree[,1]))
test.pred <- predict(mod.svm.cv, newdata = test.2)
table(pred = test.pred, true = cp.test$cond)
classAgreement(table(pred = test.pred, true = cp.test$cond))

str(test.2$c1.cond)
################################
library(nnet)
mmod <- multinom(cp.cond ~ ., data = data.tree, family = "multinomial")
summary(mmod)
p <- predict(mmod, data = data.tree)
table(pred = p, true = data.tree[,1])
pr <- predict(mmod, newdata = test.2)
table(pred = pr, true = cp.test$cond)

mmodi <- step(mmod)
pr <- predict(mmodi, newdata = test.2)
table(pred = pr, true = cp.test$cond)
###################################
install.packages("gbm")
library(gbm)
mod.gbm <- gbm(cp.cond ~., data = data.tree, distribution = "multinomial",n.trees = 10000)
summary(mod.gbm)
p <- predict(mod.gbm, data = data.tree, n.trees = 10000, type = "response")
apply(p,1,colnames(which.max))
col <- colnames(p)[apply(p,1,which.max)]
table(pred = col, true = data.tree[,1])
p.test <- predict(mod.gbm, newdata = test.2, n.trees = 10000, type = "response")
col.2 <- colnames(p.test)[apply(p.test,1,which.max)]
table(pred = col.2, true = cp.test$cond)

mod.gbm.cv <- gbm(cp.cond ~., data = data.tree, distribution = "multinomial",n.trees = 1000, cv = 3)
summary(mod.gbm.cv)
ntree_opt_cv <- gbm.perf(object = mod.gbm.cv, method = "cv")
mod.gbm.cv.2 <- gbm(cp.cond ~., data = data.tree, distribution = "multinomial",n.trees = 100, cv = 5)
p.2 <- predict(mod.gbm.cv.2, data = data.tree, n.trees = 100, type = "response")
col.2 <- colnames(p.2)[apply(p.2,1,which.max)]
table(pred = col.2, true = data.tree[,1])
p.3 <- predict(mod.gbm.cv.2, newdata = test.2, n.trees = 100, type = "response")
col.3 <- colnames(p.3)[apply(p.3,1,which.max)]
table(pred = col.3, true = cp.test$cond)

################################3
library(class)
data.std <- scale(data.tree[,c(2:4,6,8:10,12,14:16,18,20:22,24)])
test.std <- scale(test.2[,c(1,2,5:8,11:14,17:20,23,24)])
knn.train <- knn(train = data.std,test = test.std, k = 100, cl = data.tree[,1])
knn.train
table(cp.test$cond, knn.train, dnn = c("True", "Predicted"))




##########################3
data.nocond <- data.tree[,c(2:4,6,8:10,12,14:16,18,20:22,24)]
data.nocond <- cbind(data.nocond,data.tree$cp.cond)
colnames(data.nocond)[17] <- "cp.cond"
test.nocond <- test.2[,c(1,2,5:8,11:14,17:20,23,24)]
mod.gbm.cv5 <- gbm(cp.cond ~., data = data.nocond, distribution = "multinomial",n.trees = 8768, cv = 3)
ntree_opt_cv <- gbm.perf(object = mod.gbm.cv5, method = "cv")
p.5 <- predict(mod.gbm.cv5, data = data.nocond, n.trees = 8768, type = "response")
col.5 <- colnames(p.5)[apply(p.5,1,which.max)]
table(pred = col.5, true = data.tree[,1])
p.6 <- predict(mod.gbm.cv5, newdata = test.nocond, n.trees = 8768, type = "response")
col.6 <- colnames(p.6)[apply(p.6,1,which.max)]
table(pred = col.6, true = cp.test$cond)
##############################


######################33
data.nocond <- data.tree[,c(2:6,8:12,14:18,20:24)]
data.nocond <- cbind(data.nocond,data.tree$cp.cond)
colnames(data.nocond)[21] <- "cp.cond"
test.nocond <- test.2[,c(1,2,4:8,10:14,16:20,22:24)]
svm_tune <- tune(svm, cp.cond ~ ., data = data.nocond,
                 ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
model.svm <- svm(cp.cond ~ ., data = data.nocond, cost = 10, gamma = .5)
summary(model.svm)
pred <- predict(model.svm, data = data.nocond)
table(pred = pred, true = data.tree[,1])
classAgreement(table(pred = pred, true = data.tree[,1]))
test.pred <- predict(model.svm, newdata = test.nocond)
classAgreement(table(pred = test.pred, true = cp.test$cond))


###########################333
noprecip.train <- data.tree[,c(1:5,7:11,13:17,19:23,25)]
noprecip.test <- test.2[,c(1:5,7:11,13:17,19:23)]
mod.gbm.cv <- gbm(cp.cond ~., data = noprecip.train, distribution = "multinomial",n.trees = 10000, cv = 3)
summary(mod.gbm.cv)
ntree_opt_cv <- gbm.perf(object = mod.gbm.cv, method = "cv")
mod.gbm.cv.2 <- gbm(cp.cond ~., data = noprecip.train, distribution = "multinomial",n.trees = 6244, cv = 3)
p.2 <- predict(mod.gbm.cv.2, data = noprecip.train, n.trees = 6244, type = "response")
col.2 <- colnames(p.2)[apply(p.2,1,which.max)]
table(pred = col.2, true = noprecip.train[,1])
p.3 <- predict(mod.gbm.cv.2, newdata = noprecip.test, n.trees = 6244, type = "response")
col.3 <- colnames(p.3)[apply(p.3,1,which.max)]
table(pred = col.3, true = cp.test$cond)

svm_tune <- tune(svm, cp.cond ~ ., data = noprecip.train,
                 ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
model.svm <- svm(cp.cond ~ ., data = noprecip.train, cost = 10, gamma = .5)
summary(model.svm)
pred <- predict(model.svm, data = noprecip.train)
table(pred = pred, true = data.tree[,1])
classAgreement(table(pred = pred, true = data.tree[,1]))
test.pred <- predict(model.svm, newdata = noprecip.test)
classAgreement(table(pred = test.pred, true = cp.test$cond))

###############################
nodir.train <- data.tree[,c(1:4,6:10,12:16,18:22,24,25)]
nodir.test <- test.2[,c(1:3,5:9,11:15,17:21,23,24)]
mod.gbm.cv <- gbm(cp.cond ~., data = nodir.train, distribution = "multinomial",n.trees = 10000, cv = 3)
summary(mod.gbm.cv)
ntree_opt_cv <- gbm.perf(object = mod.gbm.cv, method = "cv")
mod.gbm.cv.2 <- gbm(cp.cond ~., data = nodir.train, distribution = "multinomial",n.trees = 9995, cv = 3)
p.2 <- predict(mod.gbm.cv.2, data = nodir.train, n.trees = 9995, type = "response")
col.2 <- colnames(p.2)[apply(p.2,1,which.max)]
table(pred = col.2, true = nodir.train[,1])
p.3 <- predict(mod.gbm.cv.2, newdata = nodir.test, n.trees = 9995, type = "response")
col.3 <- colnames(p.3)[apply(p.3,1,which.max)]
table(pred = col.3, true = cp.test$cond)

svm_tune <- tune(svm, cp.cond ~ ., data = nodir.train,
                 ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
model.svm <- svm(cp.cond ~ ., data = nodir.train, cost = 10, gamma = .5)
summary(model.svm)
pred <- predict(model.svm, data = nodir.train)
table(pred = pred, true = data.tree[,1])
classAgreement(table(pred = pred, true = data.tree[,1]))
test.pred <- predict(model.svm, newdata = nodir.test)
classAgreement(table(pred = test.pred, true = cp.test$cond))
