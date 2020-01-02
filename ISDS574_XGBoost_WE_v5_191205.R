
rm(list=ls()); gc()
setwd('C:\\Users\\woneu\\OneDrive\\Desktop\\ISDS574_project\\bank-additional')
#setwd("~/Downloads")
#install.packages("VIM")
#install.packages("corrplot",dependencies = TRUE)
#install.packages("ggplot2")
#install.packages('fastDummies')
#install.packages("dmm")
#install.packages("dplyr")
#install.packages("DMwR")
#install.packages("scales")
#install.packages("data.table")
#install.packages("xgboost")
#install.packages("magrittr")
#install.packages("Matrix")
#install.packages("mlr")
#install.packages("parallelMap")
##=========================Look into the data=======================================================##

dat = read.csv('bank-additional-full.csv', head=T, stringsAsFactors=F, na.strings='',sep=';')

dim(dat) # original data 41188 X 21
summary(dat)
summary.default(dat)

require("VIM")
dim(dat)
dat$y<-ifelse(dat$y =='yes', 1,0) #yes=4640, no=36548 yes is only 11.26% of whole data
dat$y <-as.factor(dat$y)
table(dat$y)

##============Combine categories and create dummy variables for categorical variables================##

#Convert Continuous to Categorical variables
dat$age<-cut(dat$age, breaks = c(0,20,30,45,60,100))
table(dat$age)

gg<-sort(table(dat$job))
require(dplyr)
dat$job[which(dat$job %in% c("unemployed","retired","student","self-employed","entrepreneur","housemaid", "unknown"))] = "Other_Jobs"
dat$job[which(dat$job %in% c("admin."))] = "admin"
dat$education[which(dat$education %in% c("illiterate","unknown"))] = "Other_education"
dat$education[which(dat$education %in% c("basic.4y","basic.6y","basic.9y"))] = "Basic_education"

require(dplyr)
dat$month[which(dat$month %in% c("jan","feb", "mar", "apr", "sep", "oct", "dec"))] = "Other_Month"
dat = dat[,-which(colnames(dat) == "nr.employed")]
dat = dat[,-which(colnames(dat) == "emp.var.rate")]
require("scales")
scale_col <- c("duration","campaign", "pdays", "previous",
                "cons.price.idx", "cons.conf.idx", "euribor3m")

for (col in scale_col) {
  dat[col] <-rescale(dat[,col])
}

# data partition no oversampling
# set.seed(1)
# n = nrow(dat)
# ind.train = sample(n, floor(n*.7))
# ind.val = setdiff(1:n, ind.train)
# train <- dat[ind.train,]
# test <- dat[ind.val,]

#data partition
set.seed(2) # set a seed so that people get the same 60% next time they run the same code
#seperate data from y=1 and y=0 yes=4640, no=36548
dat_y1 <- dat[dat$y==1,]
dat_y0 <- dat[!dat$y==1,]
# select 3000 from dat_y1(y=1) data for training data
id_train_y1 = sample(nrow(dat_y1),3000)
# rest of dat_y1 1640 becomes test data 
id_test_y1 =setdiff(1:nrow(dat_y1),id_train_y1)
dat_train_y1 <- dat_y1[id_train_y1,]
dat_test_y1 <- dat_y1[id_test_y1,]
# select 3000 from dat_y0(y=0) for training data
id_train_y0 = sample(nrow(dat_y0),3000)
id_test_y0 <- setdiff(1:nrow(dat_y0),id_train_y0)
dat_train_y0 <- dat_y0[id_train_y0,]
dat_test_y0 <- dat_y0[id_test_y0,]

train<- rbind(dat_train_y0, dat_train_y1) 
test <- rbind(dat_test_y0, dat_test_y1)
head(train,1)

dim(train)
dim(test)
#======================XGBoost============================================#

labels <- train$y 
ts_label <- test$y
require(Matrix)
dim(train)
dim(test)
str(train)
new_tr <- sparse.model.matrix(y~.-y, data = train)
new_ts <- sparse.model.matrix(y~.-y, data = test)
View(head(new_tr,1))
View(head(new_ts,1))
dim(new_tr)
dim(new_ts)
#convert factor to numeric 
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1
table(labels)
#For xgboost, use xgb.DMatrix to convert data table into a matrix (most recommended):
#preparing matrix 
require(xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(new_tr),label = labels) 
dtest <- xgb.DMatrix(data = as.matrix(new_ts),label=ts_label)
colnames(dtrain)
colnames(dtest)
#default parameters
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, 
                        subsample=1, colsample_bytree=1)

#Using the inbuilt xgb.cv function, let's calculate the best nround for this model. 
#In addition,#this function also returns CV error, which is an estimate of test error. 
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T,
                 stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)
xgbcv
names(xgbcv)
# find min test_error_mean iteration number
min(xgbcv$evaluation_log[,4])
which(xgbcv$evaluation_log[,4]==0.1158332)

#model training
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 13, watchlist = list(val=dtest,train=dtrain),
                   early_stop_round = 10, maximize = F , eval_metric = "error")
#model prediction
head(xgb1)
typeof(xgb1)
head(dtest)
typeof(dtest)
xgbpred <- predict (xgb1,dtest)
#manually use a cutoff value 0.5 since it return probability
xgbpred <- ifelse (xgbpred > 0.5,1,0)
require(caret)
confusionMatrix(table(xgbpred, ts_label))
#view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])

#convert characters to factors
fact_col <- colnames(train)[sapply(train,is.character)]

for(i in fact_col) set(train,j=i,value = factor(train[[i]]))
for (i in fact_col) set(test,j=i,value = factor(test[[i]]))

#create tasks
require(mlr)
traintask <- makeClassifTask(data = train,target = "y")
testtask <- makeClassifTask (data = test,target = "y")

#do one hot encoding`<br/> 
traintask <- createDummyFeatures (obj = traintask) 
testtask <- createDummyFeatures (obj = testtask)

#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list(
  objective="binary:logistic",
  eval_metric="error",
  nrounds=100L,
  eta=0.1
)

#set parameter space
params <- makeParamSet(
  makeDiscreteParam("booster",values = c("gbtree","gblinear")),
  makeIntegerParam("max_depth",lower = 3L,upper = 10L),
  makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
  makeNumericParam("subsample",lower = 0.5,upper = 1),
  makeNumericParam("colsample_bytree",lower = 0.5,upper = 1)
)

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)

#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

#set parallel backend
require(parallel)
require(parallelMap)
parallelStartSocket(cpus = 2)

#parameter tuning
mytune <- tuneParams(learner = lrn
                     ,task = traintask
                     ,resampling = rdesc
                     ,measures = acc
                     ,par.set = params
                     ,control = ctrl
                     ,show.info = T)


mytune$y

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- mlr::train(learner = lrn_tune,task = traintask)

#predict model
xgpred <- predict(xgmodel,testtask)

confusionMatrix(xgpred$data$response,xgpred$data$truth)
