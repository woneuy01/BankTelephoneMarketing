
rm(list=ls()); gc()
setwd('C:\\Users\\woneu\\OneDrive\\Desktop\\ISDS574_project\\bank-additional')
#setwd("~/Downloads")
#install.packages("VIM")
#install.packages("corrplot")
#install.packages("ggplot2")
#install.packages('fastDummies')
#install.packages("dmm")
#install.packages("dplyr")
#install.packages("DMwR")
#install.packages("scales")
#install.packages("randomForest")
#install.packages("useful")
##=========================Look into the data=======================================================##

dat = read.csv('bank-additional-full.csv', head=T, stringsAsFactors=F, na.strings='',sep=';')

dim(dat) # original data 41188 X 21
summary(dat)
summary.default(dat)

##==========================Check missing values=====================================================##
matrix.na = is.na(dat)
pmiss = colMeans(matrix.na) # proportion of missing for each column
nmiss = rowMeans(matrix.na) # proportion of missing for each row
matrix.na
pmiss
nmiss
plot(pmiss)

##=========================Check distribution of numeric (continuous) and categorical variables==========================##

require("ggplot2")

#Age graph
hist(dat$age, col = "purple", main = "Age", xlab="Age", ylab="Count")
theme_update(plot.title = element_text(hjust = 0.5)) # centering the title position as a default
ggplot(dat) + geom_bar(mapping= aes(x=age, fill =y)) + ggtitle("Age")
boxplot(dat$age,data=dat, main="Age", ylab="Age") 

#Job graph
ggplot(dat) + geom_bar(mapping= aes(x=job, fill =y))+ggtitle("Job")

#Marital graph
ggplot(dat) + geom_bar(mapping= aes(x=marital, fill =y)) + ggtitle("Marital")

#Education graph
ggplot(dat) + geom_bar(mapping= aes(x=education, fill =y)) + ggtitle("Education")

#Default graph
ggplot(dat) + geom_bar(mapping= aes(x=default, fill =y)) + ggtitle("Default")

#Housing graph
ggplot(dat) + geom_bar(mapping= aes(x=housing, fill =y)) + ggtitle("Housing")

#Loan graph
ggplot(dat) + geom_bar(mapping= aes(x=loan, fill =y)) + ggtitle("Loan")

theme_update(plot.title = element_text(hjust = 0.5))
# Contact graph
var_name_description <- "Contact"
ggplot(dat) + geom_bar(mapping= aes(x=contact, fill =y)) + ggtitle(var_name_description)

# Month graph
var_name_description <- "Month"
ggplot(dat) + geom_bar(mapping= aes(x=month, fill =y)) + ggtitle(var_name_description)

# Day_of_Week graph
var_name_description <- "Day_of_Week"
ggplot(dat) + geom_bar(mapping= aes(x=day_of_week, fill =y)) + ggtitle(var_name_description)

#Duration graph
var_name_description <- "Duration"
ggplot(dat) + geom_bar(mapping= aes(x=duration, fill =y)) + ggtitle(var_name_description)
#hist(dat$duration, col = "purple", main = var_name_description, xlab=var_name_description, ylab="Count")

#Campaign graph
var_name_description <- "Campaign"
ggplot(dat) + geom_bar(mapping= aes(x=campaign, fill =y)) + ggtitle(var_name_description)
#hist(dat$campaign, col = "purple", main = var_name_description, xlab=var_name_description, ylab="Count")

#pDays graph
var_name_description <- "# of pDays"
ggplot(dat) + geom_bar(mapping= aes(x=pdays, fill =y)) + ggtitle(var_name_description)+ scale_x_continuous(limits = c(0,30)) + scale_y_continuous(limits = c(0,70))
ggplot(dat) + geom_bar(mapping= aes(x=pdays, fill =y)) + ggtitle(var_name_description) + scale_x_continuous(limits = c(970,1000))

#previous graph
var_name_description <- "previous"
ggplot(dat) + geom_bar(mapping= aes(x=previous, fill =y)) + ggtitle(var_name_description)
#hist(dat$previous, col = "purple", main = var_name_description, xlab=var_name_description, ylab="Count")

#poutcome graph
ggplot(dat) + geom_bar(mapping= aes(x=poutcome, fill =y)) + ggtitle("poutcome")

#emp.var.rate graph
ggplot(dat) + geom_bar(mapping= aes(x=emp.var.rate, fill =y)) + ggtitle("emp.var.rate")
boxplot(dat$emp.var.rate,data=dat, main="emp.var.rate", ylab="emp.var.rate") 

#cons.price.idx graph
ggplot(dat) + geom_bar(mapping= aes(x=cons.price.idx, fill =y))+ggtitle("cons.price.idx")

#cons.conf.idx graph
ggplot(dat) + geom_bar(mapping= aes(x=cons.conf.idx , fill =y)) + ggtitle("cons.conf.idx ")

#euribor3m.idx graph
ggplot(dat) + geom_bar(mapping= aes(x=euribor3m, fill =y)) + ggtitle("euribor3m")

#nr.employed graph
ggplot(dat) + geom_bar(mapping= aes(x=nr.employed, fill =y)) + ggtitle("nr.employed")


##=========================Replace Unknown and Collinearity check===============================##

#replace unknown --> NA
#dat[dat=="unknown"] <- NA
require("VIM")
dim(dat)
dat$y<-ifelse(dat$y =='yes', 1,0) #yes=4640, no=36548 yes is only 11.26% of whole data
table(dat$y)

#select numeric data for correlation calculation
#set y as numeric just for correlation purpose
dat$y<-as.numeric(dat$y) # for correlation graph
names(dat)
numeric_dat <- dat[, c("age","duration", "campaign", "pdays","previous","emp.var.rate","cons.price.idx",
                       "cons.conf.idx","euribor3m","nr.employed","y")]

require(corrplot)
numeric_dat_cor= cor(numeric_dat, method = c("spearman"))
par(mfrow=c(1,1))
corrplot(numeric_dat_cor,method = "number")
dat$y<-as.factor(dat$y) 

##============Combine categories and create dummy variables for categorical variables================##

#Convert Continuous to Categorical variables
dat$age<-cut(dat$age, breaks = c(0,20,30,45,60,100),labels = c("A","B","C","D","E"))
table(dat$age)
#Make dummy variable for age variable
require(fastDummies)
dat= dummy_cols(dat, c("age"))
dat= dat[,-which(colnames(dat) == "age")]

#Make dummy variables for job variable
gg<-sort(table(dat$job))
require(dplyr)
dat$job[which(dat$job %in% c("unemployed","retired","student","self-employed","entrepreneur","housemaid", "unknown"))] = "Other_Jobs"
table(dat$job)
dat= dummy_cols(dat, c("job"))
dat= dat[,-which(colnames(dat) == "job")]
#names(my_data)[names(my_data) == "Sepal.Length"] <- "sepal_length"
names(dat)[names(dat)=="job_blue-collar"] <-"job_blue_collar"
#colnames(train$"job_blue-collar")<- colnames(train$"job_blue_collar")
names(dat)
job_admin
#Make dummy variables for marital variable
dat= dummy_cols(dat, c("marital"))
dat= dat[,-which(colnames(dat) == "marital")]
head(dat)

#Make dummy variables for Eductaion variable
table(dat$education)
dat$education[which(dat$education %in% c("illiterate","unknown"))] = "Other_education"
dat$education[which(dat$education %in% c("basic.4y","basic.6y","basic.9y"))] = "Basic_education"
table(dat$education)
dat= dummy_cols(dat, c("education"))
dat= dat[,-which(colnames(dat) == "education")]

#Make dummy variables for Default variable
table(dat$default)
dat= dummy_cols(dat, c("default"))
dat= dat[,-which(colnames(dat) == "default")]

#Make dummy variables for Housing
table(dat$housing)
dat= dummy_cols(dat, c("housing"))
dat= dat[,-which(colnames(dat) == "housing")]

#Make dummy variables for Loan
table(dat$loan)
dat= dummy_cols(dat, c("loan"))
dat= dat[,-which(colnames(dat) == "loan")]

#Make dummy variable for contact variable
dat= dummy_cols(dat, c("contact"))
dat= dat[,-which(colnames(dat) == "contact")]

#Make dummy variable for month variable
gg<-sort(table(dat$month))
require(dplyr)
dat$month[which(dat$month %in% c("jan","feb", "mar", "apr", "sep", "oct", "dec"))] = "Other_Month"
table(dat$month)
dat= dummy_cols(dat, c("month"))
dat= dat[,-which(colnames(dat) == "month")]


#Make dummy variable for day_of_week variable
table(dat$day_of_week)
dat= dummy_cols(dat, c("day_of_week"))
dat= dat[,-which(colnames(dat) == "day_of_week")]

#Make dummy variable for poutcome
table(dat$poutcome)
dat= dummy_cols(dat, c("poutcome"))
dat= dat[,-which(colnames(dat) == "poutcome")]


# delete high correlated variables ("nr.employed" & "emp.var.rate") use eruibor3m instead.
dat = dat[,-which(colnames(dat) == "nr.employed")]
dat = dat[,-which(colnames(dat) == "emp.var.rate")]

# rescale range normalize data in range [0,1] for numerical variables
#"duration","campaign", "pdays", "previous","cons.price.idx", "cons.conf.idx", "euribor3m"
require("scales")
scale_col <- c("duration","campaign", "pdays", "previous",
                "cons.price.idx", "cons.conf.idx", "euribor3m")

for (col in scale_col) {
  dat[col] <-rescale(dat[,col])
}

head(dat)
names(dat)

##===============================Creating Data Partitions=================================##

#data partition
set.seed(1) # set a seed so that people get the same 60% next time they run the same code
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
dim(train)
dim(test) 
head(train)
class(train)
names(train)
colnames(train)


##======================Please use train data set for model building================================##
##======================Random Forest===============================================================##
require("randomForest")
class(train)
trainForest <- randomForest(y ~.,data=train, ntree = 500, mtry = 4, nodesize = 5, importaince = TRUE)

# variable importance plot
varImpPlot(trainForest, type =NULL)
# confusion matrix
train_pred <- predict(trainForest,test,type="prob")[,2]
head(train_pred)
require(caret)
confusionMatrix(train_pred, test$y)

#cutoff set as 0.5 as default do not need to change
dichotomize = function(train_pred, cutoff=0.5) {
  out = rep(0, length(train_pred))
  out[train_pred > cutoff] = 1
  out
}
# change for default change
yhat = dichotomize(train_pred, 0.5)
errfwd = mean(yhat != test$y)
# misclassification error rate
errfwd


table(yhat, test$y)

senfwd = function(y, train_pred) {
  ind.true1 = which(y == 1)
  mean( y[ind.true1] == train_pred[ind.true1] )
}

spefwd = function(y, train_pred) {
  ind.true0 = which(y == 0)
  mean( y[ind.true0] == train_pred[ind.true0] )
}

senfwd(test$y, yhat)
spefwd(test$y, yhat)








