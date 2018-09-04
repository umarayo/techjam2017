########## Version 1.3 - Score  57.204/100 #######

## Install packages - First time only
# install.packages("dplyr")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
# install.packages("data.table")

## Load packages  - First time only
# library('dplyr') # data manipulation
# library('xgboost') # classification algorithm
# library('data.table')  #provides an in-memory columnar structure

## Setup working directory
setwd("C:/uma/0_TechJam/Data/2")  #Home 
getwd()

## Remove scientific notation
options(scipen = 999) 

## Load Data
acc=read.csv('./input/tj_02_account_transaction.csv',header=TRUE)
cc=read.csv('./input/tj_02_creditcard_transaction.csv',header=TRUE)
acc_x_cc = read.csv('./input/tj_02_acc_x_card.csv',header=TRUE)
train.org = read.csv('./input/tj_02_training.csv',header=FALSE)
test.org = read.csv('./input/tj_02_test.csv',header=FALSE)

##################################################################
#-----------------------------------------------------------------
# Data Preparation
#-----------------------------------------------------------------
##################################################################

# Change column name
names(acc)[names(acc)=='txn_dt'] <- 'txn_date'
names(acc)[names(acc)=='from_to_account_no'] <- 'from_to'

# Rearrange cc data
cc$txn_type <- "CC"  %>% as.factor()
cc2 <- cc[,c("card_no","txn_date","txn_hour","txn_amount","mer_cat_code","mer_id","txn_type")]


# Add column name for train and test
colnames(train.org) = c("account_no","is_merchant")
colnames(test.org) = c("account_no")

# Check duplicated data in cc table
# cc.u <-  unique( cc[ , 1:ncol(cc) ] )
# dup_flag <- duplicated(cc[ ,1:ncol(cc)])
# cc.dup <- cc[dup_flag,]
# cc.dup <- setorderv(cc.dup,c("card_no","txn_date","txn_hour","txn_amount","mer_cat_code"), c(1,1,1,1,1))

# Display all rows, which are not NA or equal to "".
acc_x_cc.m <- acc_x_cc[!(is.na(acc_x_cc$card_no) | acc_x_cc$card_no==""), ] 

# Merge table cc with acc_x_cc
cc.m <- merge(x=cc2, y=acc_x_cc.m, by = "card_no", all.x=TRUE)
#cc.u <-  unique( cc.m[ , 1:ncol(cc.m) ] )


# Combine acc and cc data and add month and day field
data <- bind_rows(acc,cc.m) #dplyr library
data$txn_date <- as.Date(data$txn_date, format='%Y-%m-%d')
#data$txn_year = as.numeric(format(data$txn_date, format = "%Y"))
data$txn_month = as.numeric(format(data$txn_date, format = "%m"))
data$txn_day = as.numeric(format(data$txn_date, format = "%d"))

# Extract more 3 Columns from field txn_type
data$is_DR <- data$txn_type=="DR"
data$is_CR <- data$txn_type=="CR"
data$is_CC <- data$txn_type=="CC"


# Selected column for analysis
##### Column Name List
#####'is_merchant'
#####'account_no','from_to','txn_date','txn_year','txn_month','txn_day','txn_hour','txn_type','txn_amount'
#####'card_no','mer_cat_code','mer_id'
#####'is_DR','is_CR','is_CC'
cols_D=c('account_no','from_to','txn_month','txn_day','txn_hour','txn_amount','txn_type','is_DR','is_CR','is_CC','card_no','mer_cat_code','mer_id')
#cols_D=c('account_no','from_to','txn_month','txn_day','txn_hour','txn_amount','txn_type','card_no','mer_cat_code','mer_id')
D <- data[,cols_D]

# Replace level by number
for (f in cols_D) {
  if (class(D[[f]])=="character") {
    levels <- unique(c(D[[f]]))
    D[[f]] <- as.numeric(factor(D[[f]], levels=levels))
  }
}

# Update Logical value True>>1 False>>0
p_logi <- names(D)[which(sapply(D, is.logical))]
for (col in p_logi) set(D, j = col, value = as.numeric(D[[col]]))

# Convert NA to 0
D[is.na(D)] <- 0

# Data Checking
str(D)
head(D)
tail(D)

# Merge train and test with data table
train <- merge(x=train.org, y=D, by = "account_no", all.x=TRUE)
test <- merge(x=test.org, y=D, by = "account_no", all.x=TRUE)

# Move column 'is_merchant' to be the first column
cols_T =c('is_merchant',cols_D)
train <- train[,cols_T]


# Convert data to xgboost format
dtrain <- xgb.DMatrix(data = data.matrix(train[, 2:ncol(train)]), label = train$is_merchant)
dtest  <- xgb.DMatrix(data.matrix(test))


##################################################################
#-----------------------------------------------------------------
# Tune and Run the model
#-----------------------------------------------------------------
##################################################################

param <- list(
  # General Parameters
  booster            = "gblinear",          # default = "gbtree"
  silent             = 0,                 	# default = 0
  # Booster Parameters
  eta                = 0.3,               	# default = 0.3, range: [0,1]
  gamma              = 0.3,                 # default = 0,   range: [0,???]
  max_depth          = 11,                 	# default = 6,   range: [1,???]
  min_child_weight   = 1,                 	# default = 1,   range: [0,???]
  subsample          = 0.5,                 # default = 1,   range: (0,1]
  colsample_bytree   = 0.9,                 # default = 1,   range: (0,1]
  colsample_bylevel  = 0.9,                 # default = 1,   range: (0,1]
  lambda             = 0.95,              	# default = 1
  alpha              = 0,                 	# default = 0
  # Task Parameters
  objective          = "binary:logistic",   # default = "reg:linear"
  eval_metric        = "error"
)

set.seed(1712)
xgm <- xgb.train(data = dtrain,
                 param, nrounds = 20,
                 watchlist = list(train = dtrain),
                 print_every_n = 10,
                 verbose = 1)

##################################################################
#-----------------------------------------------------------------
# Prediction and Output transformation
#-----------------------------------------------------------------
##################################################################

# Predict
out <- predict(xgm, dtest)
sub <- data.frame(account_no= test$account_no, is_merchant = out)

# Observe distribution of predicted data
hist(sub$is_merchant)

# Convert probability to flag 1 or 0
CutOff = median(sub$is_merchant)

p_h <- rownames(sub)[which(sub$is_merchant>=CutOff)]
for (row in p_h) set(sub, j = as.integer(2), i=as.integer(row), value = 1)

p_l <- rownames(sub)[which(sub$is_merchant<CutOff)]
for (row in p_l) set(sub, j = as.integer(2), i=as.integer(row), value = 0)

# Assign "is_merchant" flag using mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

SB <- aggregate(is_merchant ~ account_no, sub, Mode)

# Reorder outcome data to be matched with the test data
TS <-test.org
TS$is_merchant <- with(SB, is_merchant[match(TS$account_no,SB$account_no)])
OC <- data.frame(is_merchant=TS$is_merchant)

# Export to file
write.table(OC, file = "./output/2.txt", col.names=FALSE ,row.names = FALSE, quote=FALSE)
