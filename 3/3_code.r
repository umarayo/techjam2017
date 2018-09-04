########## Version 1.2 - Score  88.525/100 #######

## Install packages - First time only
# install.packages("dplyr")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
# install.packages("data.table")

## Load packages  - First time only
# library('dplyr') # data manipulation
# library('xgboost') # classification algorithm
# library('data.table')  #provides an in-memory columnar structure

## Setup working directory
setwd("C:/uma/0_TechJam/Data/3")  #Home
getwd()

## Remove scientific notation
options(scipen = 999) 

## Load Data
acc=read.csv('./input/tj_03_account_info.csv',header=TRUE)
txn=read.csv('./input/tj_03_deposit_txn.csv',header=TRUE)
train.org = read.csv('./input/tj_03_training.csv',header=FALSE)
test.org = read.csv('./input/tj_03_test.csv',header=FALSE)

##################################################################
#-----------------------------------------------------------------
# Data Preparation
#-----------------------------------------------------------------
##################################################################

# Add prefix to column name 'info_'
names(acc)[2:length(names(acc))]=paste0('info_',names(acc)[2:length(names(acc))])

# Add column name for train and test
colnames(train.org) = c("account_no","label")
colnames(test.org) = c("account_no")

# Check duplicated data -- NO DUPLICATED DATA
# acc.u <-  unique( acc[ , 1:ncol(acc) ] )
# txn.u <-  unique( txn[ , 1:ncol(txn) ] )
# dup_flag <- duplicated(txn[ ,1:ncol(txn)])
# dup_f <- data.frame(dup_flag)
# total_dup <- (dup_f[which(dup_f==TRUE),1])
# txn.dup <- txn[dup_flag,]
# txn.dup <- txn.dup[,c("account_no","txn_type","txn_amount","txn_hour","from_to_account_no","txn_dt")]
# txn.dup <- setorderv(txn.dup,c("account_no","txn_type","txn_amount","txn_hour"), c(1,1,1,1))

# Convert format to date
acc$info_txn_dt <- as.Date(acc$info_txn_dt, format='%Y-%m-%d')
acc$info_open_date <- as.Date(acc$info_open_date, format='%Y-%m-%d')
acc$info_last_active_date <- as.Date(acc$info_last_active_date, format='%Y-%m-%d')
txn$txn_dt <- as.Date(txn$txn_dt, format='%Y-%m-%d')


# Extract only max(txn_date) record per account
acc.m <- setDT(acc)[,.SD[which.max(info_txn_dt)],keyby=account_no]

# Introduce new column for open and last_active days
acc.m$info_txn_days <-  difftime(Sys.Date(),acc.m$info_txn_dt,units='days')%>% as.numeric()
acc.m$info_open_days <-  difftime(Sys.Date(),acc.m$info_open_date,units='days')%>% as.numeric()
acc.m$info_last_active_days <-  difftime(Sys.Date(),acc.m$info_last_active_date,units='days')%>% as.numeric()
txn$txn_days <- difftime(Sys.Date(),txn$txn_dt,units='days')%>% as.numeric()

# Combine acc and txn data and add month and day field
data <- merge(x=txn, y=acc.m, by = "account_no", all.x=TRUE)

# Extract more 2 Columns from field txn_type
data$is_DR <- data$txn_type=="DR"
data$is_CR <- data$txn_type=="CR"

# Convert column type
data$txn_type <- as.character(data$txn_type)
data$info_compound_frq_unit <- as.character(data$info_compound_frq_unit)

# Selected column for analysis
##### Column Name List
##### Main 	- 'account_no'
##### Date 	- 'txn_dt','info_txn_dt','info_open_date','info_last_active_date'
##### Txn 	- 'from_to_account_no','txn_amount',,'txn_hour','txn_type'
##### Info	- 'info_customer_type','info_dormant_days','info_compound_frq','info_compound_frq_unit','info_eff_interest_rate'
##### New 	- 'txn_days','is_DR','is_CR','info_txn_days','info_open_days','info_last_active_days'
cols_D=c('account_no','from_to_account_no','txn_amount','txn_hour','txn_type','info_customer_type','info_dormant_days','info_compound_frq','info_compound_frq_unit','info_eff_interest_rate','txn_days','is_DR','is_CR','info_txn_days','info_open_days','info_last_active_days')
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
cols_T =c('label',cols_D)
train <- train[,cols_T]


# Convert data to xgboost format
dtrain <- xgb.DMatrix(data = data.matrix(train[, 2:ncol(train)]), label = train$label)
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
                 param, nrounds = 30,
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
sub <- data.frame(account_no= test$account_no, label = out)

# Observe distribution of predicted data
hist(sub$label,xlim = range(0:max(sub$label)))

# Convert pop to flag 1 or 0
label<-sub$label
CutOff = mean(label)+5*(sqrt(mean(label^2)-mean(label)^2))

p_h <- rownames(sub)[which(sub$label>CutOff)]
for (row in p_h) set(sub, j = as.integer(2), i=as.integer(row), value = 1)

p_l <- rownames(sub)[which(sub$label<=CutOff)]
for (row in p_l) set(sub, j = as.integer(2), i=as.integer(row), value = 0)

# Assign "is_merchant" flag using mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

SB <- aggregate(label ~ account_no, sub, Mode)

# Reorder outcome data to be matched with the test data
TS <-test.org
TS$label <- with(SB, label[match(TS$account_no,SB$account_no)])
OC <- data.frame(label=TS$label)

# Export to file
write.table(OC, file = "./output/3.txt", col.names=FALSE ,row.names = FALSE, quote=FALSE)
