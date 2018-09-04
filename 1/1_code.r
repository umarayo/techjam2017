########## Version 1.4 - Score  75.966/100 #######

## Install packages - First time only
# install.packages("dplyr")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
# install.packages("data.table")

## Load packages  - First time only
# library('dplyr') # data manipulation
# library('xgboost') # classification algorithm
# library('data.table')  #provides an in-memory columnar structure

## Setup working directory
setwd("C:/uma/0_TechJam/Data/1")  #Home
getwd()

## Remove scientific notation
options(scipen = 999) 

## Load Data
cc=read.csv('./input/tj_01_creditcard_card.csv',header=TRUE)
cus=read.csv('./input/tj_01_creditcard_customer.csv',header=TRUE)
txn=read.csv('./input/tj_01_creditcard_transaction.csv',header=TRUE)
train.org = read.csv('./input/tj_01_training.csv',header=FALSE)
test.org = read.csv('./input/tj_01_test.csv',header=FALSE)

##################################################################
#-----------------------------------------------------------------
# Data Preparation
#-----------------------------------------------------------------
##################################################################

# Add prefix to column name 
names(cc)[2:length(names(cc))]=paste0('cc_',names(cc)[2:length(names(cc))])
names(cus)[2:length(names(cus))]=paste0('cus_',names(cus)[2:length(names(cus))])

# Change column name
names(cc)[names(cc)=='cc_cst_id'] <- 'cst_id'


# Add column name for train and test
colnames(train.org) = c("card_no","npl_flag")
colnames(test.org) = c("card_no")

# Check duplicated data -- NO DUPLICATED DATA EXCEPT txn table
# cc.u <-  unique( cc[ , 1:ncol(cc) ] )
# cus.u <-  unique( cus[ , 1:ncol(cus) ] )
# train.org.u <-  unique( train.org[ , 1:ncol(train.org) ] )
# test.org.u <-  unique( test.org[] )
# txn.u <-  unique( txn[ , 1:ncol(txn) ] )
# dup_flag <- duplicated(txn[ ,1:ncol(txn)])
# dup_f <- data.frame(dup_flag)
# total_dup <- (dup_f[which(dup_f==TRUE),1])
# txn.dup <- txn[dup_flag,]
# txn.dup <- txn.dup[,c('card_no','txn_hour','txn_amount','mer_cat_code','mer_id','txn_date')]
# txn.dup <- setorderv(txn.dup,c('card_no','txn_hour','txn_amount'), c(1,1,1))

# Convert format to date
cc$cc_pos_dt <- as.Date(cc$cc_pos_dt, format='%Y-%m-%d')
cc$cc_open_dt <- as.Date(cc$cc_open_dt, format='%Y-%m-%d')
cus$cus_pos_dt <- as.Date(cus$cus_pos_dt, format='%Y-%m-%d')
txn$txn_date <- as.Date(txn$txn_date, format='%Y-%m-%d')

# Work with Expired date
cc$cc_exp_org <-cc$cc_exp_dt
cc$cc_exp_dt <- as.character(cc$cc_exp_dt)
cc$cc_exp_year <-  paste0("20",substr(cc$cc_exp_dt,nchar(cc$cc_exp_dt)-1,nchar(cc$cc_exp_dt)))
cc$cc_exp_month <-  substr(cc$cc_exp_dt,1,nchar(cc$cc_exp_dt)-2)
cc$cc_exp_mm <-  paste0("0000",cc$cc_exp_month)
cc$cc_exp_month <-  substr(cc$cc_exp_mm,nchar(cc$cc_exp_mm)-1,nchar(cc$cc_exp_mm))
cc$cc_exp_dtm <- paste0(cc$cc_exp_year,"-",cc$cc_exp_month,"-01")
cc$cc_exp_dt <- as.Date(cc$cc_exp_dtm, format='%Y-%m-%d')

# Exclude some columns from cc
cols_cc <- names(cc)[!names(cc) %in% c("cc_exp_year", "cc_exp_month", "cc_exp_mm", "cc_exp_dtm")]
cc <- cc[,cols_cc]

# Introduce new column for open and last_active days
cc$cc_open_days <-  difftime(Sys.Date(),cc$cc_open_dt,units='days')%>% as.numeric()
cc$cc_exp_days <-  difftime(cc$cc_exp_dt,cc$cc_pos_dt,units='days')%>% as.numeric()
txn$txn_days <- difftime(Sys.Date(),txn$txn_date,units='days')%>% as.numeric()

# Excluded records that cus_age < cutoff_age
# Assumption from credit card application's conditions (Additional card(15years), Main card(20years))
cutoff_age = 14
cus$cus_age <- as.numeric(cus$cus_age)
cus.x <- cus[which(cus$cus_age > cutoff_age),]
#hist(cus.x$cus_age)
#summary(cus.x$cus_age)

# Combine 3 tables - (cus >> cc) >> txn
cus_cc <- merge(x=cc, y=cus, by="cst_id", all.x= TRUE)
data <- merge(x=txn, y=cus_cc, by = "card_no", all.x=TRUE)


##### Column Name List
##### Main 	-  'card_no','cst_id'
##### CC-B 	-  'cc_exp_org' #Backup
##### Date 	-  'txn_date','cc_pos_dt','cc_open_dt','cc_exp_dt','cus_pos_dt'
##### CC 	  -  'cc_bill_cyc','cc_cr_lmt_amt','cc_prev_cr_lmt_amt'
##### CUS 	-  'cus_incm_amt','cus_age','cus_main_zip_cd','cus_cr_line_amt'
##### Txn 	-  'txn_hour','txn_amount','mer_cat_code','mer_id'
##### New 	-  'txn_days','cc_open_days','cc_exp_days'
cols_D=c('card_no','cst_id','cc_bill_cyc','cc_cr_lmt_amt','cc_prev_cr_lmt_amt',
         'cus_incm_amt','cus_age','cus_main_zip_cd','cus_cr_line_amt',
         'txn_hour','txn_amount','mer_cat_code','mer_id',
         'txn_days','cc_open_days','cc_exp_days')
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
train <- merge(x=train.org, y=D, by = "card_no", all.x=TRUE)
test <- merge(x=test.org, y=D, by = "card_no", all.x=TRUE)

# Move column 'is_merchant' to be the first column
cols_T =c('npl_flag',cols_D)
train <- train[,cols_T]


# Convert data to xgboost format
dtrain <- xgb.DMatrix(data = data.matrix(train[, 2:ncol(train)]), label = train$npl_flag)
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
sub <- data.frame(card_no= test$card_no, label = out)

# Observe distribution of predicted data
hist(sub$label,xlim = range(0:max(sub$label)))

# Convert pop to flag 1 or 0
label<-sub$label
CutOff = mean(label)+1*(sqrt(mean(label^2)-mean(label)^2))

p_h <- rownames(sub)[which(sub$label>CutOff)]
for (row in p_h) set(sub, j = as.integer(2), i=as.integer(row), value = 1)

p_l <- rownames(sub)[which(sub$label<=CutOff)]
for (row in p_l) set(sub, j = as.integer(2), i=as.integer(row), value = 0)

# Assign "is_merchant" flag using mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

SB <- aggregate(label ~ card_no, sub, Mode)

# Reorder outcome data to be matched with the test data
TS <-test.org
TS$label <- with(SB, label[match(TS$card_no,SB$card_no)])
OC <- data.frame(npl_flag=TS$label)

# Export to file

# To analysis
#write.table(cus, file = "./analysis/cus.txt", col.names=TRUE ,row.names = FALSE, quote=FALSE)

# To output
write.table(OC, file = "./output/1.txt", col.names=FALSE ,row.names = FALSE, quote=FALSE)
