setwd("~/GitHub/Data Hack")
library(purrr)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(DMwR)

# Load train and test data
train <- read_csv("~/GitHub/Data Hack/train.csv")
test <- read_csv("~/GitHub/Data Hack/test.csv")

# view structure of train and test
glimpse(train) #2618, 87
glimpse(test)  #2553, 86

#checking for NA values
table(is.na(train))
table(is.na(test))
#No NA found

#check the target variable count
table(train$Purchase)
# 0     1(defaulter)
# 2454  164 

#Add temporary purchase column to test
test$Purchase <- rep(0, times = length(test$ID))

#create submit_df dataframe
submit_df <- cbind(ID = test$ID, Purchase = test$Purchase)
submit_df <- data.frame(submit_df)

# Let us check the proportion
prop.table(table(train$Purchase))
#   0             1 
#   0.93735676    0.06264324
# just more than 6% are defaulters 

# Dodged bar chart for comparing each variable with target variable i.e Purchase
all_bar_chart <- function(i, lab, nam){
  ggplot(train, aes(x = factor(i), fill = factor(Purchase))) + geom_bar(position = "dodge") +
    scale_x_discrete(name = nam, labels = lab) + 
    theme(axis.text.x = element_text(angle  = 60,hjust = 1,size = 10))
}

# For Comparing each proportions with target variable
all_prop_table <- function(x, y = train$Purchase){
  round(prop.table(table(x, y)) * 100, 2)
}

# Make vector of L0, L1, L2, L3, L4 will be useful for plotting to mark labels
L0_subtype <- c("High Income, expensive child", "Very Important Provincials", 
                "High status seniors", "Affluent senior apartments", "Mixed seniors",
                "Career and childcare", "Dinki's (double income no kids)", 
                "Middle class families", "Modern, complete families", "Stable family", 
                "Family starters", "Affluent young families", "Young all american family",
                "Junior cosmopolitan", "Senior cosmopolitans", "Students in apartments", 
                "Fresh masters in the city", "Single youth", "Suburban youth", 
                "Etnically diverse", "Young urban have-nots", "Mixed apartment dwellers", 
                "Young and rising", "Young, low educated", "Young seniors in the city", 
                "Own home elderly", "Seniors in apartments", "Residential elderly", 
                "Porchless seniors,  no front yard", "Religious elderly singles", 
                "Low income catholics", "Mixed seniors", "Lower class large families", 
                "Large family, employed child", "Village families", "Couples with teens 
                'Married with children", "Mixed small town dwellers", 
                "Traditional families", "Large religous families", "Large family farms", 
                "Mixed rurals")

L1_years <- c(" 20-30", "30-40", " 40-50", "50-60", "60-70", "70-80")

L2_type <- c("Successful hedonists", "Driven Growers", "Average Family",
             "Career Loners", "Living well", "Cruising Seniors", 
             "Retired and Religeous", "Family with grown ups", "Conservative families",
             "Farmers")

L3_percent <- c("0%", "1 - 10%", "11 - 23%", "24 - 36%", "37 - 49%", "50 - 62%",
                "63 - 75%", "76 - 88%", "89 - 99%", "100%") 
L4_num <- c("0", "1 - 49", "50 - 99", "100 - 199", "200 - 499", "500 - 999",
            "1000 - 4999", "5000 - 9999", "10,000 - 19,999", ">= 20,000")

Number_of_houses <- c(1:10)
Avg_size_household <- c(1:6)

# create a list of all above vectors
L <- list(L0_subtype, L1_years, L2_type, L3_percent, L4_num, Number_of_houses, Avg_size_household)
names(L) <-c("L0_subtype","L1_years", "L2_type", "L3_percent", "L4_num", 
             "Number_of_houses", "Avg_size_household")

# read the file of Description of each variable
abbrevation <- data.frame(read_excel("~/GitHub/Data Hack/abbrevation.xlsx"))

train$ID <- NULL
test$ID <- NULL

# Check Proportions of each variable with Purchase variable
map(train[2:85], all_prop_table)

# Create barplots 
# For Avg Age
map(train[, abbrevation[4,1]], function(x) all_bar_chart(x, L[[abbrevation$Code.type[4]]], 
                                                         abbrevation$Description[4]))
# For Customer Main Type
map(train[, abbrevation[5,1]], function(x) all_bar_chart(x, L[[abbrevation$Code.type[5]]], 
                                                         abbrevation$Description[5]))
# For High Status
map(train[, abbrevation[19,1]], function(x) all_bar_chart(x, L[[abbrevation$Code.type[19]]], 
                                                         abbrevation$Description[19]))
# Avg Income
map(train[, abbrevation[42,1]], function(x) all_bar_chart(x, L[[abbrevation$Code.type[42]]], 
                                                          abbrevation$Description[42]))
# Purchasing Power
map(train[, abbrevation[43,1]], function(x) all_bar_chart(x, L[[abbrevation$Code.type[43]]], 
                                                          abbrevation$Description[43]))


# Join train and test data
combi <- rbind(train, test)
glimpse(combi)


# Function for Chi square
all_chi_table <- function(x, y = train$Purchase){
  tab = table(x, y)
  chisq.test(tab, correct = T)
}

# create chi_value tables
chi_table <- map(train[, 1:85], all_chi_table)
chi_values <- round(map_dbl(chi_table, function(x) x$p.value), 7)

chi_values_df <- as.data.frame(chi_values)
#combine chi_square_values with abbrevation to create a data frame
chi_df <- data.frame(c(abbrevation, chi_values_df))

#Let us subset combi as per p-values obtained from chi square
var_name_ss <- chi_df$Abbrevation[chi_df$chi_values < 0.05]
var_name_ss <- as.character(droplevels(var_name_ss))

#subset combi to combi_ss
combi_ss <- combi[, var_name_ss]

# Split train and test data
train_ss <- combi_ss[1:length(train$Purchase), ]
test_ss <- combi_ss[-(1:length(train$Purchase)), ]

# Add purchase variable to train_ss
train_ss$Purchase <- train$Purchase

#Convert Purchase variable fron integer to factor for SMOTE
train_ss$Purchase <- as.factor(train_ss$Purchase)

# also convert tibble to data frame else SMOTE won't work
train_ss_df <-  as.data.frame(train_ss)
train_ss <- SMOTE(Purchase ~ ., train_ss_df, perc.over = 1000, perc.under = 100)
table(train_ss$Purchase)

# Group values in variable which are very small in proportion together for each variable
# We will use original MOSTYPE variables as it has many variable with small proportions
MOSTYPE <- train_ss$MOSTYPE
# Now convert Purchase variable to numeric
train_ss$Purchase <- as.numeric(train_ss$Purchase) - 1
table(train_ss$Purchase)

# Grouping for each variable in train
for(i in names(train_ss)){
  p <- 5/100
  ld <- names(which(prop.table(table(train_ss[[i]])) < p))
  train_ss[[i]][train_ss[[i]] %in% ld] <- -999
}
# grouping for each variable in test
for(i in names(test_ss)){
  p <- 5/100
  ld <- names(which(prop.table(table(test_ss[[i]])) < p))
  test_ss[[i]][test_ss[[i]] %in% ld] <- -999
}

str(train_ss)
str(test_ss)

# Assigning original values to MOSTYPE
train_ss$MOSTYPE <- MOSTYPE
test_ss$MOSTYPE <- test$MOSTYPE

str(train_ss)
str(test_ss)

# Convert to factor for Xgboost
train_ss_fac <-  map_df(train_ss, as.factor)
test_ss_fac <- map_df(test_ss, as.factor)

str(train_ss_fac)
str(test_ss_fac)

# Convert Purchase variable to integer else Xgboost gives error 
train_ss_fac$Purchase <- as.integer(train_ss_fac$Purchase) - 1
table(train_ss_fac$Purchase)

vars <- names(train_ss_fac)[-45]
library(vtreat)
library(magrittr)

# Create the treatment plan for train data
treatplan <- designTreatmentsZ(train_ss_fac, vars, verbose = FALSE)
summary(treatplan)

# Get the "clean" and "lev" variables from the scoreFrame
newvars <- treatplan %>%
  use_series(scoreFrame) %>%        
  filter(code %in% c("clean", "lev")) %>%  # get the rows you care about
  use_series(varName)

# Prepare the training data
train.treat <- prepare(treatplan, train_ss_fac,  varRestriction = newvars)

# Prepare the test data
test.treat <- prepare(treatplan, test_ss_fac,  varRestriction = newvars)

library(xgboost)

# Run xgb.cv
cv <- xgb.cv(data = as.matrix(train.treat), 
             label = train_ss_fac$Purchase,
             nrounds = 300,
             nfold = 10,
             objective = "binary:logistic",
             eta = 0.1,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0    # silent
)

# Get the evaluation log 
elog <- as.data.frame(cv$evaluation_log)

trees <- elog %>% 
  summarise(ntrees.train = which.min(elog$train_error_mean), 
            ntrees.test  = which.min(elog$test_error_mean))

# The number of trees to use, as determined by xgb.cv
ntrees = trees$ntrees.test


# Run xgboost
model_xgb <- xgboost(data = as.matrix(train.treat), # training data as matrix
                     label = train_ss_fac$Purchase,  # column of outcomes
                     nrounds = ntrees,       # number of trees to build
                     objective = "binary:logistic", # objective
                     eta = 0.1,
                     depth = 6,
                     verbose = 0)  # silent

# Make predictions submission in probablities
test_ss_fac$pred <- predict(model_xgb, as.matrix(test.treat))
submit_df$Purchase <- test_ss_fac$pred
head(submit_df)

write.csv(submit_df, file = "sub_default.csv", row.names = FALSE)

