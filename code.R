# Info about computer
sessionInfo()

# download training and test set
dlMethod <- "curl" # sets default for OSX / Linux

train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(substr(Sys.getenv("OS"),1,7) == "Windows") dlMethod <- "wininet"

if(!file.exists("pml-training.csv")) {
    download.file(train_url,
                  "pml-training.csv",  # stores file in R working directory
                  method=dlMethod # use OS-appropriate method
    )
}

if(!file.exists("pml-testing.csv")) {
    download.file(test_url,
                  "pml-testing.csv", 
                  method=dlMethod
    )
}

# Read
library(readr)
train <- read_csv("pml-training.csv", na = c("", "NA", "#DIV/0!"))
test <- read_csv("pml-testing.csv", na = c("", "NA"))

# Getting rid of column showing number of rows
train <- train[, -1]
test <- test[, -1]

# Change row types in bulk
numeric_cols <- 6:158

train[, numeric_cols] <- train[, numeric_cols] %>% apply(MARGIN = 2, FUN = function(x) as.numeric(x))
test[, numeric_cols] <- test[, numeric_cols] %>% apply(MARGIN = 2, FUN = function(x) as.numeric(x))

# Find columns in test set where all values are NAs
cols_all_NAs <- sapply(test, function(x) all(is.na(x)))

# Get rid of these columns in both test and train set
test <- test[, !cols_all_NAs]
train <- train[, !cols_all_NAs]

# create two vectors to compare column types in train and test, except the last column
train_col_classes <- sapply(train[, -59], class)
test_col_classes <- sapply(test[, -59], class)

# Compare columns classes
identical(train_col_classes, test_col_classes) # they are identical

# Change response variable type to factor
train$classe <- as.factor(train$classe)

# Lets split train set into dependant variables and independant variable
train_x <- train[, -59]
train_y <- train$classe

# As for the test set, we don't need the last column
test_x <- test[, -59]

## Next - we will combine two sets with dependant variables for further data cleaning
# Save the row number for later split into train and test
split_row <- nrow(train_x)

# Combine two sets for further cleanning
data <- bind_rows(train_x, test_x)

# Change date column to an appropriate format
library(lubridate)
data$cvtd_timestamp <- dmy_hm(data$cvtd_timestamp)

# Remove other unnessary date columns
data$raw_timestamp_part_1 <- NULL
data$raw_timestamp_part_2 <- NULL

# Change char to factor columns
data$user_name <- as.factor(data$user_name)
data$new_window <- as.factor(data$new_window)

# Create a weekday variable from date
data$weekday <- weekdays(data$cvtd_timestamp)
data$weekday <- as.factor(data$weekday)

# Delete date column
data$cvtd_timestamp <- NULL

# Split data back to train_x and test_x
train_x <- data[1:split_row, ]
test_x <- data[(split_row + 1):nrow(data), ]


# Modeling
library(caret)
library(ranger)

myGrid <- expand.grid(mtry = c(2, 5, 10, 20, 30), splitrule = "gini")

# We will use near zero variance technique to get rid of such variables,
    # and for missing values we will use median imputation
train <- bind_cols(train_x, classe=train_y)

set.seed(42)
model_rf <- train(classe ~ ., data = train,
                  method = "ranger",
                  #metric = "ROC",
                  na.action = "na.pass",
                  preProcess = c("nzv","medianImpute"),
                  trControl = trainControl(method = "cv", number = 5, verbose = TRUE),
                  tuneGrid = myGrid
                  )

plot(model_rf)

predicted <- predict(model_rf, newdata = test_x, na.action = "na.pass")
predicted

