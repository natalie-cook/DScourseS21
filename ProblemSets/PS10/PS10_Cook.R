set.seed(100)

income <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

library(tidymodels)
library(glmnet)
library(rpart)
library(parsnip)

#use parsnip to train the first model, logistic regression

logreg_spec <- logistic_reg(
  penalty = tune(), #add a penalty and speficy LASSO
  mixture = 1 #1=LASSO
) %>%       #set the specification and engine
  set_engine("glmnet") %>%   
  set_mode("regression") # Declare a mode: regression or classification
logreg_fit <- logreg_spec %>%
  fit(income.train$high.earner ~ ., data=income.train)
# inspect coefficients
tidy(logreg_fit$fit$coefficients) %>% print
tidy(est.logreg) %>% print

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 3-fold cross-validation
rec_folds <- vfold_cv(income.train, v = 3)

# Workflow
rec_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(log(medv) ~ .)
# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )
#optimal lambda
top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

# Now train with tuned lambda
final_lasso <- finalize_workflow(rec_wf, best_rmse)
# Print out results in test set
last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print
# show best RMSE
top_rmse %>% print(n = 1)

###
#Now I'm trying to do trees
###
tree_spec <- decision_tree() %>%       #set the specification and engine
  set_engine("rpart") %>%   
  set_mode("classification") # Declare a mode: regression or classification
tree_fit <- tree_spec %>%
  fit(income.train$high.earner ~ ., data=income.train)
# inspect coefficients
tidy(tree_fit$fit$coefficients) %>% print
tidy(est.tree) %>% print

#should use the following
min_n()
tree_depth()
cost_complexity()

###
#Yay (not really) now neural networks
###
neural_spec <- mlp() %>%       #set the specification and engine
  set_engine("nnet") %>%   
  set_mode("classification") # Declare a mode: regression or classification
neural_fit <- tree_spec %>%
  fit(income.train$high.earner ~ ., data=income.train)
# inspect coefficients
tidy(neural_fit$fit$coefficients) %>% print
tidy(est.neural) %>% print

#should use:
hidden_units()
penalty()

###
# Now KNN
###
neighbor_spec <- nearest_neighbor() %>%       #set the specification and engine
  set_engine("kknn") %>%   
  set_mode("classification") # Declare a mode: regression or classification
neighbor_fit <- tree_spec %>%
  fit(income.train$high.earner ~ ., data=income.train)
# inspect coefficients
tidy(neighbor_fit$fit$coefficients) %>% print
tidy(est.neighbor) %>% print

#should use
neighbors()
knn_grid <- tibble(neighbors = seq(1,30)

###
#Now SVM
###
svm_spec <- svm_rbf() %>%       #set the specification and engine
  set_engine("kernlab") %>%   
  set_mode("classification") # Declare a mode: regression or classification
svm_fit <- tree_spec %>%
  fit(income.train$high.earner ~ ., data=income.train)
# inspect coefficients
tidy(svm_fit$fit$coefficients) %>% print
tidy(est.svm) %>% print

#should use
cost()
rbf_sigma()

