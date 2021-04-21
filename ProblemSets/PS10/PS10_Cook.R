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
# Make sure discrete variables are formatted as factors
income %<>% mutate(across(c(high.earner,education,marital.status,race,workclass,occupation,relationship,sex), as.factor))
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

#so many packages
library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)
library(rpart)
library(parsnip)
library(modelsummary)
library(e1071)
library(kknn)
library(nnet)
library(kernlab)


#use parsnip to train the first model, logistic regression

logreg_spec <- logistic_reg(
  penalty = tune(), #add a penalty and speficy LASSO
  mixture = 1 #1=LASSO
) %>%       #set the specification and engine
  set_engine("glmnet") %>%   
  set_mode("regression") # Declare a mode: regression or classification

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 3-fold cross-validation
rec_folds <- vfold_cv(income.train, v = 3)

# Workflow
rec_wf <- workflow() %>%
  add_model(logreg_spec) %>%
  add_formula(income$high.earner ~ income$education+income$marital.status+
                income$race+income$workclass+income$occupation+income$relationship+income$age+
                income$sex+income$capital.gain+income$capital.loss+income$hours)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )
#optimal lambda
top_rmse  <- show_best(rec_res, metric = "accuracy")
best_rmse <- select_best(rec_res, metric = "accuracy")

# Now train with tuned lambda
final_lasso <- finalize_workflow(rec_wf, best_rmse)

# Print out results in test set
logreg_test <- last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print
# show best accuracy
top_acc %>% print(n = 1)

# find those optimal parameter values
top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_svm <- finalize_workflow(rec_wf, 
                               best_acc)

###
#Now I'm trying to do trees
###
tree_spec <- decision_tree(
  min_n = tune(),
  tree_depth = tune(),
  cost_complexity = tune()
) %>%       #set the specification and engine
  set_engine("rpart") %>%   
  set_mode("classification") # Declare a mode: regression or classification

#make some sets for the regularization parameters - there are so many yeehaw
tree_param_df1 <- tibble(cost_complexity=seq(.001,.2,by=0.5))
tree_param_df2 <- tibble(min_n = seq(10,100,by=10))
tree_param_df3 <- tibble(tree_depth = seq(5,20,by=5))
# cross-validate
tree_param_df  <- full_join(tree_param_df1,tree_param_df2,by=character()) %>% full_join(.,tree_param_df3,by=character())

#3-fold cross validation
rec_folds <- vfold_cv(income.train, v = 3)

# Workflow
rec_wf <- workflow() %>%
  add_model(tree_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + 
                occupation + relationship + sex + age + capital.gain + capital.loss + hours)
# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = tree_param_df
  )

#optimal parameters 
top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_tree <- finalize_workflow(rec_wf, best_acc)


###
#Yay (not really) now neural networks
###
neural_spec <- mlp(
  hidden_units = tune(),
  penalty = tune()
) %>%       #set the specification and engine
  set_engine("nnet") %>%   
  set_mode("classification") # Declare a mode: regression or classification

#make some regularization set things
nnet_param_df1 <- tibble(hidden_units = seq(1,10))
lambda_grid   <- grid_regular(penalty(), levels = 10)
# stick em together 
nnet_param_df  <- full_join(nnet_param_df1,lambda_grid,by=character())

# 3-fold cross-validation
rec_folds <- vfold_cv(income.train, v = 3)

# Workflow
rec_wf <- workflow() %>%
  add_model(neural_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation +
                relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = nnet_param_df
  )
# find those optimal parameter values
top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_svm <- finalize_workflow(rec_wf, 
                               best_acc)

###
# Now KNN
###
neighbor_spec <- nearest_neighbor(
  neighbors = tune()
) %>%       #set the specification and engine
  set_engine("kknn") %>%   
  set_mode("classification") # Declare a mode: regression or classification

#make a set thingy
knn_grid <- tibble(neighbors = seq(1,30))

# 3-fold cross-validation
rec_folds <- vfold_cv(income.train, v = 3)

# Workflow
rec_wf <- workflow() %>%
  add_model(neighbor_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + 
                relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = knn_grid
  )
# find those optimal parameter values
top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_svm <- finalize_workflow(rec_wf, 
                               best_acc)

###
#Now SVM
###
svm_spec <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune()
) %>%       #set the specification and engine
  set_engine("kernlab") %>%   
  set_mode("classification") # Declare a mode: regression or classification

#make the set for the parameters
svm_grid1 <- tibble(cost      = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
svm_grid2 <- tibble(rbf_sigma = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
# stick em together 
svm_param_df  <- full_join(svm_grid1,svm_grid2,by=character())

# 3-fold cross-validation
rec_folds <- vfold_cv(income_train, v = 3)

# Workflow
rec_wf <- workflow() %>%
  add_model(svm_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation +
                relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = svm_param_df
  )

# find those optimal parameter values
top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_svm <- finalize_workflow(rec_wf, 
                               best_acc)

