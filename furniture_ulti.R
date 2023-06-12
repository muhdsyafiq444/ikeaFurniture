
# Activation of Packages ----

pacman::p_load(tidyverse, lubridate, janitor, skimr,
               Hmisc, ggstatsplot, GGally,  # for EDA
               tidymodels, bestNormalize, doParallel, # Tidy ML
               klaR, xgboost, ranger, kknn, glmnet, kernlab, nnet, finetune, # algorithms
               themis, stacks, rules, baguette, # stack algorithms
               plotly, ggthemr, ggrepel, # Interactive Data Display
               shiny, shinydashboard, kableExtra, knitr, # shiny
               vip, # Feature Importance
               DT, jtools, huxtable, broom, interactions # report models
               ) # setting your dependencies

setwd("E:/R")

# Import ----

furniture <- read_csv("price_regression.csv")

# Tidy ----

# check for missing using base R
furniture %>% 
  sapply(function(x) sum(is.na (x)
                         )
         )

furniture %>% 
  skim()

furniture %>% 
  glimpse()

# getting all the variables name in tbl
furniture %>% 
  names(.) %>% 
  as_tibble()

# Transformation ----

furniture_cleaned <- 
  furniture %>% 
  mutate(across(c(name, 
                category), 
                as.factor),
         log10_Price = log10(Price)
         ) %>% 
  select(-Price, -item_id)

furniture_cleaned %>% 
  skim()

furniture %>% 
  summary()

# EDA ----

## Missing Data Imputation ----

### Univariate imputation ----

EDA_univariate_impute_recipe <- 
  recipe(formula = log10_Price ~ ., 
         data = furniture_cleaned) %>%
  step_other(name, category, threshold = 0.01) %>%
  step_impute_mean(depth, height, width) %>% 
  step_BoxCox(depth, height, width) %>% 
  step_normalize(all_numeric_predictors() 
                 ) %>% 
  step_dummy(all_nominal_predictors()
             )

EDA_univariate_impute_baked <- 
  EDA_univariate_impute_recipe %>% # plan 
  prep() %>% # for calculation
  bake(new_data = furniture_cleaned) 

skim(EDA_univariate_impute_baked)

EDA_univariate_impute_baked %>%
  select_if(is.numeric
            ) %>% 
  as.matrix(.) %>% 
  rcorr(.) %>% 
  tidy(.) %>% 
  rename(var1 = column1,
         var2 = column2,
         CORR = estimate) %>% 
  mutate(ABScorr = abs(CORR)
         ) %>% 
  select(-c(4,5)
         ) %>% 
  filter(var1 == "log10_Price" | var2 == "log10_Price") %>% 
  arrange(desc(ABScorr)
          ) %>% 
  datatable()

### KNN-based imputation ----

EDA_knn_impute_recipe <- 
  recipe(formula = log10_Price ~ ., 
         data = furniture_cleaned) %>%
  step_other(name, category, threshold = 0.01) %>%
  step_impute_knn(depth, height, width) %>% 
  step_BoxCox(depth, height, width) %>% 
  step_normalize(all_numeric_predictors() 
                 ) %>% 
  step_dummy(all_nominal_predictors()
             )

EDA_knn_impute_baked <- 
  EDA_knn_impute_recipe %>% # plan 
  prep() %>% # for calculation
  bake(new_data = furniture_cleaned) 

skim(EDA_knn_impute_baked)

corr_table <- 
EDA_knn_impute_baked %>%
  select_if(is.numeric
            ) %>% 
  as.matrix(.) %>% 
  rcorr(.) %>% 
  tidy(.) %>% 
  rename(var1 = column1,
         var2 = column2,
         CORR = estimate) %>% 
  mutate(ABScorr = abs(CORR)
         ) %>% 
  select(-c(4,5)
         ) %>% 
 # filter(var1 == "log10_Price" | var2 == "log10_Price") %>% 
  arrange(desc(ABScorr)
          )

  datatable()

### KNN-based imputation with poly ----

EDA_knn_impute_poly_recipe <- 
  recipe(formula = log10_Price ~ ., 
         data = furniture_cleaned) %>%
  step_other(name, category, threshold = 0.01) %>%
  step_impute_knn(depth, height, width) %>% 
  step_BoxCox(depth, height, width) %>% 
  step_normalize(all_numeric_predictors() 
                 ) %>% 
  step_dummy(all_nominal_predictors()
             ) %>% 
  step_poly(., 
            degree = 2)

EDA_knn_impute_poly_baked <- 
  EDA_knn_impute_poly_recipe %>% # plan 
  prep() %>% # for calculation
  bake(new_data = furniture_cleaned) 

skim(EDA_knn_impute_poly_baked)

EDA_knn_impute_poly_baked %>%
  select_if(is.numeric
            ) %>% 
  as.matrix(.) %>% 
  rcorr(.) %>% 
  tidy(.) %>% 
  rename(var1 = column1,
         var2 = column2,
         CORR = estimate) %>% 
  mutate(ABScorr = abs(CORR)
         ) %>% 
  select(-c(4,5)
         ) %>% 
  filter(var1 == "log10_Price" | var2 == "log10_Price") %>% 
  arrange(desc(ABScorr)
          ) %>% 
  datatable()

analysis(furniture_validate$splits[[1]]
         ) %>% 
  lm(log10_Price ~ depth * height,
      .) %>% 
  export_summs(exp = T)

lm_1 <- 
  analysis(furniture_validate$splits[[1]]
           ) %>% 
  lm(log10_Price ~ depth * width * height,
      .
      )

interact_plot(lm_1,
              pred = depth,
              modx = width,
              mod2 = height)

furniture_cleaned %>% 
  ggplot(aes(x = width,
             y = log10_Price)
  ) + 
  geom_point(color = "dodgerblue",
             alpha = 0.25) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = F,
              color = "red") + 
  geom_smooth(method = "lm",
              formula = y ~ poly(x,
                                 degree = 2),
              se = F,
              color = "green") + 
  geom_smooth(method = "loess", # LOESS for EDA
              formula = y ~ x,
              se = F,
              color = "purple") + 
  theme_bw()

# PREDICTIVE MODEL ----

## Splitting ----

set.seed(22062201)

### Planning stage of Split ----

furniture_split <- # this is a random sample scheme
  furniture_cleaned %>% 
  initial_split(prop = .80) 

### Execution stage of Split ----

furniture_training <- 
  furniture_split %>% 
  training()

furniture_testing <- 
  furniture_split %>% 
  testing()

### Validation ----

set.seed(22062202)

furniture_validate <- 
  furniture_training %>% 
  validation_split(prop = 0.9)

## Pre-processing (Feature Engineering)----

recipe_knn_impute <- 
  recipe(formula = log10_Price ~ ., 
         data = analysis(furniture_validate$splits[[1]]
                         )
         ) %>%
  step_other(name, category, threshold = 0.01) %>%
  step_impute_knn(depth, height, width) %>% 
  step_BoxCox(all_numeric_predictors() 
              ) %>% 
  step_normalize(all_numeric_predictors() 
                 ) %>% 
  step_dummy(all_nominal_predictors()
             )

# knn_impute_poly_XGB, rmse = 0.306
# knn_impute_poly_rf, rmse = 0.321

recipe_knn_impute_interact_3 <- 
  recipe(formula = log10_Price ~ ., 
         data = analysis(furniture_validate$splits[[1]]
                         )
         ) %>%
  step_zv(all_numeric_predictors()
          ) %>% 
  step_other(name, category, threshold = 0.01
             ) %>%
  step_impute_knn(depth, height, width
                  ) %>% 
  step_BoxCox(all_numeric_predictors() 
              ) %>% 
  step_normalize(all_numeric_predictors() 
                 ) %>% 
  step_dummy(all_nominal_predictors()
             ) %>% 
  step_interact(terms = ~ width:depth
                ) %>% 
  step_interact(terms = ~ width:height
                ) %>% 
  step_interact(terms = ~ height:depth
                ) %>% 
  step_interact(terms = ~ width:height:depth
                )

recipe_PCA <-
  recipe(formula = log10_Price ~ ., 
         data = analysis(furniture_validate$splits[[1]]
                         )
         ) %>%
  step_zv(all_numeric_predictors()
          ) %>% 
  step_other(name, category, threshold = 0.01
             ) %>%
  step_impute_knn(depth, height, width
                  ) %>% 
  step_BoxCox(all_numeric_predictors() 
              ) %>% 
  step_normalize(all_numeric_predictors()
                 ) %>% 
  step_pca(all_numeric_predictors(),
           num_comp = 2
           ) %>% 
  step_dummy(all_nominal_predictors()
             ) 

recipe_knn_impute_interact_3 %>% 
  prep() %>% 
  bake(new_data = analysis(furniture_validate$splits[[1]]
                           )
       ) 

skim(analysis(furniture_validate$splits[[1]]
              )
     )

## Fitting ----

library(klaR)

OLS <- 
  linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

RF <- 
  rand_forest() %>% # function for random forest algo.
  set_args(mtry = tune(), 
           min_n = tune(), 
           trees = 1000) %>%
  set_engine("ranger",
             importance = "impurity") %>% # for calculation of feature importance 
  set_mode("regression")

XG_BOOST <- 
  boost_tree(trees = 1000,
             mtry = tune(),
             min_n = tune(),
             tree_depth = tune(),
             sample_size = tune(),
             learn_rate = tune()
             ) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

library(glmnet)

LM <- 
  linear_reg(penalty = tune(),
             mixture = tune()
             ) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

library(kernlab) # Kernel-Based ML lab

SVM <- 
  svm_rbf(cost = tune(),
          rbf_sigma = tune()
          ) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

library(kknn) # "Weighted" k-nearest neighbor 

KNN <- 
  nearest_neighbor(neighbors = tune(),
                   weight_func = tune(),
                   dist_power = tune()
                   ) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

library(nnet)

MLP <- # Multi layer perception, single layer neural network
  mlp(penalty = tune(),
      epochs =  tune(),
      hidden_units = tune(),
  ) %>% 
  set_engine("nnet") %>% 
  set_mode("regression")

## Workflowsets ----

CONTROL_TOWER_ML <- 
  workflow_set(preproc = list(A_knn_impute_interact_3 = 
                                recipe_knn_impute_interact_3
                              #pls = recipe_PLS,
                              #pca = recipe_PCA
                              ),
  models = list(mlp = MLP,
                svm = SVM,
                knn = KNN,
                lm = LM,
                random_forest = RF,
                XGB = XG_BOOST)
  ) %>% 
  workflow_map(verbose = T,
               seed = 22062203,
               grid = 10,
               resamples = furniture_validate,
               control = 
                 control_grid(parallel_over = "everything")
               )
RANK <-
  CONTROL_TOWER_ML %>% 
  rank_results(select_best = T)

RANK

RANKINGS_cleaned <- 
  RANK %>% 
  mutate(method = map_chr(wflow_id,
                          ~ str_split(.x,
                                      "_",
                                      simplify = T)[1]
                          )
         ) %>% 
  # filter(rank <= 3 & .metric == "rmse") %>% 
  dplyr::select(wflow_id, model, .config, rmse = mean, rank) %>% 
  group_by(wflow_id) %>% 
  slice_min(rank,
            with_ties = F) %>% 
  ungroup() %>% 
  arrange(rank)

# Visualize the rankings of multiple algorithms 

CONTROL_TOWER_ML %>% 
  autoplot() +
  theme_bw()

# fitted_ensemble <- 
#   stacks() %>% 
#   add_candidates(CONTROL_TOWER_ML) %>% 
#   blend_predictions() %>% 
#   fit_members()

workflow_ID_best <-
  RANKINGS_cleaned %>% 
  slice_min(rank,
            with_ties = F) %>% 
  pull(wflow_id)

workflow_best <-
  CONTROL_TOWER_ML %>% 
  extract_workflow_set_result(workflow_ID_best) %>% 
  select_best(metric = "rmse")

FINALIZED_workflow <- 
  CONTROL_TOWER_ML %>% 
  extract_workflow(workflow_ID_best) %>% # There were multiple models
  finalize_workflow(workflow_best)

best_fit <- 
  FINALIZED_workflow %>% # The optimal parameters for the chosen one
  last_fit(furniture_split)

## Assess ----

### Model Level Metrics ----

best_fit_metrics <-
  best_fit %>% 
  collect_metrics()

best_fit_metrics

### Individual Level Metrics ----

individual_countries_predictions <- 
  best_fit %>% 
  collect_predictions()

individual_countries_predictions

furniture_names <- 
  furniture %>% 
  janitor::clean_names()

### Plotting Metrics ----

for_naming <- 
  furniture_names %>% 
  rowid_to_column(".row")

data_for_plot <- 
  for_naming %>% 
  inner_join(individual_countries_predictions,
             by = ".row") %>% 
  dplyr::select(.row, item_id, name, category, .pred, price) %>% 
  rename(actual_price = price) %>% 
  mutate(predicted_price = 10^.pred)

ggthemr("dust")

set.seed(220070261)

data_for_plot %>% 
  ggplot(aes(x = actual_price,
             y = predicted_price,
             text = name,
             label = item_id)
         ) +
  geom_abline(color = "red",
              lty = 2) + 
  geom_label_repel() + 
  # geom_point() + 
  coord_obs_pred() + # note that this comes from tune::
  scale_x_continuous(labels = scales::dollar) + 
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Actual Price of Furniture",
       y = "Predicted Price of Furniture",
       title = "Predicting IKEA Furniture Price",
       subtitle = "Currency = USD") 

for_plotly <- 
  data_for_plot %>% 
  ggplot(aes(x = actual_price,
             y = predicted_price,
             text = name,
             label = category)
         ) +
  geom_abline(color = "red",
              lty = 2) + 
  geom_point(color = "deepskyblue2",
             alpha = 0.50) + 
 # coord_obs_pred() + # note that this comes from tune::
  scale_x_continuous(labels = scales::dollar) + 
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Actual Price of Furniture",
       y = "Predicted Price of Furniture",
       title = "Predicting IKEA Furniture Price",
       subtitle = "Currency = USD") 

for_plotly %>% 
  ggplotly()

# Variable (Feature) Importance ----

library(vip)

importance <- 
  XG_BOOST %>% 
  finalize_model(workflow_best
                 ) %>% 
  set_engine("xgboost")

model_summary_for_importance <- 
  workflow() %>% 
  add_recipe(recipe_knn_impute_interact_3) %>% 
  add_model(importance) %>% # UPDATED here
  fit(analysis(furniture_validate$splits[[1]]
               )
      ) %>% 
  extract_fit_parsnip() %>% 
  vip::vip(aesthetics = list(fill = "deepskyblue3",
                        alpha = 0.75)
      )

model_summary_for_importance 

# Finalized Model ----

finalized_xgb <- 
  boost_tree(trees = 1000,
             mtry = 28,
             min_n = 14,
             tree_depth = 11,
             sample_size = 0.564700039247982,
             learn_rate = 0.0462100793562759
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

finalized_model <- 
  workflow() %>% 
  add_recipe(recipe_knn_impute_interact_3) %>%
  add_model(finalized_xgb) %>% 
  fit(furniture_cleaned)

# Store Your Algorithm ----
finalized_model %>% 
  saveRDS("furniture_finalized_model.rds")

tables <- list(furniture_cleaned,
               corr_table)

tables %>% 
  saveRDS("furniture_tables.rds")

# Store Your RAM Space ----
save.image("furniture_finalized.RData")
