otu_mini_cont_results_glmnet <- mikropml::run_ml(otu_mini_bin[, 2:11], # use built-in hyperparams
  "glmnet",
  outcome_colname = "Otu00001",
  find_feature_importance = TRUE,
  seed = 2019,
  cv_times = 2
)
usethis::use_data(otu_mini_cont_results_glmnet, overwrite = TRUE)
