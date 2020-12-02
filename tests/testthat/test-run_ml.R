options(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)
# Without this, underlying code in either stats or base R causes this warning in several places:
#   warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'


get_all_but_model <- function(ml_results) {
  return(ml_results[names(ml_results) != "trained_model"])
}

expect_equal_ml_results <- function(result1, result2, tol = 1e-5) {
  return(
    eval(bquote(expect_equal(get_all_but_model(result1),
      get_all_but_model(result2),
      tolerance = tol
    )))
  )
}

otu_mini_group <- c(
  "B", "F", "E", "D", "A", "F", "F", "D", "E", "B", "F", "F",
  "E", "A", "B", "A", "E", "A", "D", "A", "D", "A", "C", "A", "B",
  "B", "E", "F", "F", "A", "E", "B", "F", "C", "D", "D", "C", "A",
  "E", "E", "B", "B", "F", "A", "F", "C", "D", "D", "F", "D", "D",
  "A", "D", "F", "C", "A", "D", "D", "B", "F", "C", "F", "E", "C",
  "F", "F", "E", "C", "B", "D", "B", "D", "F", "C", "F", "B", "B",
  "B", "B", "B", "B", "B", "F", "C", "D", "D", "E", "A", "F", "E",
  "D", "E", "A", "D", "F", "A", "E", "E", "C", "B", "B", "E", "B",
  "F", "C", "F", "C", "D", "A", "F", "A", "F", "E", "E", "C", "F",
  "A", "C", "E", "A", "D", "A", "C", "D", "E", "E", "A", "A", "B",
  "F", "E", "C", "B", "B", "C", "C", "D", "C", "E", "E", "F", "F",
  "E", "C", "E", "F", "D", "D", "B", "B", "B", "E", "E", "A", "A",
  "A", "B", "D", "B", "D", "F", "F", "F", "B", "B", "B", "F", "F",
  "D", "B", "B", "D", "B", "D", "C", "C", "B", "F", "A", "F", "A",
  "F", "D", "C", "E", "E", "F", "A", "B", "B", "A", "B", "A", "B",
  "E", "A", "C", "E", "F", "A"
)

test_that("run_ml works for logistic regression", {
  expect_equal_ml_results(
    expect_warning(
      run_ml(otu_mini_bin, # use built-in hyperparameters
        "glmnet",
        outcome_colname = "dx",
        find_feature_importance = FALSE,
        seed = 2019,
        cv_times = 2
      ),
      "`caret::train\\(\\)` issued the following warning:"
    ),
    otu_mini_bin_results_glmnet
  )
})

test_that("run_ml works for linear regression", {
  skip_on_cran()
  expect_equal_ml_results(
    expect_message(expect_warning(run_ml(otu_mini_bin[, 2:11], # use built-in hyperparameters
      "glmnet",
      outcome_colname = "Otu00001",
      find_feature_importance = TRUE,
      seed = 2019,
      cv_times = 2
    ), "Data is being considered numeric, but all outcome values are integers. If you meant to code your values as categorical, please use character values.")),
    otu_mini_cont_results_glmnet
  )
})

test_that("run_ml works for random forest with grouping & feature importance", {
  skip_on_cran()
  expect_equal_ml_results( # use built-in hyperparams function
    mikropml::run_ml(otu_mini_bin,
      "rf",
      outcome_colname = "dx",
      find_feature_importance = TRUE,
      seed = 2019,
      cv_times = 2,
      groups = otu_mini_group
    ),
    otu_mini_bin_results_rf,
    tol = 1e-3
  )
})

test_that("run_ml works for svmRadial", {
  skip_on_cran()
  expect_equal_ml_results(
    expect_warning(mikropml::run_ml(otu_mini_bin,
      "svmRadial",
      outcome_colname = "dx",
      find_feature_importance = FALSE,
      seed = 2019,
      cv_times = 2
    )),
    otu_mini_bin_results_svmRadial
  )
})

test_that("run_ml works for xgbTree", {
  skip_on_cran()
  skip_on_os(c("linux", "windows")) # bug in xgboost package: https://discuss.xgboost.ai/t/colsample-by-tree-leads-to-not-reproducible-model-across-machines-mac-os-windows/1709
  expect_equal_ml_results(
    mikropml::run_ml(
      otu_mini_bin,
      "xgbTree",
      outcome_colname = "dx",
      find_feature_importance = FALSE,
      seed = 2019,
      cv_times = 2
    ),
    otu_mini_bin_results_xgbTree,
    tol = 1e-3
  )
})

test_that("run_ml works for rpart2", {
  skip_on_cran()
  expect_equal_ml_results(
    mikropml::run_ml(otu_mini_bin,
      "rpart2",
      outcome_colname = "dx",
      find_feature_importance = FALSE,
      seed = 2019,
      cv_times = 2
    ),
    otu_mini_bin_results_rpart2
  )
})

test_that("run_ml errors for unsupported method", {
  expect_error(expect_warning(
    run_ml(
      otu_small,
      "not_a_method"
    ),
    "Method 'not_a_method' is not officially supported by mikropml."
  ), "method 'not_a_method' is not supported.")
})

test_that("run_ml errors if outcome_colname not in dataframe", {
  expect_error(
    run_ml(
      otu_small,
      "rf",
      outcome_colname = "not_a_colname"
    ),
    "Outcome 'not_a_colname' not in column names of data."
  )
})
