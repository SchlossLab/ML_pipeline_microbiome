
test_that("get_outcome_type works", {
  expect_equal(get_outcome_type(c(1, 2, 1)), "continuous")
  expect_equal(get_outcome_type(c("a", "b", "b")), "binary")
  expect_equal(get_outcome_type(c("a", "b", "c")), "multiclass")
})
test_that("get_outcome_type errors when num_outcomes < 2", {
  error_msg <- "A continuous, binary, or multi-class outcome variable is required, but this dataset has "
  expect_error(
    get_outcome_type(c(1, 1)),
    error_msg
  )
  expect_error(
    get_outcome_type(c("a", "a", "a")),
    error_msg
  )
  expect_error(
    get_outcome_type(c()),
    error_msg
  )
})

test_that("get_perf_metric_fn works", {
  expect_equal(get_perf_metric_fn("continuous"), caret::defaultSummary)
  expect_equal(get_perf_metric_fn("binary"), caret::multiClassSummary)
  expect_equal(get_perf_metric_fn("multiclass"), caret::multiClassSummary)
  expect_error(get_perf_metric_fn("asdf"), "Outcome type of outcome must be one of:")
})

test_that("get_perf_metric_name works", {
  expect_equal(get_perf_metric_name("continuous"), "RMSE")
  expect_equal(get_perf_metric_name("binary"), "AUC")
  expect_equal(get_perf_metric_name("multiclass"), "logLoss")
  expect_error(get_perf_metric_name("asdf"), "Outcome type of outcome must be one of:")
})

test_that("calc_perf_metrics works", {
  expect_equal(
    calc_perf_metrics(otu_mini_bin_results_glmnet$test_data,
      otu_mini_bin_results_glmnet$trained_model,
      "dx",
      caret::multiClassSummary,
      class_probs = TRUE
    ),
    unlist(c(otu_mini_bin_results_glmnet$performance[, !(colnames(otu_mini_bin_results_glmnet$performance) %in% c("cv_metric_AUC", "method", "seed"))]))
  )
})

test_that("get_performance_tbl works", {
  set.seed(2019)
  expect_equal(
    get_performance_tbl(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$test_data,
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019
    ),
    otu_mini_bin_results_glmnet$performance
  )
  expect_warning(get_performance_tbl(
    otu_mini_bin_results_glmnet$trained_model,
    otu_mini_bin_results_glmnet$test_data,
    "dx",
    caret::multiClassSummary,
    "not_a_perf_metric",
    TRUE,
    "glmnet",
    seed = 2019
  ), "The performance metric provided does not match the metric used to train the data.")
})
