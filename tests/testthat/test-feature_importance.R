# tests for functions in get_features_importance
options(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)
# Without this, underlying code in either stats or base R causes this warning in several places:
#   warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'


# find_permuted_perf_metric
test_that("find_permuted_perf_metric works", {
  skip_on_cran()
  set.seed(2019)
  expect_equal(
    find_permuted_perf_metric(
      otu_mini_bin_results_glmnet$test_data,
      otu_mini_bin_results_glmnet$trained_model,
      "dx", caret::multiClassSummary,
      "AUC", TRUE, "Otu00049",
      0.6473684
    ),
    c(
      perf_metric = 0.647368421052632,
      perf_metric_diff = -2.1052632526164e-08
    ),
    tol = 10e-5
  )
  set.seed(2019)
  expect_equal(
    find_permuted_perf_metric(
      otu_mini_bin_results_glmnet$test_data,
      otu_mini_bin_results_glmnet$trained_model,
      "dx", caret::multiClassSummary,
      "AUC", TRUE, "Otu00049|Otu00050",
      0.6473684
    ),
    c(
      perf_metric = 0.647368421052632,
      perf_metric_diff = -2.1052632526164e-08
    ),
    tol = 10e-5
  )
  set.seed(2019)
  expect_equal(
    find_permuted_perf_metric(
      otu_mini_bin_results_glmnet$test_data %>%
        dplyr::as_tibble(),
      otu_mini_bin_results_glmnet$trained_model,
      "dx", caret::multiClassSummary,
      "AUC", TRUE, "Otu00009",
      0.6473684
    ),
    c(
      perf_metric = 0.639315789473684,
      perf_metric_diff = 0.00805261052631601
    ),
    tol = 10e-5
  )
})

feat_imp <- structure(list(perf_metric = c(
  0.638605263157895, 0.649368421052632,
  0.631842105263158, 0.628052631578947, 0.597105263157895, 0.635210526315789,
  0.640236842105263, 0.603184210526316, 0.644394736842105, 0.638421052631579
), perf_metric_diff = c(
  0.00876315789473692, -0.00200000000000022,
  0.0155263157894737, 0.0193157894736846, 0.0502631578947367, 0.0121578947368423,
  0.00713157894736849, 0.0441842105263159, 0.00297368421052646,
  0.00894736842105293
), names = structure(c(
  9L, 5L, 10L, 1L, 8L,
  4L, 3L, 2L, 7L, 6L
), .Label = c(
  "Otu00001", "Otu00002", "Otu00003",
  "Otu00004", "Otu00005", "Otu00006", "Otu00007", "Otu00008", "Otu00009",
  "Otu00010"
), class = "factor"), method = c(
  "glmnet", "glmnet",
  "glmnet", "glmnet", "glmnet", "glmnet", "glmnet", "glmnet", "glmnet",
  "glmnet"
), perf_metric_name = c(
  "AUC", "AUC", "AUC", "AUC", "AUC",
  "AUC", "AUC", "AUC", "AUC", "AUC"
), seed = c(
  2019, 2019, 2019,
  2019, 2019, 2019, 2019, 2019, 2019, 2019
)), row.names = c(
  NA,
  -10L
), class = "data.frame")

test_that("feature importances are correct", {
  set.seed(2019)
  expect_equal(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$trained_model$trainingData %>%
        dplyr::rename(dx = .outcome),
      otu_mini_bin_results_glmnet$test_data,
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019,
      corr_thresh = 1
    ),
    feat_imp
  )
})
test_that("feature importances are correct when tibbles used", {
  skip_on_cran()
  set.seed(2019)
  expect_equal(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$trained_model$trainingData %>%
        dplyr::rename(dx = .outcome),
      otu_mini_bin_results_glmnet$test_data %>%
        dplyr::as_tibble(),
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019,
      corr_thresh = 1
    ),
    feat_imp
  )
})

test_that("custom grouped features works", {
  skip_on_cran()
  set.seed(2019)
  feats <- otu_mini_bin_results_glmnet$trained_model$trainingData %>%
    dplyr::rename(dx = .outcome) %>%
    split_outcome_features(., "dx") %>%
    .$features
  groups <- group_correlated_features(feats, corr_thresh = 0.5)

  expect_equal(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$trained_model$trainingData %>%
        dplyr::rename(dx = .outcome),
      otu_mini_bin_results_glmnet$test_data %>%
        dplyr::as_tibble(),
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019,
      groups = groups
    ),
    data.frame(
      perf_metric = c(
        0.638605263157895, 0.631263157894737,
        0.630815789473684, 0.59628947368421, 0.637368421052632, 0.6425,
        0.636, 0.584657894736842
      ),
      perf_metric_diff = c(
        0.00876315789473692,
        0.0161052631578947, 0.0165526315789474, 0.0510789473684211, 0.01,
        0.00486842105263163, 0.0113684210526316, 0.0627105263157894
      ),
      names = structure(c(7L, 8L, 1L, 6L, 3L, 5L, 4L, 2L),
        .Label = c(
          "Otu00001", "Otu00003|Otu00005|Otu00002", "Otu00004", "Otu00006", "Otu00007",
          "Otu00008", "Otu00009", "Otu00010"
        ), class = "factor"
      ),
      method = c(
        "glmnet", "glmnet", "glmnet", "glmnet",
        "glmnet", "glmnet", "glmnet", "glmnet"
      ),
      perf_metric_name = c(
        "AUC", "AUC", "AUC", "AUC",
        "AUC", "AUC", "AUC", "AUC"
      ),
      seed = c(
        2019, 2019, 2019, 2019,
        2019, 2019, 2019, 2019
      )
    )
  )
})
