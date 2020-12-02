#' Get feature importance using permutation method
#'
#' Calculates feature importance using a trained model and test data. Requires
#' the `future.apply` package.
#'
#' @param train_data Training data: dataframe of outcome and features.
#' @inheritParams run_ml
#' @inheritParams calc_perf_metrics
#'
#' @return Dataframe with performance metrics for when each feature (or group of
#'   correlated features; `names`) is permuted (`perf_metric`), and differences
#'   between test performance metric and permuted performance metric
#'   (`perf_metric_diff`). The performance metric name (`perf_metric_name`) and
#'   seed (`seed`) are also returned.
#'
#' @examples
#' \donttest{
#' results <- run_ml(otu_small, "glmnet", kfold = 2, cv_times = 2)
#' names(results$trained_model$trainingData)[1] <- "dx"
#' get_feature_importance(results$trained_model,
#'   results$trained_model$trainingData, results$test_data,
#'   "dx",
#'   multiClassSummary, "AUC",
#'   class_probs = TRUE, method = "glmnet"
#' )
#' }
#'
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
get_feature_importance <- function(trained_model, train_data, test_data,
                                   outcome_colname, perf_metric_function,
                                   perf_metric_name, class_probs, method,
                                   seed = NA, corr_thresh = 1) {
  abort_packages_not_installed("future.apply")

  # get outcome and features
  split_dat <- split_outcome_features(train_data, outcome_colname)
  outcome <- split_dat$outcome
  features <- split_dat$features

  corr_mat <- get_corr_feats(features, corr_thresh = corr_thresh)
  corr_mat <- dplyr::select_if(corr_mat, !(names(corr_mat) %in% c("corr")))

  grps <- group_correlated_features(corr_mat, features)

  test_perf_value <- calc_perf_metrics(
    test_data,
    trained_model,
    outcome_colname,
    perf_metric_function,
    class_probs
  )[[perf_metric_name]]
  imps <- future.apply::future_lapply(grps, function(feat) {
    return(find_permuted_perf_metric(
      test_data, trained_model, outcome_colname,
      perf_metric_function, perf_metric_name,
      class_probs, feat, test_perf_value
    ))
  }, future.seed = seed) %>%
    dplyr::bind_rows()

  return(as.data.frame(imps) %>%
    dplyr::mutate(
      names = factor(grps),
      method = method,
      perf_metric_name = perf_metric_name,
      seed = seed
    ))
}

#' Get permuted performance metric difference for a single feature
#' (or group of features)
#'
#' Requires the `future.apply` package
#'
#' @param feat feature or group of correlated features to permute.
#' @param test_perf_value value of the true performance metric on the held-out
#'   test data.
#' @param nperms number of permutations to perform (default: `100`).
#' @inheritParams run_ml
#' @inheritParams get_feature_importance
#'
#' @return vector of mean permuted auc and mean difference between test and
#'   permuted auc
#' @noRd
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
find_permuted_perf_metric <- function(test_data, trained_model, outcome_colname,
                                      perf_metric_function, perf_metric_name,
                                      class_probs, feat,
                                      test_perf_value,
                                      nperms = 100) {

  # The code below uses a bunch of base R subsetting that doesn't work with tibbles.
  # We should probably refactor those to use tidyverse functions instead,
  # but for now this is a temporary fix.
  test_data <- as.data.frame(test_data)

  # permute grouped features together
  fs <- strsplit(feat, "\\|")[[1]]
  # only include ones in the test data split
  fs <- fs[fs %in% colnames(test_data)]
  # get the new performance metric and performance metric differences
  n_rows <- nrow(test_data)
  perm_perfs <- sapply(seq(1, nperms), function(x) {
    permuted_test_data <- test_data
    # this strategy works for any number of features
    rows_shuffled <- sample(n_rows)
    permuted_test_data[, fs] <- permuted_test_data[rows_shuffled, fs]
    return(
      calc_perf_metrics(
        permuted_test_data,
        trained_model,
        outcome_colname,
        perf_metric_function,
        class_probs
      )[[perf_metric_name]]
    )
  })
  mean_perm_perf <- sum(perm_perfs) / nperms
  return(c(
    perf_metric = mean_perm_perf,
    perf_metric_diff = test_perf_value - mean_perm_perf
  ))
}
