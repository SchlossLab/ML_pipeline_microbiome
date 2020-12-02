#' Run the machine learning pipeline
#'
#' This function runs machine learning (ML), evaluates the best model,
#' and optionally calculates feature importance using a robust framework
#' outlined in Topçuoğlu _et al._ 2020 ([doi:10.1128/mBio.00434-20](https://doi.org/10.1128/mBio.00434-20)).
#' Required inputs are a dataframe with an outcome variable and other columns
#' as features, as well as the ML method.
#' See `vignette('introduction')` for more details.
#'
#' @param dataset Dataframe with an outcome variable and other columns as features.
#' @param method ML method.
#'   Options: `c("glmnet", "rf", "rpart2", "svmRadial", "xgbTree")`.
#'   - glmnet: linear, logistic, or multiclass regression
#'   - rf: random forest
#'   - rpart2: decision tree
#'   - svmRadial: support vector machine
#'   - xgbTree: xgboost
#' @param outcome_colname Column name as a string of the outcome variable
#'   (default `NULL`; the first column will be chosen automatically).
#' @param hyperparameters Dataframe of hyperparameters
#'   (default `NULL`; sensible defaults will be chosen automatically).
#' @param seed Random seed (default: `NA`).
#'  Your results will only be reproducible if you set a seed.
#' @param find_feature_importance Run permutation importance (default: `FALSE`).
#'   `TRUE` is recommended if you would like to identify features important for
#'   predicting your outcome, but it is resource-intensive.
#' @param kfold Fold number for k-fold cross-validation (default: `5`).
#' @param cv_times Number of cross-validation partitions to create (default: `100`).
#' @param training_frac Fraction of data for training set (default: `0.8`).
#'   The remaining data will be used in the testing set.
#' @param perf_metric_function Function to calculate the performance metric to
#'   be used for cross-validation and test performance. Some functions are
#'   provided by caret (see [caret::defaultSummary()]).
#'   Defaults: binary classification = `twoClassSummary`,
#'             multi-class classification = `multiClassSummary`,
#'             regression = `defaultSummary`.
#' @param perf_metric_name The column name from the output of the function
#'   provided to perf_metric_function that is to be used as the performance metric.
#'   Defaults: binary classification = `"ROC"`,
#'             multi-class classification = `"logLoss"`,
#'             regression = `"RMSE"`.
#' @param groups Vector of groups to keep together when splitting the data into
#'  train and test sets, and for cross-validation.
#'  length matches the number of rows in the dataset (default: `NULL`).
#' @param corr_thresh For feature importance, group correlations
#'   above or equal to `corr_thresh` (range `0` to `1`; default: `1`).
#' @param ntree For random forest, how many trees to use (default: 1000).
#'   Note that caret doesn't allow this parameter to be tuned.
#'
#' @return
#'
#' Named list with results:
#' - `trained_model`: Output of [caret::train()], including the best model.
#' - `test_data`: Part of the data that was used for testing.
#' - `performance`: Dataframe of performance metrics. The first column is the cross-validation performance metric, and the last two columns are the ML method used and the seed (if one was set), respectively. All other columns are performance metrics calculated on the test data. This contains only one row, so you can easily combine performance dataframes from multiple calls to `run_ml()` (see `vignette("parallel")`).
#' - `feature_importance`: If feature importances were calculated, a dataframe where each row is a feature or correlated group. The columns are the performance metric of the permuted data, the difference between the true performance metric and the performance metric of the permuted data (true - permuted), the feature name, the ML method, the performance metric name, and the seed (if provided). For AUC and RMSE, the higher perf_metric_diff is, the more important that feature is for predicting the outcome. For log loss, the lower perf_metric_diff is, the more important that feature is for predicting the outcome.
#'
#' @section More details:
#'
#' For more details, please see [the vignettes](http://www.schlosslab.org/mikropml/articles/).
#'
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' \donttest{
#' run_ml(otu_small, "glmnet",
#'   seed = 2019
#' )
#' run_ml(otu_small, "rf",
#'   outcome_colname = "dx",
#'   find_feature_importance = TRUE
#' )
#' }
run_ml <-
  function(dataset,
           method,
           outcome_colname = NULL,
           hyperparameters = NULL,
           find_feature_importance = FALSE,
           kfold = 5,
           cv_times = 100,
           training_frac = 0.8,
           perf_metric_function = NULL,
           perf_metric_name = NULL,
           groups = NULL,
           corr_thresh = 1,
           ntree = 1000,
           seed = NA) {
    check_all(
      dataset,
      method,
      find_feature_importance,
      kfold,
      training_frac,
      perf_metric_function,
      perf_metric_name,
      groups,
      corr_thresh,
      ntree,
      seed
    )

    if (!is.na(seed)) {
      if (exists(".Random.seed")) {
        saved_seed <- .Random.seed
        on.exit(.Random.seed <<- saved_seed)
      }
      set.seed(seed)
    }

    # `future.apply` is required for `find_feature_importance()`.
    # check it here to adhere to the fail fast principle.
    if (find_feature_importance) {
      abort_packages_not_installed("future.apply")
    }
    # can't have categorical features for feature importance beause have to find correlations
    outcome_colname <- check_outcome_column(dataset, outcome_colname)
    if (find_feature_importance) {
      check_cat_feats(dataset %>% dplyr::select(-outcome_colname))
    }

    dataset <- randomize_feature_order(dataset, outcome_colname)

    outcomes_vec <- dataset %>% dplyr::pull(outcome_colname)
    training_inds <- get_partition_indices(outcomes_vec,
      training_frac = training_frac,
      groups = groups
    )
    train_data <- dataset[training_inds, ]
    test_data <- dataset[-training_inds, ]
    # train_groups & test_groups will be NULL if groups is NULL
    train_groups <- groups[training_inds]
    test_groups <- groups[-training_inds]

    if (is.null(hyperparameters)) {
      hyperparameters <- get_hyperparams_list(dataset, method)
    }
    tune_grid <- get_tuning_grid(hyperparameters, method)


    outcome_type <- get_outcome_type(outcomes_vec)
    class_probs <- outcome_type != "continuous"

    if (is.null(perf_metric_function)) {
      perf_metric_function <- get_perf_metric_fn(outcome_type)
    }

    if (is.null(perf_metric_name)) {
      perf_metric_name <- get_perf_metric_name(outcome_type)
    }

    cv <- define_cv(train_data,
      outcome_colname,
      hyperparameters,
      perf_metric_function,
      class_probs,
      kfold = kfold,
      cv_times = cv_times,
      groups = train_groups
    )

    model_formula <- stats::as.formula(paste(outcome_colname, "~ ."))

    trained_model_caret <- train_model(
      model_formula,
      train_data,
      method,
      cv,
      perf_metric_name,
      tune_grid,
      ntree
    )
    if (!is.na(seed)) {
      set.seed(seed)
    }

    performance_tbl <- get_performance_tbl(
      trained_model_caret,
      test_data,
      outcome_colname,
      perf_metric_function,
      perf_metric_name,
      class_probs,
      method,
      seed
    )
    feature_importance_tbl <- "Skipped feature importance"
    if (find_feature_importance) {
      feature_importance_tbl <- get_feature_importance(
        trained_model_caret,
        train_data,
        test_data,
        outcome_colname,
        perf_metric_function,
        perf_metric_name,
        class_probs,
        method,
        seed,
        corr_thresh
      )
    }

    return(
      list(
        trained_model = trained_model_caret,
        test_data = test_data,
        performance = performance_tbl,
        feature_importance = feature_importance_tbl
      )
    )
  }
