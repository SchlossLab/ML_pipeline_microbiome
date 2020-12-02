#' Get outcome type.
#'
#' If the outcome is numeric, the type is continuous.
#' Otherwise, the outcome type is binary if there are only two outcomes or
#' multiclass if there are more than two outcomes.
#'
#' @param outcomes_vec Vector of outcomes.
#'
#' @return Outcome type (continuous, binary, or multiclass).
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' get_outcome_type(c(1, 2, 1))
#' get_outcome_type(c("a", "b", "b"))
#' get_outcome_type(c("a", "b", "c"))
get_outcome_type <- function(outcomes_vec) {
  if (is.numeric(outcomes_vec)) {
    # regression
    otype <- "continuous"
  } else {
    num_outcomes <- length(unique(outcomes_vec))
    if (num_outcomes < 2) {
      stop(
        paste0(
          "A continuous, binary, or multi-class outcome variable is required, but this dataset has ",
          num_outcomes,
          " outcome(s)."
        )
      )
    } else if (num_outcomes == 2) {
      # binary classification
      otype <- "binary"
    } else {
      # multi-class classification
      otype <- "multiclass"
    }
  }
  return(otype)
}


#' Get default performance metric function
#'
#' @param outcome_type Type of outcome (one of: `"continuous"`,`"binary"`,`"multiclass"`).
#'
#' @return Performance metric function.
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' get_perf_metric_fn("continuous")
#' get_perf_metric_fn("binary")
#' get_perf_metric_fn("multiclass")
get_perf_metric_fn <- function(outcome_type) {
  if (outcome_type == "continuous") { # regression
    perf_metric_fn <- caret::defaultSummary
  } else if (outcome_type %in% c("binary", "multiclass")) { # multi-class classification
    perf_metric_fn <- caret::multiClassSummary
  } else {
    stop(paste0('Outcome type of outcome must be one of: `"continuous"`,`"binary"`,`"multiclass`), but you provided: ', outcome_type))
  }
  return(perf_metric_fn)
}


#' Get default performance metric name
#'
#' Get default performance metric name for cross-validation.
#'
#' @param outcome_type Type of outcome (one of: `"continuous"`,`"binary"`,`"multiclass"`).
#'
#' @return Performance metric name.
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' get_perf_metric_name("continuous")
#' get_perf_metric_name("binary")
#' get_perf_metric_name("multiclass")
get_perf_metric_name <- function(outcome_type) {
  if (outcome_type == "continuous") { # regression
    perf_metric_name <- "RMSE"
  } else {
    if (outcome_type == "binary") { # binary classification
      perf_metric_name <- "AUC"
    } else if (outcome_type == "multiclass") { # multi-class classification
      perf_metric_name <- "logLoss"
    } else {
      stop(paste0('Outcome type of outcome must be one of: `"continuous"`,`"binary"`,`"multiclass`), but you provided: ', outcome_type))
    }
  }
  return(perf_metric_name)
}

#' Get performance metrics for test data
#'
#' @param test_data Held out test data: dataframe of outcome and features.
#' @param trained_model Trained model from [caret::train()].
#' @param class_probs Whether to use class probabilities (TRUE for categorical outcomes, FALSE for numeric outcomes).
#' @inheritParams run_ml
#'
#' @return
#'
#' Dataframe of performance metrics.
#'
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' \donttest{
#' results <- run_ml(otu_small, "glmnet", kfold = 2, cv_times = 2)
#' calc_perf_metrics(results$test_data,
#'   results$trained_model,
#'   "dx",
#'   multiClassSummary,
#'   class_probs = TRUE
#' )
#' }
calc_perf_metrics <- function(test_data, trained_model, outcome_colname, perf_metric_function, class_probs) {
  pred_type <- "raw"
  if (class_probs) pred_type <- "prob"
  preds <- stats::predict(trained_model, test_data, type = pred_type)
  if (class_probs) {
    uniq_obs <- unique(c(test_data %>% dplyr::pull(outcome_colname), as.character(trained_model$pred$obs)))
    obs <- factor(test_data %>% dplyr::pull(outcome_colname), levels = uniq_obs)
    pred_class <- factor(names(preds)[apply(preds, 1, which.max)], levels = uniq_obs)
    perf_met <- perf_metric_function(data.frame(obs = obs, pred = pred_class, preds), lev = uniq_obs)
  } else {
    obs <- test_data %>% dplyr::pull(outcome_colname)
    perf_met <- perf_metric_function(data.frame(obs = obs, pred = preds))
  }
  return(perf_met)
}


#' Get model performance metrics as a one-row tibble
#'
#' @inheritParams calc_perf_metrics
#' @inheritParams run_ml
#' @inheritParams get_feature_importance
#'
#'
#' @return A one-row tibble with columns `cv_auroc`, column for each of the performance metrics for the test data `method`, and `seed`.
#' @export
#'
#' @examples
#'
#' \donttest{
#' results <- run_ml(otu_small, "glmnet", kfold = 2, cv_times = 2)
#' names(results$trained_model$trainingData)[1] <- "dx"
#' get_performance_tbl(results$trained_model, results$test_data,
#'   "dx",
#'   multiClassSummary, "AUC",
#'   class_probs = TRUE,
#'   method = "glmnet"
#' )
#' }
#'
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
get_performance_tbl <- function(trained_model,
                                test_data,
                                outcome_colname,
                                perf_metric_function,
                                perf_metric_name,
                                class_probs,
                                method,
                                seed = NA) {
  test_perf_metrics <- calc_perf_metrics(
    test_data,
    trained_model,
    outcome_colname,
    perf_metric_function,
    class_probs
  )

  train_perf <- caret::getTrainPerf(trained_model)
  cv_metric_name <- paste0("Train", perf_metric_name)
  cv_metric_options <- names(train_perf)

  if (!(cv_metric_name %in% cv_metric_options)) {
    warning(
      "The performance metric provided does not match the metric used to train the data.\n",
      "You provided: `", perf_metric_name, "`\n",
      "The options are: \n    ",
      paste(gsub("Train|method", "", cv_metric_options),
        collapse = ", "
      )
    )
    cv_metric_value <- NA
  } else {
    cv_metric_value <- train_perf[[cv_metric_name]]
  }
  return(dplyr::bind_rows(c(
    cv_metric = cv_metric_value,
    test_perf_metrics,
    method = method,
    seed = seed
  )) %>%
    dplyr::rename_with(
      function(x) paste0("cv_metric_", perf_metric_name),
      .data$cv_metric
    ) %>%
    change_to_num())
}
