
#' Check all params that don't return a value
#'
#' @inheritParams run_ml
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
check_all <- function(dataset, method, permute, kfold, training_frac, perf_metric_function, perf_metric_name, group, corr_thresh, ntree, seed) {
  check_method(method)
  check_dataset(dataset)
  check_permute(permute)
  check_kfold(kfold, dataset)
  check_training_frac(training_frac)
  check_perf_metric_function(perf_metric_function)
  check_perf_metric_name(perf_metric_name)
  check_groups(dataset, group, kfold)
  check_corr_thresh(corr_thresh)
  check_ntree(ntree)
  check_seed(seed)
}

#' Check that the dataset is not empty and has more than 1 column.
#'
#' Errors if there are no rows or fewer than 2 columns.
#'
#' @inheritParams run_ml
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_dataset(otu_small)
check_dataset <- function(dataset) {
  if (!any(class(dataset) == "data.frame")) {
    stop(paste("The dataset must be a `data.frame` or `tibble`, but you supplied:", class(dataset)))
  }
  if (nrow(dataset) == 0) {
    stop("No rows detected in dataset.")
  }
  if (ncol(dataset) <= 1) {
    stop(
      "1 or fewer columns detected in dataset. There should be an outcome column and at least one feature column."
    )
  }
}

#' Check if the method is supported. If not, throws error.
#'
#' @inheritParams run_ml
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_method("rf")
check_method <- function(method) {
  methods <- c("glmnet", "svmRadial", "rpart2", "rf", "xgbTree")
  if (!(method %in% methods)) {
    warning(paste0(
      "Method '",
      method,
      "' is not officially supported by mikropml. However, this method might work in our pipeline. You can use the caret documentation to see what hyperparameters are required. Supported methods are:\n    ",
      paste(methods, collapse = ", ")
    ))
  }
}

#' Check that permute is a logical
#'
#' @inheritParams run_ml
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' do_permute <- TRUE
#' check_permute(do_permute)
check_permute <- function(permute) {
  if (!is.logical(permute)) {
    stop(paste0(
      "`permute` must be TRUE or FALSE, but you provided a ",
      class(permute)
    ))
  }
}

#' Check that kfold is an integer of reasonable size
#'
#' @inheritParams run_ml
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_kfold(5, otu_small)
check_kfold <- function(kfold, dataset) {
  not_a_number <- !is.integer(kfold) & !is.numeric(kfold)
  not_an_int <- kfold != as.integer(kfold)
  nfeats <- ncol(dataset) - 1
  out_of_range <- (kfold <= 1) | (kfold > nfeats)
  if (not_a_number | not_an_int | out_of_range) {
    stop(paste0(
      "`kfold` must be an integer between 1 and the number of features in the data.\n",
      "  You provided ", kfold, " folds and your dataset has ", nfeats, " features."
    ))
  }
}

#' Check that the training fraction is between 0 and 1
#'
#' @param frac fraction (numeric)
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_training_frac(0.8)
check_training_frac <- function(frac) {
  if (!is.numeric(frac) | (frac <= 0 | frac >= 1)) {
    stop(paste0(
      "`training_frac` must be a numeric between 0 and 1.\n",
      "    You provided: ", frac
    ))
  }
}

#' check that the seed is either NA or a number
#'
#' @param seed random seed
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_seed(2019)
#' check_seed(NULL)
check_seed <- function(seed) {
  if (!is.na(seed) & !is.numeric(seed)) {
    stop(paste0(
      "`seed` must be `NA` or numeric.\n",
      "    You provided: ", seed
    ))
  }
}

#' Check that outcome column exists. Pick outcome column if not specified.
#'
#' @param check_values whether to check the outcome values or just get the column (default:TRUE)
#' @param show_message whether to show which column is being used as the output column (default: TRUE)
#' @inheritParams run_ml
#'
#' @return outcome colname
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_outcome_column(otu_small, NULL)
#' check_outcome_column(otu_small, "dx")
check_outcome_column <- function(dataset, outcome_colname, check_values = TRUE, show_message = TRUE) {
  # If no outcome colname specified, use first column in data
  if (is.null(outcome_colname)) {
    outcome_colname <- colnames(dataset)[1]
  } else {
    # check to see if outcome is in column names of data
    if (!outcome_colname %in% colnames(dataset)) {
      stop(paste0("Outcome '", outcome_colname, "' not in column names of data."))
    }
  }

  if (check_values) check_outcome_value(dataset, outcome_colname)

  if (show_message) {
    message(
      paste0(
        "Using '",
        outcome_colname,
        "' as the outcome column."
      )
    )
  }

  return(outcome_colname)
}

#' Check that the outcome variable is valid. Pick outcome value if necessary.
#'
#' @inheritParams run_ml
#'
#' @return outcome value
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' check_outcome_value(otu_small, "dx", "cancer")
check_outcome_value <- function(dataset, outcome_colname) {
  # check no NA's
  outcomes_vec <- dataset %>% dplyr::pull(outcome_colname)
  num_missing <- sum(is.na(outcomes_vec))
  if (num_missing != 0) {
    stop(paste0("Missing data in the output variable is not allowed, but the outcome variable has ", num_missing, " missing value(s) (NA)."))
  }

  # check for empty strings
  num_empty <- sum(outcomes_vec == "")
  if (num_empty != 0) {
    warning(paste0("Possible missing data in the output variable: ", num_empty, " empty value(s)."))
  }

  outcomes_all <- dataset %>%
    dplyr::pull(outcome_colname)

  # check if continuous outcome
  isnum <- is.numeric(outcomes_all)
  if (isnum) {
    # check if it might actually be categorical
    if (all(floor(outcomes_all) == outcomes_all)) {
      warning("Data is being considered numeric, but all outcome values are integers. If you meant to code your values as categorical, please use character values.")
    }
  }

  # check binary and multiclass outcome
  outcomes <- outcomes_all %>%
    unique()
  num_outcomes <- length(outcomes)
  if (num_outcomes < 2) {
    stop(
      paste0(
        "A binary or multi-class outcome variable is required, but this dataset has ",
        num_outcomes,  " outcome(s): ", paste(outcomes, collapse = ", ")
      )
    )
  }
}

#' Check whether package(s) are installed
#'
#' @param ... names of packages to check
#' @return named vector with status of each packages; installed (`TRUE`) or not (`FALSE`)
#' @noRd
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' check_packages_installed("base")
#' check_packages_installed("not-a-package-name")
#' all(check_packages_installed("parallel", "doFuture"))
check_packages_installed <- function(...) {
  return(sapply(c(...), requireNamespace, quietly = TRUE))
}


#' Throw error if required packages are not installed.
#'
#' Reports which packages need to be installed and the parent function name.
#' See \url{https://stackoverflow.com/questions/15595478/how-to-get-the-name-of-the-calling-function-inside-the-called-routine}
#' This is only intended to be used inside a function. It will error otherwise.
#'
#' @inheritParams check_packages_installed
#' @noRd
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' abort_packages_not_installed("base")
#' \donttest{
#' abort_packages_not_installed("not-a-package-name", "caret", "dplyr", "non_package")
#' }
abort_packages_not_installed <- function(...) {
  package_status <- check_packages_installed(...)
  parent_fcn_name <- sub("\\(.*$", "\\(\\)", deparse(sys.calls()[[sys.nframe() - 1]]))
  packages_not_installed <- Filter(isFALSE, package_status)
  if (length(packages_not_installed) > 0) {
    msg <- paste0(
      "The following package(s) are required for `", parent_fcn_name,
      "` but are not installed: \n  ",
      paste0(names(packages_not_installed), collapse = ", ")
    )
    stop(msg)
  }
}

#' Check features
#'
#' @param features features for machine learning
#' @param check_missing check whether the features have missing data (default: TRUE)
#'
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' check_features(otu_mini_bin[, 2:11])
check_features <- function(features, check_missing = TRUE) {
  if (!class(features)[1] %in% c("data.frame", "tbl_df")) {
    stop(paste("Argument `features` must be a `data.frame` or `tibble`, but you provided:", class(features)))
  }

  # check for empty strings
  num_empty <- sum(features == "", na.rm = TRUE)
  if (num_empty != 0) {
    warning(paste0("Possible missing data in the features: ", num_empty, " empty value(s)."))
  }

  if (check_missing) {
    # check no NA's
    num_missing <- sum(is.na(features))
    if (num_missing != 0) {
      stop(paste0("Missing data in the features is not allowed, but the features have ", num_missing, " missing value(s) (NA)."))
    }
  }
}

#' Check grouping vector
#'
#' @inheritParams run_ml
#'
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' check_groups(mikropml::otu_mini_bin,
#'   sample(LETTERS, nrow(mikropml::otu_mini_bin), replace = TRUE),
#'   kfold = 2
#' )
check_groups <- function(dataset, groups, kfold) {
  # check that groups is a vector or NULL
  isvec <- is.vector(groups)
  isnull <- is.null(groups)
  if (!(isvec | isnull)) {
    stop(paste0("group should be either a vector or NULL, but group is class ", class(groups), "."))
  }
  # if group is a vector, check that it's the correct length
  if (isvec) {
    ndat <- nrow(dataset)
    ngrp <- length(groups)
    if (ndat != ngrp) {
      stop(paste0("group should be a vector that is the same length as the number of rows in the dataset (", ndat, "), but it is of length ", ngrp, "."))
    }
    # check that there are no NAs in group
    nas <- is.na(groups)
    nnas <- sum(nas)
    if (any(nas)) {
      stop(paste0("No NA values are allowed in group, but ", nnas, " NA(s) are present."))
    }
    # check that there is more than 1 group
    ngrp <- length(unique(groups))
    if (ngrp < 2) {
      stop(paste0("The total number of groups should be greater than 1. If all samples are from the same group, use `group=NULL`"))
    }
    # check that kfold is not greater than the number of groups minus 1 (assuming 1 in the test set)
    if (kfold > (ngrp - 1)) {
      stop(paste0("The number of folds for cross-validation, `k-fold`, must be less than the number of groups. Number of groups: ", ngrp, ". `kfold`: ", kfold, "."))
    }
  }
}

#' check that corr_thresh is either NULL or a number between 0 and 1
#'
#' @param corr_thresh correlation threshold
#'
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' check_corr_thresh(1)
#' check_corr_thresh(0.8)
#' check_corr_thresh(2019)
#' check_corr_thresh(NULL)
check_corr_thresh <- function(corr_thresh) {
  err <- paste0(
    "`corr_thresh` must be `NULL` or numeric between 0 and 1 inclusive.\n",
    "    You provided: ", corr_thresh
  )
  if (!is.null(corr_thresh) & !is.numeric(corr_thresh)) {
    stop(err)
  }
  if (is.numeric(corr_thresh)) {
    if (!(corr_thresh >= 0 & corr_thresh <= 1)) {
      stop(err)
    }
  }
}

#' Check perf_metric_function is NULL or a function
#'
#' @param perf_metric_function performance metric function
#'
#' @return
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' check_perf_metric_function(NULL)
check_perf_metric_function <- function(perf_metric_function) {
  if (!is.function(perf_metric_function) & !is.null(perf_metric_function)) {
    stop(paste0("`perf_metric_function` must be `NULL` or a function.\n    You provided: ", class(perf_metric_function)))
  }
}

#' Check perf_metric_name is NULL or a function
#'
#' @param perf_metric_name performance metric function
#'
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' check_perf_metric_name(NULL)
check_perf_metric_name <- function(perf_metric_name) {
  if (!is.character(perf_metric_name) & !is.null(perf_metric_name)) {
    stop(paste0("`perf_metric_name` must be `NULL` or a character\n    You provided: ", perf_metric_name))
  }
}

#' Check if any features are categorical
#'
#' @param feats features
#'
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' check_cat_feats(otu_mini_bin)
check_cat_feats <- function(feats) {
  if (any(sapply(feats, class) %in% c("factor", "character"))) {
    stop("No categorical features can be used when performing permutation importance. Please change these features to numeric. One option is to use `preprocess_data`.")
  }
}

#' Check remove_var
#'
#' @inheritParams preprocess_data
#'
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' check_remove_var(NULL)
check_remove_var <- function(remove_var) {
  if (!is.null(remove_var)) {
    if (!(remove_var %in% c("nzv", "zv"))) {
      stop(paste0("`remove_var` must be one of: NULL, 'nzv','zv'. You provided: ", remove_var))
    }
  }
}

#' Check ntree
#'
#' @inheritParams run_ml
#'
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' check_ntree(NULL)
check_ntree <- function(ntree) {
  if (!is.null(ntree)) {
    if (!is.numeric(ntree) | length(ntree) > 1) {
      stop(paste0("`ntree` must be of length 1 and class numeric. You provided: ", class(ntree)))
    } else if (ntree < 1) {
      stop(paste0("`ntree` must be greater than zero. You provided: ", ntree))
    }
  }
}
