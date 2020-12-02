
#' Preprocess data prior to running machine learning
#'
#' Function to preprocess your data for input into [run_ml()].
#'
#' @param method Methods to preprocess the data, described in
#'   [caret::preProcess()] (default: `c("center","scale")`, use `NULL` for
#'   no normalization).
#' @param remove_var Whether to remove variables with near-zero variance
#'   (`'nzv'`; default), zero variance (`'zv'`), or none (`NULL`).
#' @param collapse_corr_feats Whether to keep only one of perfectly correlated features.
#' @param to_numeric Whether to change features to numeric where possible.
#' @inheritParams run_ml
#' @inheritParams get_corr_feats
#'
#' @return
#'
#' Named list including:
#' - `dat_transformed`: Preprocessed data.
#' - `grp_feats`: If features were grouped together, a named list of the features corresponding to each group.
#' - `removed_feats`: Any features that were removed during preprocessing (e.g. because there was zero variance or near-zero variance for those features).
#'
#' @section More details:
#'
#' See the [preprocessing vignette](http://www.schlosslab.org/mikropml/articles/preprocess.html)
#' for more details.
#'
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' preprocess_data(mikropml::otu_small, "dx")
preprocess_data <- function(dataset, outcome_colname, method = c("center", "scale"), remove_var = "nzv", collapse_corr_feats = TRUE, to_numeric = TRUE, group_neg_corr = TRUE) {
  check_dataset(dataset)
  check_outcome_column(dataset, outcome_colname, check_values = FALSE)
  check_remove_var(remove_var)

  dataset <- rm_missing_outcome(dataset, outcome_colname)

  split_dat <- split_outcome_features(dataset, outcome_colname)

  features <- split_dat$features
  if (to_numeric) {
    features <- change_to_num(features)
  }

  nv_feats <- process_novar_feats(features)
  split_feats <- process_cat_feats(nv_feats$var_feats)
  cont_feats <- process_cont_feats(split_feats$cont_feats, method)

  # combine all processed features
  processed_feats <- dplyr::bind_cols(
    cont_feats$transformed_cont,
    split_feats$cat_feats,
    nv_feats$novar_feats
  )
  # remove features with (near-)zero variance
  feats <- get_caret_processed_df(processed_feats, remove_var)
  processed_feats <- feats$processed
  removed_feats <- c(cont_feats$removed_cont, feats$removed)

  # remove perfectly correlated features
  grp_feats <- NULL
  if (collapse_corr_feats) {
    if (is.null(remove_var)) {
      message("Removing features with zero variance prior to collapsing correlated features.")
      feats <- get_caret_processed_df(processed_feats, "zv")
      processed_feats <- feats$processed
      removed_feats <- c(removed_feats, feats$removed)
    }
    feats_and_grps <- collapse_correlated_features(processed_feats, group_neg_corr)
    processed_feats <- feats_and_grps$features
    grp_feats <- feats_and_grps$grp_feats
  }

  # combine outcome and features
  dat_transformed <- dplyr::bind_cols(split_dat$outcome, processed_feats) %>%
    dplyr::as_tibble()

  return(list(
    dat_transformed = dat_transformed,
    grp_feats = grp_feats,
    removed_feats = removed_feats
  ))
}

#' Remove missing outcome values
#'
#' @inheritParams run_ml
#'
#' @return dataset with no missing outcomes
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' rm_missing_outcome(mikropml::otu_mini_bin, "dx")
#'
#' test_df <- mikropml::otu_mini_bin
#' test_df[1:100, "dx"] <- NA
#' rm_missing_outcome(test_df, "dx")
rm_missing_outcome <- function(dataset, outcome_colname) {
  n_outcome_na <- sum(is.na(dataset %>% dplyr::pull(outcome_colname)))
  total_outcomes <- nrow(dataset)
  perc_na <- round(n_outcome_na / total_outcomes * 100, 2)
  dataset <- dataset %>% dplyr::filter(!is.na(!!(dplyr::sym(outcome_colname))))
  if (n_outcome_na != 0) {
    message(paste0("Removed ", n_outcome_na, "/", total_outcomes, " (", perc_na, "%) of samples because of missing outcome value (NA)."))
  }
  return(dataset)
}


#' Change columns to numeric if possible
#'
#' @param features dataframe of features for machine learning
#'
#' @return dataframe with numeric columns where possible
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' class(change_to_num(data.frame(val = c("1", "2", "3")))[[1]])
change_to_num <- function(features) {
  lapply_fn <- select_apply(fun = "lapply")
  check_features(features, check_missing = FALSE)
  features[] <- lapply_fn(features, function(col) {
    if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
      as.numeric(as.character(col))
    } else {
      col
    }
  })
  return(features)
}

#' Process features with no variation
#'
#' @param features dataframe of features for machine learning
#'
#' @return list of two dataframes: features with variability (unprocessed) and without (processed)
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' process_novar_feats(mikropml::otu_small[, 2:ncol(otu_small)])
process_novar_feats <- function(features) {
  novar_feats <- NULL
  var_feats <- NULL
  if (!is.null(features)) {
    check_features(features, check_missing = FALSE)

    # get features with no variation
    apply_fn <- select_apply(fun = "apply")
    novar_feats_bool <-
      apply_fn(features, 2, function(x) {
        length(unique(x[!is.na(x)])) == 1
      })
    novar_feats <- features %>% dplyr::select_if(novar_feats_bool)

    # change categorical features with no variation to zero
    sapply_fn <- select_apply(fun = "sapply")
    novar_feats <- sapply_fn(novar_feats, function(x) {
      if (class(x) %in% c("factor", "character")) {
        rep(0, length(x))
      } else {
        x
      }
    }) %>% dplyr::as_tibble()

    if (ncol(novar_feats) == 0) {
      novar_feats <- NULL
    }

    # get features with variation
    var_feats <- features %>%
      dplyr::select_if(!novar_feats_bool) %>%
      dplyr::as_tibble()

    if (ncol(var_feats) == 0) {
      stop("All features have zero variance.")
    }

    # make missing data identical to others for novar_feats (not sure this is the best way to go)
    n_missing <- sum(is.na(novar_feats))
    lapply_fn <- select_apply("lapply")
    novar_feats[] <- lapply_fn(novar_feats, function(x) {
      rep(unique(x[!is.na(x)]), nrow(novar_feats))
    })
    if (n_missing > 0) {
      message(
        paste0(
          "There are ",
          n_missing,
          " missing value(s) in features with no variation. Missing values were replaced with the non-varying value."
        )
      )
    }
  }
  return(list(novar_feats = novar_feats, var_feats = var_feats))
}

#' Process categorical features
#'
#' @param features dataframe of features for machine learning
#'
#' @return list of two dataframes: categorical (processed) and continuous features (unprocessed)
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' process_cat_feats(mikropml::otu_small[, 2:ncol(otu_small)])
process_cat_feats <- function(features) {
  feature_design_cat_mat <- NULL
  cont_feats <- NULL
  if (!is.null(features)) {
    check_features(features, check_missing = FALSE)

    sapply_fn <- select_apply("sapply")
    cat_feats_bool <- sapply_fn(features, function(x) {
      xu <- unique(x[!is.na(x)])
      cl <- class(x)
      bool <- (cl %in% c("character", "factor") | length(xu) == 2) &
        !((is.numeric(x) | is.integer(x)) & !all(c(0, 1) %in% xu))
    })
    cat_feats <- features %>%
      dplyr::select_if(cat_feats_bool) %>%
      dplyr::mutate_all(as.character)
    lapply_fn <- select_apply(fun = "lapply")
    cat_feats[] <- lapply_fn(cat_feats, function(x) {
      x[!is.na(x)] <- paste0("_", x[!is.na(x)])
      x
    })
    cont_feats <- features %>%
      dplyr::select_if(!cat_feats_bool) %>%
      dplyr::as_tibble()
    if (ncol(cont_feats) == 0) cont_feats <- NULL


    feature_design_cat_mat <- NULL
    if (ncol(cat_feats) != 0) {
      no_missing_bin <- sapply_fn(cat_feats, function(x) !any(is.na(x)) & length(unique(x[!is.na(x)])) == 2)
      no_missing_bin_mat <- cat_feats[, no_missing_bin] %>% dplyr::as_tibble()
      missing_nonbin_mat <- cat_feats[, !no_missing_bin] %>% dplyr::as_tibble()

      # full rank for binary features with no missing data (i.e. one column for each binary feature with no missing data)
      feature_design_no_missing_bin <- NULL
      if (ncol(no_missing_bin_mat) > 0) {
        feature_design_no_missing_bin <- get_caret_dummyvars_df(no_missing_bin_mat, full_rank = TRUE)
      }
      # change categorical binary variables to 0 and 1 (not full rank, i.e. one column for each unique element in the column)
      feature_design_missing_nonbin <- NULL
      if (ncol(missing_nonbin_mat) > 0) {
        feature_design_missing_nonbin <- get_caret_dummyvars_df(missing_nonbin_mat, full_rank = FALSE)
      }

      # combine binary no missing and other categorical features
      feature_design_cat_mat <- dplyr::bind_cols(feature_design_no_missing_bin, feature_design_missing_nonbin) %>% dplyr::as_tibble()

      missing <- is.na(feature_design_cat_mat)
      n_missing <- sum(missing)
      feature_design_cat_mat[missing] <- 0
      if (n_missing > 0) {
        message(paste0(n_missing, " categorical missing value(s) (NA) were replaced with 0. Note that the matrix is not full rank so missing values may be duplicated in separate columns."))
      }
    }
  }
  return(list(cat_feats = feature_design_cat_mat, cont_feats = cont_feats))
}

#' Preprocess continuous features
#'
#' @inheritParams get_caret_processed_df
#'
#' @return dataframe of preprocessed features
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' process_cont_feats(mikropml::otu_small[, 2:ncol(otu_small)], c("center", "scale"))
process_cont_feats <- function(features, method) {
  transformed_cont <- NULL
  removed_cont <- NULL

  if (!is.null(features)) {
    check_features(features, check_missing = FALSE)

    if (ncol(features) != 0) {
      # transform continuous features
      transformed_cont <- features
      if (ncol(features) > 0 & !is.null(method)) {
        feats <- get_caret_processed_df(features, method)
        transformed_cont <- feats$processed
        removed_cont <- feats$removed
      }
      sapply_fn <- select_apply("sapply")
      cl <- sapply_fn(transformed_cont, function(x) {
        class(x)
      })
      missing <-
        is.na(transformed_cont[, cl %in% c("integer", "numeric")])
      n_missing <- sum(missing)
      if (n_missing > 0) {
        # impute missing data using the median value
        transformed_cont <- sapply_fn(transformed_cont, function(x) {
          if (class(x) %in% c("integer", "numeric")) {
            m <- is.na(x)
            x[m] <- stats::median(x, na.rm = TRUE)
          }
          return(x)
        }) %>% dplyr::as_tibble()
        message(
          paste0(
            n_missing,
            " missing continuous value(s) were imputed using the median value of the feature."
          )
        )
      }
    }
  }
  return(list(transformed_cont = transformed_cont, removed_cont = removed_cont))
}

#' Get preprocessed dataframe for continuous variables
#'
#' @param features Dataframe of features for machine learning
#' @inheritParams preprocess_data
#'
#' @return
#'
#' Named list:
#' - `processed`: Dataframe of processed features.
#' - `removed`: Names of any features removed during preprocessing.
#'
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' get_caret_processed_df(mikropml::otu_small[, 2:ncol(otu_small)], c("center", "scale"))
get_caret_processed_df <- function(features, method) {
  processed <- features
  removed <- NULL
  if (!is.null(method) & !is.null(features)) {
    check_features(features, check_missing = FALSE)
    preproc_values <- caret::preProcess(features, method = method)
    processed <- stats::predict(preproc_values, features)
    removed <- names(features)[!names(features) %in% names(processed)]
  }
  return(list(processed = processed, removed = removed))
}

#' Get dummyvars dataframe (i.e. design matrix)
#'
#' @param features dataframe of features for machine learning
#' @param full_rank whether matrix should be full rank or not (see `[caret::dummyVars])
#'
#' @return design matrix
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' df <- data.frame(
#'   outcome = c("normal", "normal", "cancer"),
#'   var1 = 1:3,
#'   var2 = c("a", "b", "c"),
#'   var3 = c("no", "yes", "no"),
#'   var4 = c(0, 1, 0)
#' )
#' get_caret_dummyvars_df(df, TRUE)
get_caret_dummyvars_df <- function(features, full_rank = FALSE) {
  check_features(features, check_missing = FALSE)
  if (!is.null(process_novar_feats(features)$novar_feats)) {
    stop("Some variables have no variation. Please remove prior to running this function.")
  }
  feature_design <- caret::dummyVars(" ~ .", data = features, fullRank = full_rank)
  feature_design_mat <- stats::predict(feature_design, features) %>%
    dplyr::as_tibble()
  return(feature_design_mat)
}


#' Collapse correlated features
#'
#' @inheritParams get_corr_feats
#'
#' @return features where perfectly correlated ones are collapsed
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' collapse_correlated_features(mikropml::otu_small[, 2:ncol(otu_small)])
collapse_correlated_features <- function(features, group_neg_corr = TRUE) {
  feats_nocorr <- features
  grp_feats <- NULL
  if (!is.null(features)) {
    sapply_fn <- select_apply(fun = "sapply")
    if (any(sapply_fn(features, class) %in% c("character", "factor"))) {
      stop(
        "Some features are charactors or factors. Please remove these before proceeding with `collapse_correlated_features`."
      )
    }
    if (!is.null(process_novar_feats(features)$novar_feats)) {
      stop(
        "Some features have no variation. Please remove these before proceeding with `collapse_correlated_features`."
      )
    }
    if (ncol(features) != 1) {
      corr_feats <-
        get_corr_feats(features, group_neg_corr = group_neg_corr) %>%
        group_correlated_features(., features)
      corr_cols <- gsub("\\|.*", "", corr_feats)
      feats_nocorr <-
        features %>% dplyr::select(dplyr::all_of(corr_cols))
      names_grps <- sapply_fn(names(feats_nocorr), function(n) {
        not_corr <- n %in% corr_feats
        if (not_corr) {
          name <- n
        } else {
          name <-
            corr_feats[grep(
              paste0("^", n, "\\||\\|", n, "\\||\\|", n, "$"),
              corr_feats
            )]
        }
      })
      grp_cols <- grep("\\|", names_grps)
      num_grps <- length(grp_cols)
      if (num_grps == 0) {
        grp_feats <- NULL
      } else {
        names(names_grps)[grp_cols] <- paste0("grp", 1:num_grps)
        names(feats_nocorr) <- names(names_grps)
        grp_feats <-
          sapply_fn(names_grps, function(x) {
            strsplit(x, split = "\\|")
          })
      }
    }
  }
  return(list(features = feats_nocorr, grp_feats = grp_feats))
}
