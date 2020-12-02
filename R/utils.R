#' dplyr pipe
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' rlang data pronoun
#' @importFrom rlang .data
#' @export
rlang::.data

#' caret contr.ltfr
#' @importFrom caret contr.ltfr
#' @export
caret::contr.ltfr


#' @importFrom rlang !!
#' @export
rlang::`!!`

#' @importFrom rlang :=
#' @export
rlang::`:=`

## make R CMD CHECK shut up about the dot `.``
## See: \url{https://github.com/tidyverse/magrittr/issues/29}
utils::globalVariables(c("."))

## Suppress R CMD check note 'All declared Imports should be used'.
## These packages are used by caret to train models and evaluate performance.
## The datasets cannot be loaded if these packages aren't declared in Imports.
## See https://community.rstudio.com/t/how-should-a-meta-package-handle-this-note-all-declared-imports-should-be-used/23400/3
#' @importFrom MLmetrics AUC
#' @importFrom e1071 best.randomForest
#' @importFrom glmnet glmnet
#' @importFrom kernlab as.kernelMatrix
#' @importFrom randomForest getTree
#' @importFrom rpart rpart
#' @importFrom xgboost xgboost
NULL

#' Randomize feature order to eliminate any position-dependent effects
#'
#'
#' @inheritParams run_ml
#'
#' @return Dataset with feature order randomized.
#' @export
#' @author Nick Lesniak, \email{nlesniak@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' dat <- data.frame(
#'   outcome = c("1", "2", "3"),
#'   a = 4:6, b = 7:9, c = 10:12, d = 13:15
#' )
#' randomize_feature_order(dat, "outcome")
randomize_feature_order <- function(dataset, outcome_colname) {
  features_reordered <- dataset %>%
    split_outcome_features(outcome_colname) %>%
    .[["features"]] %>%
    colnames() %>%
    sample()
  dataset <- dplyr::select(
    dataset,
    dplyr::one_of(outcome_colname),
    dplyr::one_of(features_reordered)
  )
  return(dataset)
}

#' Split dataset into outcome and features
#'
#' @inheritParams run_ml
#'
#' @return list of length two: outcome, features (as dataframes)
#' @noRd
#'
#' @examples
#' split_outcome_features(mikropml::otu_mini_bin, "dx")
split_outcome_features <- function(dataset, outcome_colname) {
  # input validation
  check_dataset(dataset)
  check_outcome_column(dataset, outcome_colname, show_message = FALSE)
  # split outcome and features
  outcome <- dataset %>% dplyr::select(outcome_colname)
  features <- dataset %>% dplyr::select(!dplyr::matches(outcome_colname))
  return(list(outcome = outcome, features = features))
}

#' Use future apply if available
#'
#' @param fun apply function to use (apply, lapply, sapply, etc.)
#'
#' @return output of apply function
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' select_apply(fun = "sapply")
select_apply <- function(fun = "apply") {
  pkg <- "base"
  if (all(check_packages_installed("future.apply"))) {
    fun <- paste0("future_", fun)
    pkg <- "future.apply"
  }
  return(utils::getFromNamespace(fun, pkg))
}

#' Mutate all columns with `utils::type.convert()`.`
#'
#' Turns factors into characters and numerics where possible.
#'
#' @param dat data.frame to convert
#'
#' @return data.frame with no factors
#' @noRd
#'
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' dat <- data.frame(
#'   c1 = as.factor(c("a", "b", "c")),
#'   c2 = as.factor(1:3)
#' )
#' class(dat$c1)
#' class(dat$c2)
#' dat <- mutate_all_types(dat)
#' class(dat$c1)
#' class(dat$c2)
mutate_all_types <- function(dat) {
  return(dat %>% dplyr::mutate_all(utils::type.convert, as.is = TRUE))
}
