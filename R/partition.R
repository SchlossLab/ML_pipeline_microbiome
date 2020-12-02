#' Select indices to partition the data into training & testing sets.
#'
#' Use this function to get the row indices for the training set.
#'
#' If `groups` is `NULL`, uses \link[caret]{createDataPartition}.
#' Otherwise, uses `create_grouped_data_partition()`.
#'
#' Set the seed prior to calling this function if you would like your data
#' partitions to be reproducible (recommended).
#'
#' @param outcomes vector of outcomes
#' @param training_frac max fraction of data for the training set(default: 0.8)
#' @param groups vector of groups. length must match the number of rows in the dataset. (default: NULL)
#'
#' @return Vector of row indices for the training set.
#'
#' @examples
#' training_inds <- get_partition_indices(otu_mini_bin$dx)
#' train_data <- otu_mini_bin[training_inds, ]
#' test_data <- otu_mini_bin[-training_inds, ]
#' @export
#' @author Kelly Sovacool, {sovacool@@umich.edu}
get_partition_indices <- function(outcomes, training_frac = 0.8, groups = NULL) {
  check_training_frac(training_frac)
  if (is.null(groups)) {
    training_inds <- caret::createDataPartition(outcomes,
      p = training_frac,
      list = FALSE
    ) %>% .[, 1]
  } else {
    training_inds <- create_grouped_data_partition(groups, p = training_frac)
  }
  return(training_inds)
}


# split by groups (e.g. facility)
# try to get ~split defined by user (training_frac)

#' Split into train and test set while splitting by groups
#'
#' @param groups vector of groups. length must match the number of rows in the dataset.
#' @param p maximum fraction of data that goes to training (maybe less depending on groups sizes)
#'
#' @return vector of row indices for the training set
#' @noRd
#' @author Zena Lapp, {zenalapp@@umich.edu}
#'
#' @examples
#' groups <- c("A", "B", "A", "B", "C", "C", "A", "A", "D")
#' set.seed(0)
#' train_ind <- create_grouped_data_partition(groups, 0.8)
create_grouped_data_partition <- function(groups, p) {
  # get indices
  indices <- seq(along = groups)
  # get unique groups
  grps <- unlist(unique(groups))
  # initialize train groups & set
  train_grps <- grps
  train_set <- indices
  train_set_grp <- groups
  # initialize fraction of data in train set
  frac_in_train <- length(train_set) / length(indices)
  # keep removing data from train set until fraction in train set <= p (e.g. 0.8)
  while (frac_in_train > p) {
    # randomly choose a groups
    grp <- sample(train_grps, size = 1)
    # remove groups from train groups & set
    train_grps <- train_grps[train_grps != grp]
    train_set <- train_set[train_set_grp != grp]
    train_set_grp <- train_set_grp[train_set_grp != grp]
    # calcuate fraction of data in train set
    frac_in_train <- length(train_set) / length(indices)
  }
  message(paste0("Fraction of data in the training set: ", frac_in_train, "."))
  # get train groups indices
  return(train_set)
}
