#' Tries to find the grouping variable (id)
#' 
#' Ranks candidate grouping variables using diagnostics from
#' \code{explain_find_id()}.
#' 
#' @param d data frame
#' @return potential grouping variables
#' @import sjPlot
#' @examples find_id(mlmRev::Exam)
#' @export
find_id <- function(d) {
  diagnostics <- explain_find_id(d)
  group_variables <- as.character(diagnostics$variable[diagnostics$is_candidate])
  # if still empty, there is probably no id
  if (length(group_variables) == 0) {
    showNotification("There seems to be no proper grouping variable (ID). The analysis will likely not work. Please check the structure of your data!",
                     duration = NULL)
    group_variables <- names(d)
  }
  group_variables
}

#' Explain grouping variable detection
#'
#' Returns one row per possible grouping variable with diagnostics used by
#' \code{find_id()}.
#'
#' @param d data frame
#' @return data frame with candidate scores and diagnostics
#' @examples explain_find_id(mlmRev::Exam)
#' @export
explain_find_id <- function(d) {
  # first check data types and variation of variables
  d2 <- select_columns(d, function(x) is_integer(x) | is.character(x) | is.factor(x))
  d2 <- select_columns(d2, function(x) length(unique(x)) != length(x))
  d2 <- select_columns(d2, function(x) length(unique(x)) != 1)
  # take only variables where a certain amount of groups has at least two
  # values, this avoids taking as a group id an arbitrary variable (e.g. open
  # field) with many possible values
  d2 <- select_columns(d2, function(x) isTRUE(prop.table(table(table(x) > 1))["TRUE"] >= .33))
  variables <- names(d2)
  if (length(variables) == 0) {
    return(empty_id_explanation())
  }

  # sort variables by number of reverse levels
  avg_levels_mtrx <- create_avg_levels_mtrx(d2, variables)
  sum_of_reverse_levels <- colSums(avg_levels_mtrx, na.rm = T)
  # this gives the number of variables that are on level 2
  n_variables_lvl2 <- apply(avg_levels_mtrx, 1, function(x) sum(x == 1, na.rm = T))
  diagnostics <- do.call(rbind, lapply(variables, function(variable) {
    score_group_candidate(d2, variable)
  }))
  diagnostics$n_variables_lvl2 <- n_variables_lvl2[diagnostics$variable]
  diagnostics$sum_of_reverse_levels <- sum_of_reverse_levels[diagnostics$variable]
  diagnostics$level2_variable_prop <- diagnostics$n_variables_lvl2 / pmax(ncol(d2) - 1, 1)
  max_lvl2 <- max(diagnostics$n_variables_lvl2, na.rm = TRUE)
  if (!is.finite(max_lvl2) || max_lvl2 == 0) {
    max_lvl2 <- 1
  }
  diagnostics$level2_count_score <- sqrt(diagnostics$n_variables_lvl2 /
                                           max_lvl2)
  max_reverse_levels <- max(diagnostics$sum_of_reverse_levels, na.rm = TRUE)
  if (!is.finite(max_reverse_levels) || max_reverse_levels == 0) {
    max_reverse_levels <- 1
  }
  diagnostics$reverse_level_score <- diagnostics$sum_of_reverse_levels /
    max_reverse_levels
  diagnostics$final_score <- with(diagnostics,
    0.50 * level2_count_score +
      0.20 * reverse_level_score +
      0.10 * repeated_row_prop +
      0.06 * repeated_group_prop +
      0.05 * group_count_score +
      0.04 * median_n_score +
      0.03 * name_score +
      0.02 * (1 - missing_prop)
  )
  best_score <- max(diagnostics$final_score, na.rm = TRUE)
  if (!is.finite(best_score)) {
    best_score <- 0
  }
  has_level2_evidence <- any(diagnostics$n_variables_lvl2 >= 1)
  if (has_level2_evidence) {
    diagnostics$is_candidate <- diagnostics$n_variables_lvl2 >= 1 &
      diagnostics$final_score >= 0.75 * best_score
  } else {
    diagnostics$is_candidate <- seq_len(nrow(diagnostics)) == 1 &
      diagnostics$final_score >= 0.40
  }
  diagnostics <- diagnostics[order(
    -diagnostics$is_candidate,
    -diagnostics$final_score,
    -diagnostics$n_variables_lvl2,
    diagnostics$sum_of_reverse_levels,
    diagnostics$variable
  ), ]
  rownames(diagnostics) <- NULL
  diagnostics
}

#' Determines variables on level 1 and level 2
#' 
#' By specifying the data and the group variable, the function will categorize
#' variables in level 1 and level 2.
#' 
#' @param group_variable name of the group variable (id) 
#' @param data yeah, this is the data
#' @param ignore_na whether to ignor na, default = T
#' @param show_prog whether to show progress in shiny app, default = F
#' @return list with two elements, consisting of the names of the variables on
#' each level
#' @examples determine_levels(find_id(mlmRev::Exam)[1], mlmRev::Exam)
#' @importFrom stats na.omit
#' @export
determine_levels <- function(group_variable, data, ignore_na = T,
                             show_prog = F){
  identified_levels <- find_avg_levels(data, group_variable, ncol(data),
                                       ignore_na, show_prog)
  result <- NULL
  level2 <- stats::na.omit(identified_levels == 1)
  result[[1]] <- names(level2)[!level2]
  level2[group_variable] <- F
  result[[2]] <- names(level2)[level2]
  names(result) <- c("level1", "level2")
  result
}

#' Number of levels
#' 
#' Check how many levels exist for a vector, adapted to the needs of mimosa.
#' 
#' @param x vector
#' @param ignore_na what is says, boolean, default = T
#' @return number of levels
#' @examples 
#' get_levels(c(NA, NA, NA))
#' get_levels(c(1, 1, 2, 3, 3, 5))
#' @noRd
get_levels <- function(x, ignore_na = TRUE) {
  length_levels <- length(levels(as.factor(as.character(x))))
  # if there are only NA in a variable, length_levels will be 0
  # see also example data by karin
  if (ignore_na) {
    length_levels <- ifelse(length_levels == 0, 1, length_levels)
  }
  length_levels
}

#' Calculate average number of levels
#' 
#' Takes the data frame and grouping variable and returns the average number
#' of levels over all groups for every variable
#' 
#' @param data data frame
#' @param group_var the grouping variable
#' @param var_total_length how many variables will be tested (only used for
#'   progress bar)
#' @param ignore_na yes, it is what is says, boolean, default = T
#' @param show_prog whether to show progress bar (useful for shiny and non-shiny
#'   use), default = F
#' @return vector with average levels per group for each variable of d
#' @noRd
find_avg_levels <- function(data, group_var, var_total_length, 
                            ignore_na = TRUE, show_prog = F){
  if (show_prog) {
    incProgress(1 / var_total_length, message = paste("Testing Variable ", group_var, " as grouping variable"))
  }
  group_values <- data[[group_var]]
  groups <- split(seq_len(nrow(data)), group_values, drop = TRUE)
  if (anyNA(group_values)) {
    groups <- c(groups, list(which(is.na(group_values))))
  }
  levels <- vapply(groups, function(rows) {
    vapply(data[rows, , drop = FALSE], get_levels, numeric(1), ignore_na = ignore_na)
  }, numeric(ncol(data)))
  levels <- rowMeans(levels)
  levels[group_var] <- NA
  levels
}

#' Calculate average number of levels in matrix form
#' 
#' Takes the data frame and grouping variable and returns the average number
#' of levels over all groups for every variable in matrix form
#' 
#' @param data data frame
#' @param variables the variables that should be included
#' @return matrix with average levels per group for each potential grouping variable
#' @noRd
create_avg_levels_mtrx <- function(data, variables){
  avg_levels <- lapply(variables, function(x) t(find_avg_levels(data, x, length(variables))))
  avg_levels <- do.call(rbind.data.frame, avg_levels)
  rownames(avg_levels) <- variables
  avg_levels
}

#' Find moderators
#' 
#' @param var_name the variable to check for moderators
#' @param all_moderators all interaction terms
#' @return variables that moderate var_name
#' @noRd
who_moderates_me <- function(var_name, all_moderators){
  if (length(all_moderators) > 0){
    split <- strsplit(all_moderators, ":")
    position <- grep(var_name, split)
    if (length(position) > 0) {
      moderates_me <- sapply(split[position], function(x) x[2])
      return(moderates_me)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

#' Filter independent variables
#' 
#' Options to filter out some independent variables. At the moment only nominal
#' variables with too many levels are filtered out in mimosa.
#' 
#' @param ivs the independent variables (character)
#' @param data the data frame
#' @param n_levels_max how many levels should a variable have at max
#' @noRd
filter_ivs <- function(ivs, data, n_levels_max = 10) {
  data <- data[names(data) %in% ivs]
  n_levels <- sapply(data, function(x) length(levels(x)))
  names(n_levels[(n_levels <= n_levels_max)])
}

#' Filter dependent variables
#'
#' Dependent variables for the Gaussian mixed model must be numeric.
#'
#' @param dvs potential dependent variables
#' @param data the data frame
#' @noRd
filter_dvs <- function(dvs, data) {
  data <- data[names(data) %in% dvs]
  is_numeric <- vapply(data, is.numeric, logical(1))
  names(data)[is_numeric]
}

#' Select columns matching a predicate
#'
#' @noRd
select_columns <- function(data, predicate) {
  keep <- vapply(data, predicate, logical(1))
  data[!is.na(keep) & keep]
}

#' Score one grouping-variable candidate
#'
#' @noRd
score_group_candidate <- function(data, variable) {
  values <- data[[variable]]
  missing_prop <- mean(is.na(values))
  non_missing <- !is.na(values)
  sizes <- table(values[non_missing])
  n_groups <- length(sizes)
  repeated <- sizes >= 2
  repeated_group_prop <- if (n_groups > 0) mean(repeated) else 0
  repeated_row_prop <- if (sum(non_missing) > 0) {
    sum(sizes[repeated]) / sum(non_missing)
  } else {
    0
  }
  median_n <- if (n_groups > 0) stats::median(as.numeric(sizes)) else 0
  max_n <- if (n_groups > 0) max(as.numeric(sizes)) else 0
  group_count_score <- score_group_count(n_groups, nrow(data))
  median_n_score <- score_median_n(median_n)
  name_score <- score_candidate_name(variable)

  data.frame(
    variable = variable,
    n_groups = n_groups,
    repeated_group_prop = repeated_group_prop,
    repeated_row_prop = repeated_row_prop,
    median_n = median_n,
    max_n = max_n,
    missing_prop = missing_prop,
    group_count_score = group_count_score,
    median_n_score = median_n_score,
    name_score = name_score,
    stringsAsFactors = FALSE
  )
}

#' Score whether the number of groups is plausible
#'
#' @noRd
score_group_count <- function(n_groups, n_rows) {
  if (n_rows == 0 || n_groups <= 1 || n_groups >= n_rows) {
    return(0)
  }
  group_ratio <- n_groups / n_rows
  if (group_ratio <= 0.5) {
    return(1)
  }
  max(0, 1 - ((group_ratio - 0.5) / 0.5))
}

#' Score the typical cluster size
#'
#' @noRd
score_median_n <- function(median_n) {
  if (is.na(median_n) || median_n < 2) {
    return(0)
  }
  pmin(1, median_n / 4)
}

#' Small prior for common grouping-variable names
#'
#' @noRd
score_candidate_name <- function(variable) {
  pattern <- paste(
    c("id", "subject", "subj", "person", "participant", "student",
      "pupil", "school", "class", "cluster", "group", "gruppe",
      "team", "site", "dyad", "family", "household", "code", "serial"),
    collapse = "|"
  )
  ifelse(grepl(pattern, variable, ignore.case = TRUE), 1, 0)
}

#' Empty result with the same columns as explain_find_id()
#'
#' @noRd
empty_id_explanation <- function() {
  data.frame(
    variable = character(0),
    n_groups = integer(0),
    repeated_group_prop = numeric(0),
    repeated_row_prop = numeric(0),
    median_n = numeric(0),
    max_n = numeric(0),
    missing_prop = numeric(0),
    group_count_score = numeric(0),
    median_n_score = numeric(0),
    name_score = numeric(0),
    n_variables_lvl2 = numeric(0),
    sum_of_reverse_levels = numeric(0),
    level2_variable_prop = numeric(0),
    level2_count_score = numeric(0),
    reverse_level_score = numeric(0),
    final_score = numeric(0),
    is_candidate = logical(0),
    stringsAsFactors = FALSE
  )
}

#' check if all values of a vector are integers
#' 
#' @noRd
is_integer <- function(x) {
  ifelse(is.numeric(x), all(floor(x) == x, na.rm = TRUE), FALSE)
}
