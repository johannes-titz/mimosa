find_id <- function(d){
  # the idea here is to check for each potential grouping variable if it has
  # at least another variable on level 2
  d2 <- select_if(d, function(x) is.integer(x) | is.character(x) | is.factor(x))
  d2 <- select_if(d2, function(x) length(unique(x)) != length(x))
  # take only variables where every group has at least two values, this avoids
  # taking as a group id an arbitrary variable (e.g. open field) with many
  # possible values
  res <- apply(d2, 2, function(x) prop.table(table(table(x) > 1))["TRUE"] >= .70)
  vars <- names(res)
  res <- lapply(vars, function(x) t(extract_levels2(d2, x, length(vars))))
  res <- plyr::ldply(res, data.frame)
  rownames(res) <- vars
  #test1 <- apply(res, 1, function(x) sum(x == 0))
  test2 <- apply(res, 1, function(x) sum(x == 1, na.rm = T))
  #test1 <- apply(res, 1, function(x) sum(x != 1, na.rm = T))
  #test3 <- test1 > 0 & test2 > 1
  ids <- vars[test2 > 0]
  #position_of_id <- which.min(res)
  #result <- vars[position_of_id]
  ids
}

determine_levels <- function(id_name, data){
  identified_levels <- extract_levels2_without_progress(data, id_name)
  result <- NULL
  level2 <- na.omit(identified_levels == 1)
  # assuming a maximum of 2 levels
  result[[1]] <- names(level2)[!level2]
  #result[[1]] <- select(data, which(!level2))  
  level2[id_name] <- F
  result[[2]] <- names(level2)[level2] #data.frame(data[, which(level2)])
  names(result) <- c("level1", "level2")
  result
}

get_levels <- function(x) {
  length(levels(as.factor(as.character(x))))
}
  
extract_levels2 <- function(d, var, var_total_length){
  incProgress(1 / var_total_length,
              message = paste("Testing Variable ", var, " as grouping variable"))
  # changed it so that simply the average number of levels for the var is
  # calculated
  d <- group_by_(d, var)

  levels <- summarize_all(d, get_levels)
  levels[var] <- NA
  levels <- colMeans(levels)
  levels
  # mean(unlist(levels), na.rm = T)
}

extract_levels2_without_progress <- function(d, var){
  # changed it so that simply the average number of levels for the var is
  # calculated
  d <- group_by_(d, var)

  levels <- summarize_all(d, get_levels)
  levels[var] <- NA
  levels <- colMeans(levels)
  levels
  # mean(unlist(levels), na.rm = T)
}

load_data <- function(datafile){
  fileending <- stringr::str_match(datafile$datapath, "(\\..+$)")[1,1]
  if (fileending == ".sav") {
    data <- Hmisc::spss.get(datafile$datapath, use.value.labels = F)
  }
  
  if (fileending == ".csv") {
    data <- read.csv(datafile$datapath)
  }
  data
}

create_mdl2_formula <- function(beta_nmbr, beta_varies, interaction = NULL){
  beta_varies <- ifelse(beta_varies, paste(" + u<sub>", beta_nmbr, "j",
                                           "</sub>", sep =""), "")
  interaction <- ifelse(!is.null(interaction),
                        paste("&gamma;<sub>",
                              beta_nmbr, 1:length(interaction),
                              "</sub>", interaction, "<sub>j</sub>", 
                              collapse = "+", sep =""),
                        "")
  interaction <- ifelse(interaction == "", "", paste("+", interaction))
  eq_beta <- c("<br>&beta;<sub>", beta_nmbr, "j</sub> = &gamma;<sub>", beta_nmbr, 
                         "0</sub> ", interaction, beta_varies)
  eq_beta
}

create_lvl2_constant <- function(l2){
  part <- paste("&gamma;<sub>0", 1:length(l2), "</sub>", l2, "<sub>j</sub>",
                collapse = "+", sep ="")
  part <- paste("+", part)
  part2 <- paste("&beta;<sub>0j</sub> = &gamma;<sub>00</sub>", ifelse(is.null(l2), "", part))
  paste(part2, "+u<sub>0j</sub>")
}