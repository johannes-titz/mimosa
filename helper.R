find_id <- function(d){
  # the idea here is to check for each potential grouping variable if it has
  # at least another variable on level 2
  d2 <- select_if(d, function(x) is.integer(x) | is.character(x) | is.factor(x))
  d2 <- select_if(d2, function(x) length(unique(x)) != length(x))
  d2 <- select_if(d2, function(x) length(unique(x)) != 1)
  # take only variables where every group has at least two values, this avoids
  # taking as a group id an arbitrary variable (e.g. open field) with many
  # possible values
  d2 <- select_if(d2, function(x) prop.table(table(table(x) > 1))["TRUE"] >= .70)
  vars <- names(d2)
  res <- lapply(vars, function(x) t(extract_levels2(d2, x, length(vars))))
  #res <- lapply(vars, function(x) t(extract_levels2(d2, x, length(vars))))
  res <- plyr::ldply(res, data.frame)
  rownames(res) <- vars
  #test1 <- apply(res, 1, function(x) sum(x == 0))
  # this gives the number of variables that are on level 2
  test2 <- apply(res, 1, function(x) sum(x == 1, na.rm = T))
  #test1 <- apply(res, 1, function(x) sum(x != 1, na.rm = T))
  #test3 <- test1 > 0 & test2 > 1
  test2 <- test2[test2 > 0]
  ids <- names(sort(test2, decreasing = T))#vars[test2 > 0]
  if (length(ids) == 0){
    ids <- vars
  }
  
  #position_of_id <- which.min(res)
  #result <- vars[position_of_id]
  ids
}

perc_more_than_one_val_per_group <- function(x){
  prop.table(table(table(x) > 1))["TRUE"]
}

determine_levels <- function(id_name, data, show_prog = F){
  identified_levels <- extract_levels2(data, id_name, ncol(data), show_prog)
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
  
extract_levels2 <- function(d, var, var_total_length, show_prog = F){
  if(show_prog) {
    incProgress(1 / var_total_length, message = paste("Testing Variable ", var, " as grouping variable"))
  }
  
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

create_one_slope <- function(var_name, position){
  paste(" + &beta;<sub>", position, "j</sub>", var_name[position], "<sub>ij</sub>",
        sep = "")
}

create_equation <- function(dv, l1 = NULL){
  slopes <- paste(mapply(create_one_slope, l1, seq(l1)), collapse = "")
  constant <- " &beta;<sub>0j</sub>"
  error <- " + e<sub>ij</sub>"
  left_side <- paste(dv, "<sub>ij</sub> =", sep ="")
  right_side <- paste(constant, slopes, error, sep ="")
  equation <- paste(left_side, right_side, sep = "")
  equation
}

who_moderates_me <- function(var_name, all_moderators){
  if (length(all_moderators) > 0){
    split <- strsplit(all_moderators, ":")
    position <- grep(var_name, split)
    if (length(position) > 0) {
      moderates_me <- sapply(split[position], function(x) x[2])
    }
    return(moderates_me)
  } else {
    return(NULL)
  }
}