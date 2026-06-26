#' Create table output
#' 
#' This simply uses sjPlot with some adapted defaults.
#' 
#' @importFrom sjPlot tab_model
#' @param mdl the lme4 model object
#' @param l1 level 1 variables
#' @param output_options what to show in the output
#' @noRd
create_table <- function(mdl, l1, output_options) {
  check <- c("standard error", "AIC", "Deviance", "Log-Likelihood",
             "standardized coefficients", "test statistic", "p-value") 
  
  show <- check %in% output_options
  names(show) <- check
  
  if (length(l1) > 0 & show["standardized coefficients"]) {
    show_beta <- T
  } else {
    show_beta <- NULL
  }
  table <- sjPlot::tab_model(
    mdl,
    show.se = show["standard error"], 
    show.p = show["p-value"],
    show.stat = show["test statistic"],
    show.aic = show["AIC"],
    show.dev = show["Deviance"],
    show.loglik = show["Log-Likelihood"],
    show.std = show_beta,
    string.se = "SE",
    string.std = "&beta;",
    string.ci = "95% CI",
    string.stat = "<i>t</i>",
    collapse.ci = F, show.icc = TRUE, show.re.var = TRUE,
    show.ngroups = TRUE, show.fstat = FALSE, show.aicc = F
  )[[3]]
  table <- move_random_components_to_predictor_table(table, mdl)
  table <- add_fixed_effect_variance(table, calculate_fixed_effect_variance(mdl))
  add_summary_tooltips(table, mdl)
}

#' Empirical variance of fixed effects
#'
#' Computes var(X beta) from the fitted model matrix and fixed coefficients.
#'
#' @param mdl fitted lme4 model
#' @noRd
calculate_fixed_effect_variance <- function(mdl) {
  fixed_predictor <- as.vector(stats::model.matrix(mdl) %*% lme4::fixef(mdl))
  stats::var(fixed_predictor)
}

#' Add fixed-effect variance row to sjPlot table HTML
#'
#' @param table HTML table created by sjPlot
#' @param fixed_variance fixed-effect variance
#' @noRd
add_fixed_effect_variance <- function(table, fixed_variance) {
  row <- create_variance_row(
    sigma_squared_label("FE"),
    fixed_variance,
    "Empirical variance of the fixed-effects-only predictions."
  )
  if (!nzchar(row)) {
    return(table)
  }
  marker <- paste0(
    "  <tr>\n",
    "    <td class=\"tdata leftalign summary\">Marginal R<sup>2</sup> / Conditional R<sup>2</sup></td>"
  )
  if (grepl(marker, table, fixed = TRUE)) {
    return(sub(marker, paste0(row, marker), table, fixed = TRUE))
  }
  sub("</table>", paste0(row, "\n</table>"), table, fixed = TRUE)
}

#' Create a variance summary row
#'
#' @param label row label
#' @param variance numeric variance
#' @param tooltip title text shown by browser tooltip
#' @noRd
create_variance_row <- function(label, variance, tooltip) {
  if (!is.finite(variance)) {
    return("")
  }
  value <- tooltip_span(format_table_number(variance), tooltip)
  row <- paste0(
    "\n  <tr>\n",
    "    <td class=\"tdata leftalign summary\">",
    label,
    "</td>\n",
    "    <td class=\"tdata summary summarydata\" colspan=\"100\">",
    value,
    "</td>\n",
    "  </tr>\n"
  )
  row
}

#' Format a table number like sjPlot summary rows
#'
#' @param x numeric value
#' @noRd
format_table_number <- function(x) {
  format(round(x, 2), nsmall = 2, trim = TRUE)
}

#' Add tooltips to sjPlot summary rows
#'
#' @param table HTML table created by sjPlot
#' @param mdl fitted lme4 model
#' @noRd
add_summary_tooltips <- function(table, mdl) {
  sigma_tooltip <- paste0(
    sigma_squared_label(), "<br>",
    "Estimated residual variance: remaining level-1/error variance after fixed and random effects."
  )
  tau_tooltip <- paste0(
    "&tau;<br>",
    "Estimated random-effect variance component for the matching intercept or slope."
  )
  rho_tooltip <- paste0(
    "&rho;<br>",
    "Estimated correlation between random effects. ",
    "&rho;<sub>01</sub> is the correlation between random effect 0 and random effect 1, ",
    "usually the random intercept and first random slope for the grouping factor."
  )
  icc_tooltip <- create_icc_tooltip(mdl)

  table <- sub_summary_label(
    table,
    "&sigma;<sup>2</sup>",
    sigma_squared_label(),
    sigma_tooltip
  )
  table <- add_title_to_summary_rows(table, "&tau;", tau_tooltip)
  table <- add_title_to_summary_rows(table, "&rho;", rho_tooltip)
  table <- sub_summary_label(table, "ICC", "ICC", icc_tooltip)
  table <- add_r2_tooltips(table, mdl)
  table
}

#' Move random-effect variance/correlation components into the predictor table
#'
#' @param table HTML table created by sjPlot
#' @param mdl fitted lme4 model
#' @noRd
move_random_components_to_predictor_table <- function(table, mdl) {
  rows <- strsplit(table, "\n", fixed = TRUE)[[1]]
  tau_rows <- extract_tau_rows(rows)
  rho_values <- extract_intercept_rhos(mdl)
  if (length(tau_rows$values) == 0 && length(rho_values) == 0) {
    return(rename_random_effects_section(table))
  }
  old_colspan <- get_model_colspan(table)
  if (is.na(old_colspan)) {
    return(rename_random_effects_section(table))
  }
  rows <- remove_tau_rows(rows, tau_rows$remove)
  rows <- remove_rho_rows(rows)
  new_columns <- 0
  if (length(tau_rows$values) > 0) {
    new_columns <- new_columns + 1
    rows <- add_random_component_header(rows, old_colspan + 1 + new_columns, "&tau;")
    rows <- add_random_component_cells(
      rows,
      tau_rows$values,
      old_colspan + 1 + new_columns,
      format_tau_tooltip
    )
  }
  if (length(rho_values) > 0) {
    new_columns <- new_columns + 1
    rows <- add_random_component_header(rows, old_colspan + 1 + new_columns, "&rho;")
    rows <- add_random_component_cells(
      rows,
      rho_values,
      old_colspan + 1 + new_columns,
      format_intercept_rho_tooltip
    )
  }
  table <- paste(rows, collapse = "\n")
  table <- gsub(
    paste0("colspan=\"", old_colspan, "\""),
    paste0("colspan=\"", old_colspan + new_columns, "\""),
    table,
    fixed = TRUE
  )
  table <- rename_random_effects_section(table)
  set_randomparts_colspan(table, old_colspan + 1 + new_columns)
}

#' Backwards-compatible wrapper
#'
#' @param table HTML table created by sjPlot
#' @noRd
move_tau_to_predictor_table <- function(table) {
  move_random_components_to_predictor_table(table, NULL)
}

#' Extract tau values from sjPlot summary rows
#'
#' @param rows HTML table split into lines
#' @noRd
extract_tau_rows <- function(rows) {
  values <- list()
  remove <- integer(0)
  tau_lines <- grep("&tau;<sub>", rows, fixed = TRUE)
  for (line_index in tau_lines) {
    value_index <- line_index + 1
    if (line_index <= 1 || value_index > length(rows)) {
      next
    }
    tau_label <- extract_tau_label(rows[line_index])
    tau_value <- strip_html(rows[value_index])
    if (is.null(tau_label) || !nzchar(tau_value)) {
      next
    }
    values[[tau_label]] <- tau_value
    remove <- c(remove, line_index - 1, line_index, value_index)
    if (value_index + 1 <= length(rows) && !nzchar(trimws(rows[value_index + 1]))) {
      remove <- c(remove, value_index + 1)
    }
  }
  list(values = values, remove = unique(remove))
}

#' Extract the fixed-effect label that corresponds to a tau row
#'
#' @param row tau label row
#' @noRd
extract_tau_label <- function(row) {
  tau_id <- sub(".*&tau;<sub>([^<]+)</sub> <sub>([^<]+)</sub>.*", "\\1", row)
  group_term <- sub(".*&tau;<sub>([^<]+)</sub> <sub>([^<]+)</sub>.*", "\\2", row)
  if (identical(tau_id, row) || identical(group_term, row)) {
    return(NULL)
  }
  if (tau_id == "00") {
    return("(Intercept)")
  }
  if (!grepl(".", group_term, fixed = TRUE)) {
    return(NULL)
  }
  sub("^[^.]+\\.", "", group_term)
}

#' Strip simple HTML tags and trim whitespace
#'
#' @param x HTML text
#' @noRd
strip_html <- function(x) {
  trimws(gsub("<[^>]+>", "", x))
}

#' Remove rows by index
#'
#' @param rows HTML table split into lines
#' @param remove row indexes to remove
#' @noRd
remove_tau_rows <- function(rows, remove) {
  rows[-remove]
}

#' Remove rho summary rows from sjPlot output
#'
#' @param rows HTML table split into lines
#' @noRd
remove_rho_rows <- function(rows) {
  rho_lines <- grep("&rho;<sub>", rows, fixed = TRUE)
  if (length(rho_lines) == 0) {
    return(rows)
  }
  remove <- integer(0)
  for (line_index in rho_lines) {
    value_index <- line_index + 1
    remove <- c(remove, line_index - 1, line_index, value_index)
    if (value_index + 1 <= length(rows) && !nzchar(trimws(rows[value_index + 1]))) {
      remove <- c(remove, value_index + 1)
    }
  }
  rows[-unique(remove)]
}

#' Extract intercept/slope correlations from a fitted model
#'
#' @param mdl fitted lme4 model
#' @noRd
extract_intercept_rhos <- function(mdl) {
  if (is.null(mdl)) {
    return(list())
  }
  values <- list()
  varcorr <- lme4::VarCorr(mdl)
  for (group in names(varcorr)) {
    correlations <- attr(varcorr[[group]], "correlation")
    if (is.null(correlations) || !"(Intercept)" %in% colnames(correlations)) {
      next
    }
    terms <- setdiff(colnames(correlations), "(Intercept)")
    for (term in terms) {
      rho <- correlations["(Intercept)", term]
      if (!is.finite(rho)) {
        next
      }
      values[[term]] <- append(
        values[[term]],
        list(list(group = group, value = format_table_number(rho)))
      )
    }
  }
  values
}

#' Add random-component column header
#'
#' @param rows HTML table split into lines
#' @param col numeric column index for CSS class
#' @param label column label
#' @noRd
add_random_component_header <- function(rows, col, label) {
  header_index <- grep("<td class=\"depvarhead", rows, fixed = TRUE)
  header_index <- header_index[length(header_index)]
  header <- paste0(
    "    <td class=\"depvarhead firsttablerow col",
    col,
    "\">",
    label,
    "</td>"
  )
  append(rows, header, after = header_index)
}

#' Add random-component cells to fixed-effect rows
#'
#' @param rows HTML table split into lines
#' @param values named list mapping predictor labels to values
#' @param col numeric column index for CSS class
#' @param tooltip_fun function that creates a tooltip from a row label and value
#' @noRd
add_random_component_cells <- function(rows, values, col, tooltip_fun) {
  row_starts <- grep("<td class=\"tdata firsttablecol col1\">", rows, fixed = TRUE)
  offset <- 0
  for (row_start in row_starts) {
    row_start <- row_start + offset
    label <- strip_html(rows[row_start])
    value <- values[[label]]
    if (is.null(value)) {
      value <- "&nbsp;"
    } else {
      value <- tooltip_span(
        format_random_component_value(value),
        tooltip_fun(label, value)
      )
    }
    cell <- paste0(
      "    <td class=\"tdata centeralign modelcolumn1 col",
      col,
      "\">",
      value,
      "</td>"
    )
    row_end <- row_start
    while (row_end <= length(rows) && !grepl("</tr>", rows[row_end], fixed = TRUE)) {
      row_end <- row_end + 1
    }
    rows <- append(rows, cell, after = row_end - 1)
    offset <- offset + 1
  }
  rows
}

#' Format a random-component cell value
#'
#' @param value random-component value
#' @noRd
format_random_component_value <- function(value) {
  if (is.list(value) && !is.null(value[[1]]$group)) {
    values <- vapply(value, function(x) x$value, character(1))
    return(paste(values, collapse = "<br>"))
  }
  value
}

#' Format tau tooltip
#'
#' @param label fixed-effect row label
#' @param value tau value
#' @noRd
format_tau_tooltip <- function(label, value) {
  paste0("&tau; for ", label, ": Estimated random-effect variance component.")
}

#' Format random-intercept correlation tooltip
#'
#' @param label fixed-effect row label
#' @param value rho value
#' @noRd
format_intercept_rho_tooltip <- function(label, value) {
  paste0(
    "&rho; for ", label, "<br>",
    "Estimated correlation between the group-level random intercept and this predictor's random slope."
  )
}

#' Get model-column colspan from sjPlot table
#'
#' @param table HTML table created by sjPlot
#' @noRd
get_model_colspan <- function(table) {
  match <- regexec("<th colspan=\"([0-9]+)\" class=\"thead firsttablerow\">", table)
  pieces <- regmatches(table, match)[[1]]
  if (length(pieces) < 2) {
    return(NA_integer_)
  }
  as.integer(pieces[2])
}

#' Rename lower summary section
#'
#' @param table HTML table created by sjPlot
#' @noRd
rename_random_effects_section <- function(table) {
  sub(">Random Effects</td>", ">Model summary</td>", table, fixed = TRUE)
}

#' Set section-header colspan after adding tau column
#'
#' @param table HTML table created by sjPlot
#' @param colspan colspan for section header
#' @noRd
set_randomparts_colspan <- function(table, colspan) {
  pattern <- "(<td colspan=\")[0-9]+(\" class=\"randomparts\">Model summary</td>)"
  replace_first_match(table, pattern, function(pieces) {
    paste0(pieces[2], colspan, pieces[3])
  })
}

#' Replace one summary label and add a tooltip to label and value cells
#'
#' @param table HTML table created by sjPlot
#' @param old_label current label HTML
#' @param new_label replacement label HTML
#' @param tooltip title text shown by browser tooltip
#' @noRd
sub_summary_label <- function(table, old_label, new_label, tooltip) {
  pattern <- paste0(
    "(<td class=\"tdata leftalign summary[^\"]*\">)",
    old_label,
    "(</td>\\s*<td class=\"tdata summary summarydata[^\"]*\" colspan=\"[^\"]*\">)",
    "([^<]*)",
    "(</td>)"
  )
  replace_first_match(table, pattern, function(pieces) {
    paste0(pieces[2], new_label, pieces[3],
           tooltip_span(pieces[4], tooltip), pieces[5])
  })
}

#' Add a tooltip to summary rows whose labels contain a string
#'
#' @param table HTML table created by sjPlot
#' @param label_contains text identifying matching labels
#' @param tooltip title text shown by browser tooltip
#' @noRd
add_title_to_summary_rows <- function(table, label_contains, tooltip) {
  pattern <- paste0(
    "(<td class=\"tdata leftalign summary[^\"]*\">)",
    "(.*?",
    label_contains,
    ".*?)(</td>\\s*<td class=\"tdata summary summarydata[^\"]*\" colspan=\"[^\"]*\">)",
    "([^<]*)",
    "(</td>)"
  )
  replace_all_matches(table, pattern, function(pieces) {
    paste0(pieces[2], pieces[3], pieces[4],
           tooltip_span(pieces[5], tooltip), pieces[6])
  })
}

#' Add separate tooltips for marginal and conditional R-squared
#'
#' @param table HTML table created by sjPlot
#' @param mdl fitted lme4 model
#' @noRd
add_r2_tooltips <- function(table, mdl) {
  components <- calculate_r2_components(mdl)
  if (is.null(components)) {
    marginal_tooltip <- paste0(
      "R<sup>2</sup><sub>m</sub><br>",
      "Marginal R-squared: proportion of model-implied variance explained by fixed effects only."
    )
    conditional_tooltip <- paste0(
      "R<sup>2</sup><sub>c</sub><br>",
      "Conditional R-squared: proportion of model-implied variance explained by fixed and random effects together."
    )
  } else {
    marginal_tooltip <- paste0(
      "Marginal R-squared: fixed-effects share of the model-implied variance.<br>",
      "R<sup>2</sup><sub>m</sub> = ",
      sigma_squared_label("FE"), " / (",
      sigma_squared_label("FE"), " + ", sigma_squared_label("RE"), " + ",
      sigma_squared_label(), ")<br>",
      "= ", format_table_number(components$fixed), " / (",
      format_table_number(components$fixed), " + ",
      format_table_number(components$random), " + ",
      format_table_number(components$residual), ") = ",
      format_table_number(components$marginal)
    )
    conditional_tooltip <- paste0(
      "Conditional R-squared: fixed plus random effects share of the model-implied variance.<br>",
      "R<sup>2</sup><sub>c</sub> = (",
      sigma_squared_label("FE"), " + ", sigma_squared_label("RE"), ") / (",
      sigma_squared_label("FE"), " + ", sigma_squared_label("RE"), " + ",
      sigma_squared_label(), ")<br>",
      "= (", format_table_number(components$fixed), " + ",
      format_table_number(components$random), ") / (",
      format_table_number(components$fixed), " + ",
      format_table_number(components$random), " + ",
      format_table_number(components$residual), ") = ",
      format_table_number(components$conditional)
    )
  }

  label <- "Marginal R<sup>2</sup> / Conditional R<sup>2</sup>"
  pattern <- paste0(
    "(<td class=\"tdata leftalign summary[^\"]*\">)",
    "Marginal R<sup>2</sup> / Conditional R<sup>2</sup>",
    "(</td>\\s*<td class=\"tdata summary summarydata[^\"]*\" colspan=\"[^\"]*\">)",
    "([^<]*)",
    "(</td>)"
  )
  match <- regexec(pattern, table, perl = TRUE)
  pieces <- regmatches(table, match)[[1]]
  if (length(pieces) == 0) {
    return(table)
  }
  values <- strsplit(pieces[4], " / ", fixed = TRUE)[[1]]
  if (length(values) != 2) {
    return(table)
  }
  value <- paste0(
    tooltip_span(values[1], marginal_tooltip),
    " / ",
    tooltip_span(values[2], conditional_tooltip)
  )
  replacement <- paste0(pieces[2], label, pieces[3], value, pieces[5])
  replace_first_match(table, pattern, function(pieces) replacement)
}

#' Create ICC tooltip text
#'
#' @param mdl fitted lme4 model
#' @noRd
create_icc_tooltip <- function(mdl) {
  components <- calculate_r2_components(mdl)
  if (is.null(components)) {
    return(paste0(
      "ICC<br>",
      "Intraclass correlation coefficient: share of model-implied variance attributable to grouping."
    ))
  }
  icc <- components$random / (components$random + components$residual)
  paste0(
    "Intraclass correlation coefficient: share of non-fixed model variance attributable to grouping.<br>",
    "ICC = ", sigma_squared_label("RE"), " / (",
    sigma_squared_label("RE"), " + ", sigma_squared_label(), ")<br>",
    "= ", format_table_number(components$random), " / (",
    format_table_number(components$random), " + ",
    format_table_number(components$residual), ") = ",
    format_table_number(icc)
  )
}

#' Calculate R-squared variance components
#'
#' @param mdl fitted lme4 model
#' @noRd
calculate_r2_components <- function(mdl) {
  if (!requireNamespace("insight", quietly = TRUE)) {
    return(NULL)
  }
  variances <- tryCatch(
    suppressWarnings(insight::get_variance(mdl)),
    error = function(e) NULL
  )
  if (is.null(variances)) {
    return(NULL)
  }
  fixed <- as.numeric(variances$var.fixed)
  random <- as.numeric(variances$var.random)
  residual <- as.numeric(variances$var.residual)
  if (!identical(lengths(list(fixed, random, residual)), c(1L, 1L, 1L))) {
    return(NULL)
  }
  if (any(!is.finite(c(fixed, random, residual)))) {
    return(NULL)
  }
  total <- fixed + random + residual
  if (!is.finite(total) || total <= 0) {
    return(NULL)
  }
  list(
    fixed = fixed,
    random = random,
    residual = residual,
    marginal = fixed / total,
    conditional = (fixed + random) / total
  )
}

#' Create a tooltip span
#'
#' @param label visible label
#' @param tooltip HTML tooltip body
#' @noRd
tooltip_span <- function(label, tooltip) {
  paste0(
    "<span class=\"mimosa-tooltip\" tabindex=\"0\">",
    label,
    "<span class=\"mimosa-tooltip-box\">",
    tooltip,
    "</span></span>"
  )
}

#' HTML label for sigma-squared
#'
#' @param subscript optional subscript
#' @noRd
sigma_squared_label <- function(subscript = NULL) {
  label <- "&sigma;<sup>2</sup>"
  if (!is.null(subscript)) {
    label <- paste0(label, "<sub>", subscript, "</sub>")
  }
  label
}

#' Replace first regex match without interpreting replacement backslashes
#'
#' @param text source text
#' @param pattern regex pattern
#' @param replacement_fun function receiving regex pieces
#' @noRd
replace_first_match <- function(text, pattern, replacement_fun) {
  match <- regexpr(pattern, text, perl = TRUE)
  if (match[1] < 0) {
    return(text)
  }
  match_text <- regmatches(text, match)
  pieces <- regmatches(match_text, regexec(pattern, match_text, perl = TRUE))[[1]]
  replacement <- replacement_fun(pieces)
  start <- as.integer(match[1])
  end <- start + attr(match, "match.length") - 1
  paste0(
    substr(text, 1, start - 1),
    replacement,
    substr(text, end + 1, nchar(text))
  )
}

#' Replace all regex matches without interpreting replacement backslashes
#'
#' @param text source text
#' @param pattern regex pattern
#' @param replacement_fun function receiving regex pieces
#' @noRd
replace_all_matches <- function(text, pattern, replacement_fun) {
  repeat {
    next_text <- replace_first_match(text, pattern, replacement_fun)
    if (identical(next_text, text)) {
      return(text)
    }
    text <- next_text
  }
}
