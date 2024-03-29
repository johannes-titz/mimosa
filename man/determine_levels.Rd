% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{determine_levels}
\alias{determine_levels}
\title{Determines variables on level 1 and level 2}
\usage{
determine_levels(group_variable, data, ignore_na = T, show_prog = F)
}
\arguments{
\item{group_variable}{name of the group variable (id)}

\item{data}{yeah, this is the data}

\item{ignore_na}{whether to ignor na, default = T}

\item{show_prog}{whether to show progress in shiny app, default = F}
}
\value{
list with two elements, consisting of the names of the variables on
each level
}
\description{
By specifying the data and the group variable, the function will categorize
variables in level 1 and level 2.
}
\examples{
determine_levels(find_id(mlmRev::Exam)[1], mlmRev::Exam)
}
