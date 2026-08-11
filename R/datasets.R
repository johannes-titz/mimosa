#' Simulated popularity data from Hox et al. (2018)
#'
#' The popularity data in popular2 are simulated data for 2000 pupils in 100
#' schools. The purpose is to offer a very simple example for multilevel
#' regression analysis. The main outcome variable is the pupil popularity, a
#' popularity rating on a scale of 1–10 derived by a sociometric procedure.
#' Typically, a sociometric procedure asks all pupils in a class to rate all the
#' other pupils, and then assigns the average received popularity rating to each
#' pupil. Because of the sociometric procedure, group effects as apparent from
#' higher-level variance components are rather strong. There is a second outcome
#' variable: pupil popularity as rated by their teacher, on a scale from 1 to
#' 10. The explanatory variables are pupil gender (boy = 0, girl = 1), pupil
#' extraversion (10-point scale), and teacher experience in years.
#' 
#' @usage data(popular2)
#' @format ## `popular2`
#' A data frame with 2k rows and 15 columns:
#' \describe{
#'   \item{pupil}{}
#'   \item{class}{}
#'   \item{extrav}{}
#'   \item{sex}{}
#'   \item{texp}{}
#'   \item{popular}{}
#'   \item{popteach}{}
#'   \item{Zextrav}{}
#'   \item{Zsex}{}
#'   \item{Ztexp}{}
#'   \item{Zpopular}{}
#'   \item{Zpopteach}{}
#'   \item{Cextrav}{}
#'   \item{Ctexp}{}
#'   \item{Csex}{}
#' }
#' @source <https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/tree/master/chapter%202/popularity>
"popular2"

#' Simulated beetle body-length data
#'
#' A balanced simulated data set from the worked example in Nakagawa and
#' Schielzeth (2013). Its imaginary sampling design contains beetles from 12
#' populations and 120 containers, with two habitats, two dietary treatments,
#' and both sexes. Body length is the continuous response.
#'
#' @usage data(BeetlesBody)
#' @format ## `BeetlesBody`
#' A data frame with 960 rows and 6 columns:
#' \describe{
#'   \item{Population}{Population identifier.}
#'   \item{Container}{Container identifier.}
#'   \item{Sex}{Beetle sex: `Female` or `Male`.}
#'   \item{Habitat}{Microhabitat: `A` or `B`.}
#'   \item{Treatment}{Dietary treatment: `Cont` or `Exp`.}
#'   \item{BodyL}{Body length.}
#' }
#' @references
#' Nakagawa, S. & Schielzeth, H. (2013). A general and simple method for
#' obtaining R2 from generalized linear mixed-effects models. *Methods in
#' Ecology and Evolution*, 4, 133–142. \doi{10.1111/j.2041-210x.2012.00261.x}
#' @source <https://github.com/mastoffel/rptR/blob/master/data/BeetlesBody.rda>
"BeetlesBody"
