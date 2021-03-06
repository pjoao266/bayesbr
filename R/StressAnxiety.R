#' Dependency of Anxiety on Stress
#'
#'For this data, stress and anxiety were measured among nonclinical women in Townsville, Queensland, Australia.
#'@usage data("StressAnxiety")
#'@details   Both variables were evaluated on the scales from 0 to 42, Smithson and Verkuilen (2006) transformed them in a range from 0 to 1.
#'@source Example 2 from Smithson and Verkuilen (2006) supplements.
#' @format   A data frame containing 166 observations on 2 variables.
#'\describe{
#'  \item{stress}{score, linearly transformed to the open unit
#'    interval (see below).}
#'  \item{anxiety}{score, linearly transformed to the open unit
#'    interval (see below).}
#'}
#'@references
#'\doi{10.1037/1082-989X.11.1.54} Smithson, M., and Verkuilen, J. (2006).
#'A Better Lemon Squeezer? Maximum-Likelihood Regression with
#'Beta-Distributed Dependent Variables.
#'\emph{Psychological Methods}, \bold{11}(7), 54--71.
#'@examples
#'data("StressAnxiety", package = "bayesbr")
#'bbr <- bayesbr(anxiety ~ stress | stress,
#'               data = StressAnxiety, iter = 100)
#'
#'summary(bbr)
"StressAnxiety"
