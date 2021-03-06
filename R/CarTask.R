#' Probability Judgment for Car Dealership with Partition
#'
#' Participants who responded to the study were expected to judge the likelihood of a customer trades in a coupe or that a customer buys a car from a specific seller among four possible sellers.
#'@usage data(CarTask)
#'@details Study participants were graduate students from The Australian National University, some students received credits in Psychology for participating in the study.
#'
#'
#'With the Needs for Closing and Needs for Certainty scales strongly correlated, the NFCCscale is a combined scale between the previous two.
#'
#'For \code{task} the questions were:
#'  \describe{
#'    \item{Car}{What is the probability that a customer trades in a coupe?}
#'    \item{Salesperson}{What is the probability that a customer buys a
#'      car from Carlos?}
#'  }
#'
#'The \code{task} variable that was a qualitative variable was transformed into a quantitative variable to be used by the package functions.
#' @format   A data frame with 155 observations on the following 3 variables.
#' \describe{
#'   \item{\code{task}}{A variable specified as conditions. When 0 the set value is \code{Car}, when 1 the set value is \code{Salesperson}.}
#'   \item{\code{probability}}{a numeric vector of the estimated probability.}
#'   \item{\code{NFCCscale}}{a numeric vector of the NFCC scale.}
#' }
#'@references
#'\doi{10.3102/1076998610396893} Smithson, M., Merkle, E.C., and Verkuilen, J. (2011). Beta
#'Regression Finite Mixture Models of Polarization and Priming.
#'\emph{Journal of Educational and Behavioral Statistics}, \bold{36}(6), 804--831.
#'@references
#'\doi{10.1080/15598608.2009.10411918} Smithson, M., and Segale, C. (2009). Partition Priming in Judgments of
#'Imprecise Probabilities. \emph{Journal of Statistical Theory and
#'  Practice}, \bold{3}(1), 169--181.
#' @examples
#'data("CarTask", package = "bayesbr")
#'
#'car_bayesbr <- bayesbr(probability ~ NFCCscale + task, data = CarTask,
#'                       iter =100)
"CarTask"
