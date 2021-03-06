#' Precise and Imprecise Probabilities and Priming for Weather Task
#'
#'In this experiment, participants judged the likelihood of Sunday being the hottest day of week
#'@usage data(WeatherTask)
#'@details All study participants were from the first or second year, none of the participants had an in-depth knowledge of probability.
#'
#'For \code{priming} the questions were:
#'  \describe{
#'    \item{two-fold}{[What is the probability that] the temperature at
#'      Canberra airport on Sunday will be higher than every other day
#'      next week?}
#'    \item{seven-fold}{[What is the probability that] the highest
#'      temperature of the week at Canberra airport will occur on Sunday?}
#'  }
#'For \code{eliciting} the instructions were if
#'\describe{
#'  \item{precise}{to assign a probability estimate,}
#'  \item{imprecise}{to assign a lower and upper probability estimate.}
#'}
#'
#'The \code{priming} and \code{eliciting} variables that was a qualitative variable was transformed into a quantitative variable to be used by the package functions.
#'@source Taken from Smithson et al. (2011) supplements.
#' @format   A data frame with 345 observations on the following 3 variables.
#'\describe{
#'  \item{\code{priming}}{a variable. If 0, \code{two-fold} (case
#'                                                           prime); If 1, \code{seven-fold} (class prime).}
#'  \item{\code{eliciting}}{a variable. If 0, \code{precise};If 1,
#'    \code{imprecise} (lower and upper limit).}
#'  \item{\code{agreement}}{a numeric vector, probability indicated by
#'    participants or the average between minimum and maximum
#'    probability indicated.}
#'}
#'@references
#'  \doi{10.3102/1076998610396893} Smithson, M., Merkle, E.C., and Verkuilen, J. (2011). Beta
#'Regression Finite Mixture Models of Polarization and Priming.
#'\emph{Journal of Educational and Behavioral Statistics}, \bold{36}(6), 804--831.
#'@references
#'\doi{10.3102/1076998610396893} Smithson, M., and Segale, C. (2009). Partition Priming in Judgments of
#'Imprecise Probabilities. \emph{Journal of Statistical Theory and
#'  Practice}, \bold{3}(1), 169--181.
#'@examples
#'data("WeatherTask", package = "bayesbr")
#' \dontshow{
#' lines = sample(1:345,15)
#' WeatherTask = WeatherTask[lines,]
#' }
#'bbr <- bayesbr(agreement~eliciting+priming, data = WeatherTask,
#'               iter = 200)
"WeatherTask"
