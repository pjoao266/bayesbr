#' Mock Jurors' Confidence in Their Verdicts
#'
#'Answers from mock jurors. It presents the difference in the juror's confidence in a conventional two-option verdict (guilt x absolution) versus a three-option verdict (the new option is "unproven"), in the presence or absence of conflicting testimonial evidence.
#'@usage data("MockJurors")
#'@details   The data were collected by Professor Daily at the Australian National University among first-year psychology students. Smithson and Verkuilen (2006) used the original confidence data and transformed it to a scale of 0 to 1, using the following calculation: \code{((original_confidence/100) * 103 - 0.5) / 104}.
#'
#'
#'The \code{verdict} and \code{conflict} variables that was a qualitative variable was transformed into a quantitative variable to be used by the package functions.
#'@source Example 1 from Smithson and Verkuilen (2006) supplements.
#' @format   A data frame containing 104 observations on 3 variables.
#'\describe{
#'  \item{verdict}{a variable indicating whether a two-option or
#'    three-option verdict is requested. If \code{verdict} is 0 is interpreted as two-option, if \code{verdict} is 1 is interpreted as three-option.}
#'  \item{conflict}{Is there conflicting testimonial evidence? If 0, yes. If 1, no.}
#'  \item{confidence}{jurors degree of confidence in his/her verdict,
#'    scaled to the open unit interval.}
#'}
#'@references
#'\doi{10.1037/1082-989X.11.1.54} Smithson, M., and Verkuilen, J. (2006).
#'A Better Lemon Squeezer? Maximum-Likelihood Regression with
#'Beta-Distributed Dependent Variables.
#'\emph{Psychological Methods}, \bold{11}(7), 54--71.
#'@references
#'\doi{10.1080/10888430709336633} Pammer, K., and Kevan, A. (2004).
#'The Contribution of Visual Sensitivity, Phonological Processing
#'and Non-Verbal IQ to Children's Reading.
#'  \emph{Unpublished manuscript}, The Australian National University, Canberra.
#'@examples
#'data("MockJurors", package = "bayesbr")
#'
#'
#'bbr = bayesbr(confidence~verdict+conflict, iter=1000,
#'              data = MockJurors)
"MockJurors"
