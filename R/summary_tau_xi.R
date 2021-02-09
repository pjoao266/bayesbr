#'@title Coefficients for tau_xi
#'@aliases summary_tau_xi
#'@name summary_tau_xi
#'@description A function that uses values of the posterior distribution of the model and calculates the estimates for tau_xi parametrer.
#'@usage summary_tau_xi(x,prob=0.95)
#'@param x an object of the class \emph{bayesbr}, containing the list returned from the \code{\link{bayesbr}} function.
#'@param prob a probability containing the credibility index for the HPD interval for the coefficients of the covariates.
#'@return A list containing the estimates for tau parametrer, this list contains the following items:
#'\describe{
#'\item{table}{a table with the means, medians, standard deviations and the Highest Posterior Density (HPD) Interval,}
#'\item{coeff}{a vector containing the estimated coefficients.}}
#'@seealso \code{\link{summary_xi}},\code{\link{values}},\code{\link{summary_tau_delta}}
summary_tau_xi = function(x,prob=0.95){
  tau = x$info$samples$tau_xi$tau_xi
  warmup = x$info$warmup
  iter = x$info$iter
  table = NULL
  coeff = numeric()
  mean_t = round(mean(tau),5)
  coeff = mean_t

  median_t = round(median(tau),5)
  sd_t = round(sd(tau),5)
  tau_mcmc = as.mcmc( c(tau) )
  hpd = HPDinterval(tau_mcmc, prob=prob)

  table = matrix(c(mean_t,median_t,sd_t,round(hpd[1],5),round(hpd[2],5)),nrow=1)
  colnames(table) = c("Mean","Median", "Std. Dev.","HPD_inf","HPD_sup")
  rownames(table) = 'tau_xi'
  names(coeff) = 'tau_xi'

  list = list(table = table,tau_xi = coeff)
  return(list)
}
