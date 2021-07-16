#'@title Coefficients for deltas
#'@aliases summary_delta
#'@name summary_delta
#'@description A function that uses posterior distribution values of the model and calculates the estimates for delta parametrer.
#'@usage summary_delta(x,prob=0.95)
#'@param x an object of the class \emph{bayesbr}, containing the list returned from the \code{\link{bayesbr}} function.
#'@param prob a probability containing the credibility index for the HPD interval for the coefficients of the covariates.
#'@return A list containing the estimates for delta parametrer, this list contains the following items:
#'\describe{
#'\item{table}{a table with the means, medians, standard deviations and the Highest Posterior Density (HPD) Interval,}
#'\item{coeff}{a vector containing the estimated coefficients.}}
#'@seealso \code{\link{summary_xi}},\code{\link{values}},\code{\link{summary.bayesbr}}
summary_delta = function(x,prob=0.95){
  deltas = x$info$samples$delta
  n = x$info$n
  warmup = x$info$warmup
  iter = x$info$iter

  table = NULL
  coeff = numeric()

  for (i in 1:n) {
    aux = paste0('delta[',i,']')
    delta = deltas[[aux]]

    mean_t = round(mean(delta),5)
    coeff = c(coeff,mean_t)
    median_t = round(median(delta),5)
    sd_t = round(sd(delta),5)
    delta_mcmc = as.mcmc( c(delta) )
    hpd = HPDinterval(delta_mcmc, prob=prob)
    vec = c(mean_t,median_t,sd_t,round(hpd[1:2],5))
    table = rbind(table,vec)
  }

  colnames(table) = c("Mean","Median", "Std. Dev.","HPD_inf","HPD_sup")
  rownames(table) = paste0("delta ",1:n)
  names(coeff) = paste0("delta ",1:n)

  list = list(table = table,deltas = coeff)
  return(list)
}
