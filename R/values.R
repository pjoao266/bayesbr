#'@title Values of a Posteriori Distribution
#'@aliases values
#'@name values
#'@description A function that uses the values returned from the sampling function of RStan and returns the parameter chain of the posterior distribution, the parameters can be beta, gamma, theta or zeta.
#'@param type Characters indicating which values will be returned by the function
#'@param obj containing the data returned from the sampling function of the Rstan package. This type of object is used because it returns the values of the posterior distribution of the model.
#'@param iter A positive integer specifying the number of iterations for each chain (including warmup).
#'@param warmup A positive integer specifying the number of iterations that will be in the warm-up period.
#'@param n The number of observations of the model's variable response.
#'@param par A number containing the number of parameters for theta or zeta. If type is equal to beta or theta par is similar to p (number of parameters for theta), otherwise even is similar to q (number of parameters for zeta). When type is equal to 'delta', 'xi', 'tau_xi', or 'tau_delta' the par variable verify spatial effect in adjusted model.
#'@details The function \code{values} returns the parameter of interest by taking the data returned by the Stan function excluding the warmup period data. All data returned is in the format of 5 decimal places.
#'@return A list containing the values according to the type argument, the values are returned excluding the warmups.
#'@seealso \code{\link{summary_mean}},\code{\link{summary_precision}},\code{\link{model.bayesbr}}
values = function(type=c("beta","gamma","theta","zeta","tau_delta","tau_xi","xi","delta"),obj,iter,warmup,n,par){
  type = match.arg(type)
  if(type=="theta"){
    list = list()
    for (i in 1:n) {
      if(par==0){
        v_theta = obj@sim$samples[[1]]$theta
        v_theta = v_theta[(warmup+1):iter]
        v_theta = round(v_theta,5)
        list$theta = v_theta
      }
      else{
        aux = paste0('theta[',i,']')
        v_theta = obj@sim$samples[[1]]
        v_theta = v_theta[[aux]]
        v_theta = v_theta[(warmup+1):iter]
        v_theta = round(v_theta,5)
        list[[aux]] = v_theta
      }
    }
    return(list)
  }
  if(type=="zeta"){
    list = list()
    for (i in 1:n) {
      if(par==0){
        v_zeta = obj@sim$samples[[1]]$zeta
        v_zeta = v_zeta[(warmup+1):iter]
        v_zeta = round(v_zeta,5)
        list$zeta = v_zeta
      }
      else{
        aux = paste0('zeta[',i,']')
        v_zeta = obj@sim$samples[[1]]
        v_zeta = v_zeta[[aux]]
        v_zeta = v_zeta[(warmup+1):iter]
        v_zeta = round(v_zeta,5)
        list[[aux]] = v_zeta
      }
    }
    return(list)
  }
  if(type=="beta"){
    list = list()
    if(par == 1){
      betas = obj@sim$samples[[1]]$'betas'
      betas = round(betas,5)
      betas = betas[(warmup+1):iter]
      list[['betas']] = betas
    }
    else if(par>1){
      for (i in 1:par) {
        aux = paste0('betas[',i,']')
        betas = obj@sim$samples[[1]]
        betas = betas[[aux]]
        betas = round(betas,5)
        betas = betas[(warmup+1):iter]
        list[[aux]] = betas
      }
    }
    else{
      list = NULL
    }
    return(list)
  }
  if(type=="gamma"){
    list = list()
    if(par == 1){
      gammas = obj@sim$samples[[1]]$'gammas'
      gammas = round(gammas,5)
      gammas = gammas[(warmup+1):iter]
      list[['gammas']] = gammas
    }
    else if(par>1) {
      for (i in 1:par) {
        aux = paste0('gammas[',i,']')
        gammas = obj@sim$samples[[1]]
        gammas = gammas[[aux]]
        gammas = round(gammas,5)
        gammas = gammas[(warmup+1):iter]
        list[[aux]] = gammas
      }
    }
    else{
      list = NULL
    }
    return(list)
  }
  if(type=="tau_delta"){
    list = NULL
    if(par==T){
      tau = obj@sim$samples[[1]]$'tau_delta'
      tau = round(tau,5)
      tau = tau[(warmup+1):iter]
      list[['tau_delta']] = tau
    }
    return(list)
  }
  if(type=="tau_xi"){
    list = NULL
    if(par==T){
      tau = obj@sim$samples[[1]]$'tau_xi'
      tau = round(tau,5)
      tau = tau[(warmup+1):iter]
      list[['tau_xi']] = tau
    }
    return(list)
  }
  if(type=="delta"){
    list = NULL

    if(par==T){
      for (i in 1:n) {
        aux = paste0('delta[',i,']')
        deltas = obj@sim$samples[[1]]
        deltas = deltas[[aux]]
        deltas = round(deltas,5)
        deltas = deltas[(warmup+1):iter]
        list[[aux]] = deltas
      }
    }
    return(list)
  }
  if(type=="xi"){
    list = NULL

    if(par==T){
      for (i in 1:n) {
        aux = paste0('xi[',i,']')
        xis = obj@sim$samples[[1]]
        xis = xis[[aux]]
        xis = round(xis,5)
        xis = xis[(warmup+1):iter]
        list[[aux]] = xis
      }
    }
    return(list)
  }
}
