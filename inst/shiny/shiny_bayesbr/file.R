library(bayesbr)
data = readRDS('C:\\Users\\JoaoPedro\\Documents\\dados_shinyBayesbr.RDS')

formula = "Y ~ 1 | 1"

pars = c('theta','zeta','betas','gammas')

priors_mean_b = c(0)

priors_var_b = c(10)

priors_mean_g = c(0)

priors_var_g = c(10)

iter = 1000

warmup = 500

model = bayesbr(formula = formula, data = data,
  mean_betas = priors_mean_b,variance_betas = priors_var_b,
  mean_gammas = priors_mean_g, variance_gammas = priors_var_g,
  iter = iter, warmup = warmup, pars = pars)

model
