#This function fits a survival function to the data and allows for a surviving sub-population.  Such a scenario is
#typical with marketing data, where conversions are considered "deaths" and not all users will eventually convert.
#S: Survival object as constructed by Surv().  Should have "log()-ed" times:
#  S=Surv(log(tl), log(tr), event, type='interval')
#formula: The formula for fitting the survival function.
#data: a data.frame to supply additional data.  Only needed if formula is not S~1.
#weight: observation weights.
#fast: Limit the subpopulation proportion to 3 times the initial deaths.  Allows easier optimization of the likelihood
#  does not ensure a maximum is obtained.
#dist: The distribution
sp_survreg = function( S, formula="S~1", data=NULL, weight=rep(1,nrow(S)), fast=F
  ,dist=c("spweibull","sploglogistic","splognormal") ){
  
  if(length(dist)>1)
    dist = dist[1]
  #sp_survreg assumes S is ordered (to calculate bounds for the asymptote)
  S = S[order(S[,1]),]

  #define sp-weibull distribution:
  spweibull<-list(name='SPWeibull',
    init=function(x, weights, ...) c(median(x), mad(x)),
    density=function(x, prob) {
      cbind(prob * (1 - exp(-exp(x))), # F(x)
        1 + prob * (exp(-exp(x)) - 1), # 1 - F(x)
        prob * exp(-exp(x) + x), # f(x)
        1 - exp(x), # f'(x) / f(x)
        1 + exp(x) * (exp(x) - 3) # f''(x) / f(x)
      )
    },
    quantile=function(p, prob) log(-log((prob - p) / prob)),
    parms=c(prob=1),
    deviance=function(...) stop('deviance residuals not defined')
  )
  
  #define sp-loglogistic distribution:
  sploglogistic<-list(name='SPLogLogistic',
    init=function(x, weights, ...) c(median(x), mad(x)),
    density=function(x, prob) {
      cbind(prob/(1+exp(-x)), # F(x)
        1 - prob/(1+exp(-x)), # 1 - F(x)
        prob * exp(x)/(1+exp(x))^2, # f(x)
        -(exp(x)-1)/(exp(x)+1), # f'(x) / f(x)
        (exp(2*x)-4*exp(x)+1)/(exp(x)+1)^2 # f''(x) / f(x)
      )
    },
    quantile=function(p, prob) -log(prob/x-1),
    parms=c(prob=1),
    deviance=function(...) stop('deviance residuals not defined')
  )

  erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
  erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)
  #define sp-lognormal distribution:
  splognormal<-list(name='SPLogNormal',
    init=function(x, weights, ...) c(median(x), mad(x)),
    density=function(x, prob) {
      cbind(prob/2 * (1+erf(x/sqrt(2))), # F(x)
        1 - prob/2 * (1+erf(x/sqrt(2))), # 1 - F(x)
        prob * exp(-x^2/2)/sqrt(2*pi), # f(x)
        -x, # f'(x) / f(x)
        x^2-1 # f''(x) / f(x)
      )
    },
    quantile=function(p, prob) sqrt(2)*erfinv(2/p*prob-1),
    parms=c(prob=1),
    deviance=function(...) stop('deviance residuals not defined')
  )

  loglik <- function(prob){
    model <- survreg(as.formula(formula), data=data, weight=weight
        , parms=c(prob=prob),dist=eval(parse(text=dist))
        , control=survreg.control(maxiter=1000,rel.tolerance=1e-10))
    return( model$loglik[1] )
  }

  prob0 = sum( weight[-length(weight)] ) / sum(weight)
  max = ifelse( fast, min( 3*prob0, 1), 1 )
#  opt <- optimize( loglik, c(prob0,max), maximum=TRUE, tol=1e-12 )
  opt <- nlminb( start=prob0, objective=function(x) -loglik(x), lower=prob0, upper=max)
  prob.opt <- opt$par
  model <- survreg(formula, weight=weight, parms=c(prob=prob.opt), dist=dist )
  return( list( location=model$coeff, scale=model$scale, p=prob.opt ) )
}
