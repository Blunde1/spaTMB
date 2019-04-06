## Simulate data
set.seed(123)
n <- 10000
sigma <- .3
phi <- .8
simdata <- function(){
    u <- numeric(n)
    u[1] = rnorm(1)
    if(n>=2)
        for(i in 2:n){
            u[i] = phi * u[i-1] + rnorm(1, sd = sqrt(1 - phi^2))
        }
    u <- u * sigma
    x <- as.numeric( rbinom(n, 1, plogis(u)) )
    data <- list(obs=x)
    data
}
##
data <- simdata()
parameters <- list(phi=phi, logSigma=log(sigma))

## Adapt parameter list
parameters$u <- rep(0,n)

require(TMB)
compile('laplace_example.cpp')
dyn.load(dynlib('laplace_example'))

obj <- MakeADFun(data, parameters, random="u", DLL="laplace_example")
obj$fn()
opt <- nlminb(obj$par, obj$fn, obj$gr)
opt
obj$fn()
obj$env$f()
obj$env$f(obj$env$last.par, order=0)
rep <- sdreport(obj)

dyn.unload(dynlib('laplace_example'))

system.time( opt <- nl)