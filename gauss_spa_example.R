# Simulate data
set.seed(123)
n <- 10000
mu <- 3
sigma <- 1.5
y <- rnorm(n, mu, sigma)

# data and parameters
data <- list(y=y)
parameters <- list(mu=0, logSigma = log(1), s=numeric(n))

# Compile c++ code and load into R
library(TMB)
compile("gauss_spa_example.cpp")
dyn.load(dynlib("gauss_spa_example"))

# create adfun, set s="random" for SPA inner problem
obj <- MakeADFun(data, parameters, random="s", DLL="gauss_spa_example", silent = T)

# update obj$env$e functions to calculate SPA
source("spaTMB.R")

# optimize
opt <- nlminb(obj$par, obj$fn, obj$gr)
rep <- sdreport(obj)
rbind(summary(rep, "fixed", p.value = TRUE), summary(rep, "report", p.value = TRUE))

# check s = normal sp and parameters
#   arg min K - sx
#   K' - x = 0
#   K' = mu + sigma^2 s = x
#   s = (x - mu)/sigma^2
plot(summary(rep, "random")[,"Estimate"], (y-opt$par[1])/(exp(opt$par[2]))^2,
     main="Numerical versus theoretical saddlepoints",
     xlab="Numerical", ylab="Theoretical")


dyn.unload(dynlib("gauss_spa_example"))