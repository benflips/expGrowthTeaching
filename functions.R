# Exponential growth general solution
  # paramaterised either with birth and death rates (b-d), or r
expGrowth <- function(t, b, d, N0, r = b-d){
  exp(r*t)*N0
}

# Geometric growth general solution
  # paramaterised either with birth and death rates (1+b)(1-d), or lambda
geom <- function(t, b, d, N0, lambda = (1+b)*(1-d)){
  lambda^t*N0
}

# calculate lambda, given per capita birth, deaths
lambdaCalc <- function(b, d){
  (1+b)*(1-d)
}

# calculate r, given birth and death rates
rCalc <- function(b, d){
  b-d
}

