# Exponential growth general solution
  # paramaterised either with birth and death rates (b-d), or r
expGrowth <- function(t, b, d, N0 = 1, r = b-d){
  exp(r*t)*N0
}

# Geometric growth general solution
  # paramaterised either with birth and death rates, or R
geomGrowth <- function(t, b, d, N0 = 1, R = 1+b-d){
  R^t*N0
}

# calculate R, given per capita birth, deaths R as in N_{t+1}=RN_t
RCalc <- function(b, d){
  (1+b-d)
}

# calculate r (lambda in geom model), given birth and death rates r as in dN/dt=rN
rCalc <- function(b, d){
  b-d
}

