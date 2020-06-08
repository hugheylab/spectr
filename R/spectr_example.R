library('data.table')

set.seed(1789)
deltat = 0.1
tau = 25
tt = seq(0, 24 * 3, deltat)
x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

spec = spectr(x, deltat)
specPeak = spec[which.min(log_pval)]
