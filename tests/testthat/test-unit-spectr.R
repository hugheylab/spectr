context('spectr Unit Tests')

library(data.table)

test_that('spectr CSP Greedy', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  greedyChisq = spectr(x, deltat, periodRange = c(20,30), method = 'greedy_chisq')

})

test_that('spectr CSP Conservative', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  conservativeChisq = spectr(x, deltat, periodRange = c(20,30), method = 'conservative_chisq')
})

test_that('spectr CSP Standard', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  standardChisq = spectr(x, deltat, periodRange = c(20,30), method = 'standard_chisq')
})

test_that('spectr Lomb Scargle', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  lombscargle = spectr(x, deltat, periodRange = c(20,30), method = 'lombscargle')
})

test_that('spectr FFT', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  fft = spectr(x, deltat, periodRange = c(20,30), method = 'fft')
})
