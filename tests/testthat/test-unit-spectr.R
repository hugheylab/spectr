context('spectr Unit Tests')


test_that('spectr CSP Greedy', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  greedy_chisq = spectr(x, deltat, periodRange = c(20,30), method = 'greedy_chisq')

})

test_that('spectr CSP Conservative', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  conservative_chisq = spectr(x, deltat, periodRange = c(20,30), method = 'conservative_chisq')
})

test_that('spectr CSP Standard', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  standard_chisq = spectr(x, deltat, periodRange = c(20,30), method = 'standard_chisq')
})

test_that('spectr Lomb Scargle', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  lombscargle = spectr(x, deltat, periodRange = c(20,30), method = 'lombscargle')
})

test_that('spectr FFT', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  fft = spectr(x, deltat, periodRange = c(20,30), method = 'fft')
})
