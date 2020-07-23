context('cspgram Unit Tests')

library(data.table)

test_that('CSP Greedy', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  cspGr = cspgram(x, deltat, periodRange = c(20,30), method = 'greedy', dopar = FALSE)
})

test_that('CSP Standard', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  cspSt = cspgram(x, deltat, periodRange = c(20,30), method = 'standard', dopar = FALSE)
})

test_that('CSP Conservative', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  cspCo = cspgram(x, deltat, periodRange = c(20,30), method = 'conservative', dopar = FALSE)
})
