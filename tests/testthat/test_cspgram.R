context('cspgram')

# library(data.table)
set.seed(1789)
deltat = 0.1
tau = 25
tt = seq(0, 24 * 3, deltat)
x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

periodRange = c(20, 30)
dopar = FALSE

test_that('CSP Greedy', {
  name = 'csp_greedy'

  specObs = cspgram(x, deltat, periodRange = periodRange, method = 'greedy', dopar = dopar)
  fwrite(specObs, sprintf('%s_obs.csv', name))

  specExpect = fread(sprintf('%s_exp.csv', name))

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE)
  write(specEqual, file = sprintf('%s_equal.txt', name))
  expect_true(specEqual)
})

test_that('CSP Standard', {
  name = 'csp_standard'

  specObs = cspgram(x, deltat, periodRange = periodRange, method = 'standard', dopar = dopar)
  fwrite(specObs, sprintf('%s_obs.csv', name))

  specExpect = fread(sprintf('%s_exp.csv', name))

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = sprintf('%s_equal.txt', name))
  expect_true(specEqual)
})

test_that('CSP Conservative', {
  name = 'csp_conservative'

  specObs = cspgram(x, deltat, periodRange = periodRange, method = 'conservative', dopar = dopar)
  fwrite(specObs, sprintf('%s_obs.csv', name))

  specExpect = fread(sprintf('%s_exp.csv', name))

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = sprintf('%s_equal.txt', name))
  expect_true(specEqual)
})
