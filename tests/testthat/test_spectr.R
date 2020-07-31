context('spectr')

set.seed(1789)
deltat = 0.1
tau = 25
tt = seq(0, 24 * 3, deltat)
x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

periodRange = c(20, 30)
dopar = FALSE

test_that('spectr CSP Greedy', {
  name = 'spectr_greedy'

  specObs = spectr(x, deltat, periodRange = periodRange, method = 'greedy_chisq', dopar = dopar)
  fwrite(specObs, sprintf('%s_obs.csv', name))

  specExpect = fread(sprintf('%s_exp.csv', name))

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE)
  write(specEqual, file = sprintf('%s_equal.txt', name))
  expect_true(specEqual)
})

test_that('spectr CSP Conservative', {
  name = 'spectr_conservative'

  specObs = spectr(x, deltat, periodRange = periodRange, method = 'conservative_chisq', dopar = dopar)
  fwrite(specObs, sprintf('%s_obs.csv', name))

  specExpect = fread(sprintf('%s_exp.csv', name))

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = sprintf('%s_equal.txt', name))
  expect_true(specEqual)
})

test_that('spectr CSP Standard', {
  name = 'spectr_standard'

  specObs = spectr(x, deltat, periodRange = periodRange, method = 'standard_chisq', dopar = dopar)
  fwrite(specObs, sprintf('%s_obs.csv', name))

  specExpect = fread(sprintf('%s_exp.csv', name))

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = sprintf('%s_equal.txt', name))
  expect_true(specEqual)
})

test_that('spectr Lomb-Scargle', {
  name = 'spectr_lsp'

  specObs = spectr(x, deltat, periodRange = periodRange, method = 'lombscargle')
  fwrite(specObs, sprintf('%s_obs.csv', name))

  specExpect = fread(sprintf('%s_exp.csv', name))

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = sprintf('%s_equal.txt', name))
  expect_true(specEqual)
})

test_that('spectr FFT', {
  name = 'spectr_fft'

  specObs = spectr(x, deltat,periodRange = periodRange, method = 'fft')
  fwrite(specObs, sprintf('%s_obs.csv', name))

  specExpect = fread(sprintf('%s_exp.csv', name))

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = sprintf('%s_equal.txt', name))
  expect_true(specEqual)
})
