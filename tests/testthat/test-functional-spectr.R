context('spectr Functional Tests')

library(data.table)

test_that('spectr CSP Greedy', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  specObs = spectr(x, deltat, periodRange = c(20,30), method = 'greedy_chisq')
  fwrite(specObs, file = "spec_greedy_gen.csv")

  specExpect = fread('spec_greedy.csv')

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE)
  write(specEqual, file = "spectr_all_eq_greedy_chisq.txt")
  expect_true(specEqual)

})

test_that('spectr CSP Conservative', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  specObs = spectr(x, deltat, periodRange = c(20,30), method = 'conservative_chisq')
  fwrite(specObs, file = "spec_cons_gen.csv")

  specExpect = fread('spec_cons.csv')

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = "spectr_all_eq_cons_chisq.txt")
  expect_true(specEqual)
})

test_that('spectr CSP Standard', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  specObs = spectr(x, deltat, periodRange = c(20,30), method = 'standard_chisq')
  fwrite(specObs, file = "spec_stand_gen.csv")

  specExpect = fread('spec_stand.csv')

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = "spectr_all_eq_standard_chisq.txt")
  expect_true(specEqual)
})

test_that('spectr Lomb Scargle', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  specObs = spectr(x, deltat, periodRange = c(20,30), method = 'lombscargle')
  fwrite(specObs, file = "spec_lombs_gen.csv")

  specExpect = fread('spec_lombs.csv')

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = "spectr_all_eq_lombs.txt")
  expect_true(specEqual)
})

test_that('spectr FFT', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  specObs = spectr(x, deltat,periodRange = c(20,30), method = 'fft')
  fwrite(specObs, file = "spec_fft_gen.csv")

  specExpect = fread('spec_fft.csv')

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = "spectr_all_eq_fft.txt")
  expect_true(specEqual)
})
