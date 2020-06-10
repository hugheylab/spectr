context('spectr Functional Tests')


test_that('spectr CSP Greedy', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  greedyChisq = spectr(x, deltat, periodRange = c(20,30), method = 'greedy_chisq')
  fwrite(greedyChisq, file = "spec_greedy_gen.csv")

  greedyChisqExp = fread('spec_greedy.csv')

  spectrAllEqGreedyChisq = all.equal(greedyChisq, greedyChisqExp, check.attributes = FALSE)
  write(spectrAllEqGreedyChisq, file = "spectr_all_eq_greedy_chisq.txt")
  expect_true(all.equal(greedyChisq, greedyChisqExp, check.attributes = FALSE))

})

test_that('spectr CSP Conservative', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  conservativeChisq = spectr(x, deltat, periodRange = c(20,30), method = 'conservative_chisq')
  fwrite(conservativeChisq, file = "spec_cons_gen.csv")

  conservativeChisqExp = fread('spec_cons.csv')

  spectrAllEqConsChisq = all.equal(conservativeChisq, conservativeChisqExp, check.attributes = FALSE, tolerance = 0.001)
  write(spectrAllEqConsChisq, file = "spectr_all_eq_cons_chisq.txt")
  expect_true(all.equal(conservativeChisq, conservativeChisqExp, check.attributes = FALSE, tolerance = 0.001))
})

test_that('spectr CSP Standard', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  standardChisq = spectr(x, deltat, periodRange = c(20,30), method = 'standard_chisq')
  fwrite(standardChisq, file = "spec_stand_gen.csv")

  standardChisqExp = fread('spec_stand.csv')

  spectrAllEqStandardChisq = all.equal(standardChisq, standardChisqExp, check.attributes = FALSE, tolerance = 0.001)
  write(spectrAllEqStandardChisq, file = "spectr_all_eq_standard_chisq.txt")
  expect_true(all.equal(standardChisq, standardChisqExp, check.attributes = FALSE, tolerance = 0.001))
})

test_that('spectr Lomb Scargle', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  lombscargle = spectr(x, deltat, periodRange = c(20,30), method = 'lombscargle')
  fwrite(lombscargle, file = "spec_lombs_gen.csv")

  lombscargleExp = fread('spec_lombs.csv')

  spectrAllEqLombs = all.equal(lombscargle, lombscargleExp, check.attributes = FALSE, tolerance = 0.001)
  write(spectrAllEqLombs, file = "spectr_all_eq_lombs.txt")
  expect_true(all.equal(lombscargle, lombscargleExp, check.attributes = FALSE, tolerance = 0.001))
})

test_that('spectr FFT', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  fft = spectr(x, deltat,periodRange = c(20,30), method = 'fft')
  fwrite(fft, file = "spec_fft_gen.csv")

  fftExp = fread('spec_fft.csv')

  spectrAllEqFft = all.equal(fft, fftExp, check.attributes = FALSE, tolerance = 0.001)
  write(spectrAllEqFft, file = "spectr_all_eq_fft.txt")
  expect_true(all.equal(fft, fftExp, check.attributes = FALSE, tolerance = 0.001))
})
