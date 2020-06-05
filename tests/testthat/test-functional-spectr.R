context('spectr Functional Tests')


test_that('spectr CSP Greedy', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  greedy_chisq = spectr(x, deltat, periodRange = c(20,30), method = 'greedy_chisq')
  fwrite(greedy_chisq, file = "spec_GreedyGen.csv")

  greedy_chisqExp = fread('spec_Greedy.csv')

  spectrAllEqTest1 = all.equal(greedy_chisq, greedy_chisqExp, check.attributes = FALSE)
  fwrite(spectrAllEqTest1, file = "spectrAllEqTest1.txt")
  expect_true(all.equal(greedy_chisq, greedy_chisqExp, check.attributes = FALSE))

})

test_that('spectr CSP Conservative', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  conservative_chisq = spectr(x, deltat, periodRange = c(20,30), method = 'conservative_chisq')
  fwrite(conservative_chisq, file = "spec_ConsGen.csv")

  conservative_chisqExp = fread('spec_Cons.csv')

  spectrAllEqTest2 = all.equal(conservative_chisq, conservative_chisqExp, check.attributes = FALSE, tolerance = 0.001)
  fwrite(spectrAllEqTest2, file = "spectrAllEqTest2.txt")
  expect_true(all.equal(conservative_chisq, conservative_chisqExp, check.attributes = FALSE, tolerance = 0.001))
})

test_that('spectr CSP Standard', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  standard_chisq = spectr(x, deltat, periodRange = c(20,30), method = 'standard_chisq')
  fwrite(standard_chisq, file = "spec_StandGen.csv")

  standard_chisqExp = fread('spec_Stand.csv')

  spectrAllEqTest3 = all.equal(standard_chisq, standard_chisqExp, check.attributes = FALSE, tolerance = 0.001)
  fwrite(spectrAllEqTest3, file = "spectrAllEqTest3.txt")
  expect_true(all.equal(standard_chisq, standard_chisqExp, check.attributes = FALSE, tolerance = 0.001))
})

test_that('spectr Lomb Scargle', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  lombscargle = spectr(x, deltat, periodRange = c(20,30), method = 'lombscargle')
  fwrite(lombscargle, file = "spec_LombsGen.csv")

  lombscargleExp = fread('spec_Lombs.csv')

  spectrAllEqTest4 = all.equal(lombscargle, lombscargleExp, check.attributes = FALSE, tolerance = 0.001)
  fwrite(spectrAllEqTest4, file = "spectrAllEqTest4.txt")
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
  fwrite(fft, file = "spec_FftGen.csv")

  fftExp = fread('spec_Fft.csv')

  spectrAllEqTest5 = all.equal(fft, fftExp, check.attributes = FALSE, tolerance = 0.001)
  fwrite(spectrAllEqTest5, file = "spectrAllEqTest5.txt")
  expect_true(all.equal(fft, fftExp, check.attributes = FALSE, tolerance = 0.001))
})
