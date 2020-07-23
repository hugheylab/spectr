context('FFT Functional Test')

library(data.table)

test_that('FFT', {
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  specObs = fftpgram(x, deltat, periodRange = c(20,30))
  fwrite(specObs, file = "fft_gen.csv")

  specExpect = fread('spec_fft.csv')

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = "fft_all_eq_test.txt")
  expect_true(specEqual)
})
