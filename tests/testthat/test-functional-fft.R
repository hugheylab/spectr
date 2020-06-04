context('FFT Functional Test')


test_that('FFT', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))
  fft = fftpgram(x, deltat,periodRange = c(20,30))
  write.csv(fft, file = "fftGen.csv")

  fftExp = fread('spec_Fft.csv')
  fftExp[,V1:=NULL]

  fftAllEqTest5 = all.equal(fft,fftExp,check.attributes=FALSE,tolerance = 0.001)
  write(fftAllEqTest5, file = "fftAllEqTest5.txt")
  expect_true(all.equal(fft,fftExp,check.attributes=FALSE,tolerance = 0.001))
})
