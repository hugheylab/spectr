context('Lomb Scargle Functional Test')


test_that('Lomb Scargle', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  lombscargle = lspgram(x, deltat, periodRange = c(20,30))
  fwrite(lombscargle, file = "lombs_gen.csv")

  lombscargleExpect = fread('spec_lombs.csv')

  lombsAllEqTest = all.equal(lombscargle, lombscargleExpect, check.attributes = FALSE, tolerance = 0.001)
  write(lombsAllEqTest, file = "lombs_all_eq_test.txt")
  expect_true(all.equal(lombscargle, lombscargleExpect, check.attributes = FALSE, tolerance = 0.001))
})
