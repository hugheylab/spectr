context('lspgram')

test_that('Lomb-Scargle', {
  name = 'lsp'

  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  periodRange = c(20, 30)

  specObs = lspgram(x, deltat, periodRange = periodRange)
  data.table::fwrite(specObs, sprintf('%s_obs.csv', name))

  specExpect = data.table::fread(sprintf('%s_exp.csv', name))

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = sprintf('%s_equal.txt', name))
  expect_true(specEqual)
})
