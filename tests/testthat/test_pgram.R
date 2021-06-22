set.seed(1789)
deltat = 0.1
tau = 25
tt = seq(0, 24 * 3, deltat)
x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

periodRange = c(20, 30)
dopar = FALSE

test_that('CSP greedy', {
  name = 'csp_greedy'
  specObs = cspgram(x, deltat, periodRange = periodRange,
                    method = 'greedy', dopar = dopar)
  # qs::qsave(specObs, sprintf('spec_%s.qs', name))
  specExp = qs::qread(sprintf('spec_%s.qs', name))
  expect_equal(specObs, specExp)
})

test_that('CSP conservative', {
  name = 'csp_conservative'
  specObs = cspgram(x, deltat, periodRange = periodRange,
                    method = 'conservative', dopar = dopar)
  # qs::qsave(specObs, sprintf('spec_%s.qs', name))
  specExp = qs::qread(sprintf('spec_%s.qs', name))
  expect_equal(specObs, specExp)
})

test_that('CSP standard', {
  name = 'csp_standard'
  specObs = cspgram(x, deltat, periodRange = periodRange,
                    method = 'standard', dopar = dopar)
  # qs::qsave(specObs, sprintf('spec_%s.qs', name))
  specExp = qs::qread(sprintf('spec_%s.qs', name))
  expect_equal(specObs, specExp)
})

test_that('FFT', {
  name = 'fft'
  specObs = fftpgram(x, deltat, periodRange = periodRange)
  # qs::qsave(specObs, sprintf('spec_%s.qs', name))
  specExp = qs::qread(sprintf('spec_%s.qs', name))
  expect_equal(specObs, specExp)
})

test_that('Lomb-Scargle', {
  name = 'lsp'
  specObs = lspgram(x, deltat, periodRange = periodRange)
  # qs::qsave(specObs, sprintf('spec_%s.qs', name))
  specExp = qs::qread(sprintf('spec_%s.qs', name))
  expect_equal(specObs, specExp)
})
