set.seed(1789)
deltat = 0.1
tau = 25
tt = seq(0, 24 * 3, deltat)
x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

periodRange = c(20, 30)
dopar = FALSE

test_that('spectr CSP greedy', {
  name = 'spectr_greedy'
  specObs = spectr(x, deltat, periodRange = periodRange,
                   method = 'greedy_chisq', dopar = dopar)
  # qs::qsave(specObs, sprintf('spec_%s.qs', name))
  specExp = qs::qread(sprintf('spec_%s.qs', name))
  expect_equal(specObs, specExp)
})

test_that('spectr CSP conservative', {
  name = 'spectr_conservative'
  specObs = spectr(x, deltat, periodRange = periodRange,
                   method = 'conservative_chisq', dopar = dopar)
  # qs::qsave(specObs, sprintf('spec_%s.qs', name))
  specExp = qs::qread(sprintf('spec_%s.qs', name))
  expect_equal(specObs, specExp)
})

test_that('spectr CSP standard', {
  name = 'spectr_standard'
  specObs = spectr(x, deltat, periodRange = periodRange,
                   method = 'standard_chisq', dopar = dopar)
  # qs::qsave(specObs, sprintf('spec_%s.qs', name))
  specExp = qs::qread(sprintf('spec_%s.qs', name))
  expect_equal(specObs, specExp)
})

test_that('spectr Lomb-Scargle', {
  name = 'spectr_lsp'
  specObs = spectr(x, deltat, periodRange = periodRange,
                   method = 'lombscargle', dopar = dopar)
  # qs::qsave(specObs, sprintf('spec_%s.qs', name))
  specExp = qs::qread(sprintf('spec_%s.qs', name))
  expect_equal(specObs, specExp)
})

test_that('spectr FFT', {
  name = 'spectr_fft'
  specObs = spectr(x, deltat, periodRange = periodRange,
                   method = 'fft', dopar = dopar)
  # qs::qsave(specObs, sprintf('spec_%s.qs', name))
  specExp = qs::qread(sprintf('spec_%s.qs', name))
  expect_equal(specObs, specExp)
})
