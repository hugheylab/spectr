context('cspgram Functional Tests')


test_that('CSP Greedy', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  specObs = cspgram(x, deltat, periodRange = c(20,30), method = 'greedy', dopar = FALSE)
  fwrite(specObs, file = "csp_gr_gen.csv")

  specExpect = fread('spec_greedy.csv')

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE)
  write(specEqual, file = "all_eq_csp_gr.txt")
  expect_true(aspecEqual)

})

test_that('CSP Standard', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  specObs = cspgram(x, deltat, periodRange = c(20,30), method = 'standard', dopar = FALSE)
  fwrite(specObs, file = "csp_st_gen.csv")

  specExpect = fread('spec_stand.csv')

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = "all_eq_csp_st.txt")
  expect_true(specEqual)
})

test_that('CSP Conservative', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  specObs = cspgram(x, deltat, periodRange = c(20,30), method = 'conservative', dopar = FALSE)
  fwrite(specObs, file = "csp_co_gen.csv")

  specExpect = fread('spec_cons.csv')

  specEqual = all.equal(specObs, specExpect, check.attributes = FALSE, tolerance = 0.001)
  write(specEqual, file = "all_eq_csp_co.txt")
  expect_true(specEqual)
})
