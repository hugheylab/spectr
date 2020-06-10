context('cspgram Functional Tests')


test_that('CSP Greedy', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  cspGr = cspgram(x, deltat, periodRange = c(20,30), method = 'greedy', dopar = FALSE)
  fwrite(cspGr, file = "csp_gr_gen.csv")

  cspGrExp = fread('spec_greedy.csv')

  allEqCspGr = all.equal(cspGr, cspGrExp, check.attributes = FALSE)
  write(allEqCspGr, file = "all_eq_csp_gr.txt")
  expect_true(all.equal(cspGr, cspGrExp, check.attributes = FALSE))

})

test_that('CSP Standard', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  cspSt = cspgram(x, deltat, periodRange = c(20,30), method = 'standard', dopar = FALSE)
  fwrite(cspSt, file = "csp_st_gen.csv")

  cspStExp = fread('spec_stand.csv')

  allEqCspSt = all.equal(cspSt, cspStExp, check.attributes = FALSE, tolerance = 0.001)
  write(allEqCspSt, file = "all_eq_csp_st.txt")
  expect_true(all.equal(cspSt, cspStExp, check.attributes = FALSE, tolerance = 0.001))
})

test_that('CSP Conservative', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  cspCo = cspgram(x, deltat, periodRange = c(20,30), method = 'conservative', dopar = FALSE)
  fwrite(cspCo, file = "csp_co_gen.csv")

  cspCoExp = fread('spec_cons.csv')

  allEqCspCo = all.equal(cspCo, cspCoExp, check.attributes = FALSE, tolerance = 0.001)
  write(allEqCspCo, file = "all_eq_csp_co.txt")
  expect_true(all.equal(cspCo, cspCoExp, check.attributes = FALSE, tolerance = 0.001))
})
