context('cspgram Functional Tests')


test_that('CSP Greedy', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

  cspGr = cspgram(x, deltat, periodRange = c(20,30), method = 'greedy', dopar = FALSE)
  fwrite(cspGr, file = "cspGrGen.csv")

  cspGrExp = fread('spec_Greedy.csv')

  allEqTest1 = all.equal(cspGr, cspGrExp, check.attributes = FALSE)
  fwrite(allEqTest1, file = "allEqTest1.txt")
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
  fwrite(cspSt, file = "cspStGen.csv")

  cspStExp = fread('spec_Greedy.csv')

  allEqTest2 = all.equal(cspSt, cspStExp, check.attributes = FALSE, tolerance = 0.001)
  fwrite(allEqTest2, file = "allEqTest2.txt")
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
  fwrite(cspCo, file = "cspCoGen.csv")

  cspCoExp = fread('spec_Cons.csv')

  allEqTest3 = all.equal(cspCo, cspCoExp, check.attributes = FALSE, tolerance = 0.001)
  fwrite(allEqTest3, file = "allEqTest3.txt")
  expect_true(all.equal(cspCo, cspCoExp, check.attributes = FALSE, tolerance = 0.001))
})
