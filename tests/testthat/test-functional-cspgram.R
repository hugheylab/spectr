context('cspgram Functional Tests')



test_that('CSP Greedy', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))
  cspGr = cspgram(x, deltat,periodRange = c(20,30), method = 'greedy', dopar = FALSE)
  write.csv(cspGr, file = "cspGrGen.csv")

  cspGrExp = fread('cspGr.csv')
  cspGrExp[,V1:=NULL]

  allEqTest1 = all.equal(cspGr,cspGrExp,check.attributes=FALSE)
  write(allEqTest1, file = "allEqTest1.txt")
  expect_true(all.equal(cspGr,cspGrExp,check.attributes=FALSE))

})

test_that('CSP Standard', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))
  cspSt = cspgram(x, deltat,periodRange = c(20,30), method = 'standard', dopar = FALSE)
  write.csv(cspSt, file = "cspStGen.csv")

  cspStExp = fread('cspSt.csv')
  cspStExp[,V1:=NULL]

  allEqTest2 = all.equal(cspSt,cspStExp,check.attributes=FALSE,tolerance = 0.001)
  write(allEqTest2, file = "allEqTest2.txt")
  expect_true(all.equal(cspSt,cspStExp,check.attributes=FALSE,tolerance = 0.001))
})

test_that('CSP Conservative', {
  library(data.table)
  set.seed(1789)
  deltat = 0.1
  tau = 25
  tt = seq(0, 24 * 3, deltat)
  x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))
  cspCo = cspgram(x, deltat,periodRange = c(20,30), method = 'conservative', dopar = FALSE)
  write.csv(cspCo, file = "cspCoGen.csv")

  cspCoExp = fread('cspCo.csv')
  cspCoExp[,V1:=NULL]

  allEqTest3 = all.equal(cspCo,cspCoExp,check.attributes=FALSE,tolerance = 0.001)
  write(allEqTest3, file = "allEqTest3.txt")
  expect_true(all.equal(cspCo,cspCoExp,check.attributes=FALSE,tolerance = 0.001))
})
