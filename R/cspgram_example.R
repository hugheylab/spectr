library('data.table')
library('testthat')

set.seed(1789)
deltat = 0.1
tau = 25
tt = seq(0, 24 * 3, deltat)
x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))
cspGr = cspgram(x, deltat,periodRange = c(20,30), method = 'greedy', dopar = FALSE)

write.csv(cspGr, file = "cspGr.csv")

cspGrPeak = cspGr[which.min(log_pval)]

cspGrPeakExp = data.table(period = 24.4,
                          chisq = 635.4408,
                          df = 243,
                          log_pval = (-83.23465),
                          pval = (7.106419 * 10 ^ (-37)))
expect_true(all.equal(cspGrPeak,cspGrPeakExp,check.attributes=FALSE))


cspSt = cspgram(x, deltat, method = 'standard', dopar = FALSE)
write.csv(cspSt, file = "cspSt.csv")
cspStPeak = cspSt[which.min(log_pval)]

cspStPeakExp = data.table(period = 24,
                          chisq = 616.0034,
                          df = 239,
                          log_pval = (-79.13604),
                          pval = (4.282085 * 10 ^ (-35)))
expect_true(all.equal(cspStPeak,cspStPeakExp,check.attributes=FALSE,tolerance = 0.001))


cspCo = cspgram(x, deltat, method = 'conservative', dopar = FALSE)
write.csv(cspCo, file = "cspCo.csv")
cspCoPeak = cspCo[which.min(log_pval)]

cspCoPeakExp = data.table(period = 25.5,
                          chisq = 468.7745,
                          df = 254,
                          log_pval = (-32.75673),
                          pval = (5.942029 * 10 ^ (-15)))
expect_true(all.equal(cspCoPeak,cspCoPeakExp,check.attributes=FALSE,tolerance = 0.001))
