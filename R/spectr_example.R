library('data.table')

set.seed(1789)
deltat = 0.1
tau = 25
tt = seq(0, 24 * 3, deltat)
x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

spec_Greedy = spectr(x, deltat,periodRange = c(20,30), method = "greedy_chisq")
fwrite(spec_Greedy, file = "spec_Greedy.csv")

spec_Cons = spectr(x, deltat,periodRange = c(20,30), method = "conservative_chisq")
fwrite(spec_Cons, file = "spec_Cons.csv")

spec_Stand = spectr(x, deltat,periodRange = c(20,30), method = "standard_chisq")
fwrite(spec_Stand, file = "spec_Stand.csv")

spec_Lombs = spectr(x, deltat,periodRange = c(20,30), method = "lombscargle")
fwrite(spec_Lombs, file = "spec_Lombs.csv")

spec_Fft = spectr(x, deltat,periodRange = c(20,30), method = "fft")
fwrite(spec_Fft, file = "spec_Fft.csv")
