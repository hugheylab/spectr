library('data.table')

set.seed(1789)
deltat = 0.1
tau = 25
tt = seq(0, 24 * 3, deltat)
x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))

specCsp = spectr(x, deltat, method = 'greedy')
peakCsp = specCsp[which.min(log_pval)]

specLsp = spectr(x, deltat, method = 'lomb')
peakLsp = specLsp[which.max(power)]

specFft = spectr(x, deltat, method = 'fft')
peakFft = specFft[which.max(power)]
