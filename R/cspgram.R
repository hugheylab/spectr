#' @rdname spectr
#' @export
cspgram = function(x, deltat, periodRange = c(18, 32),
                   method = c('greedy', 'conservative', 'standard'),
                   na.action = stats::na.fail, dopar = FALSE) {
  p = period = df = log_pval = chisq = NULL

  method = match.arg(method)
  checkX(x)
  checkSingleNum(deltat, 0, FALSE)
  checkPeriodRange(periodRange)

  pSpan = (periodRange[1L] / deltat):(periodRange[2L] / deltat)
  if (length(pSpan) == 0) {
    stop('deltat and periodRange are incompatible.')}

  if (isTRUE(dopar)) {
    doOp = `%dopar%`
  } else {
    doOp = `%do%`}

  x = na.action(x)
  k = length(x) %/% max(pSpan) # for conservative
  m = mean(x) # for greedy
  n = length(x)

  d = doOp(foreach(p = pSpan, .combine = rbind), {
    if (method == 'greedy') {
      k = n / p
      xNow = c(x, rep.int(NA, ceiling(k) * p - n))
      xMat = matrix(xNow, ncol = p, byrow = TRUE)
      xH = colMeans(xMat, na.rm = TRUE)
      qP = k * n * sum((xH - m)^2) / sum((x - m)^2)
    } else {
      if (method == 'standard') {
        k = n %/% p}
      xNow = x[1:(k * p)]
      mNow = mean(xNow)
      xMat = matrix(xNow, ncol = p, byrow = TRUE)
      xH = colMeans(xMat)
      qP = k * length(xNow) * sum((xH - mNow)^2) / sum((xNow - mNow)^2)}
    dNow = data.table(p = p, chisq = qP)})

  d[, period := p * deltat]
  d[, df := p - 1]
  d[, p := NULL]

  d[, log_pval := stats::pchisq(chisq, df, lower.tail = FALSE, log.p = TRUE)]

  data.table::setcolorder(d, 'period')
  data.table::setattr(d, 'method', method)
  return(d[])}
