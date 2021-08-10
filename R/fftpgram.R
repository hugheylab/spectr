#' @rdname spectr
#' @export
fftpgram = function(x, deltat, periodRange = c(18, 32), pad = 50,
                    na.action = stats::na.fail, ...) {
  period = NULL
  checkX(x)
  checkSingleNum(deltat, 0, FALSE)
  checkPeriodRange(periodRange)
  checkSingleNum(pad, 0, TRUE)

  pg = stats::spec.pgram(
    stats::ts(x, deltat = deltat), plot = FALSE, pad = pad,
    na.action = na.action, ...)

  spec = data.table(period = 1 / pg$freq, power = pg$spec)
  data.table::setorderv(spec, 'period')
  spec = spec[period %between% periodRange]
  return(spec)}
