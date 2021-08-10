#' @rdname spectr
#' @export
lspgram = function(x, deltat, time, periodRange = c(18, 32), ofac = 50) {
  period = NULL
  checkX(x)
  time = checkTime(x, deltat, time)
  checkPeriodRange(periodRange)
  checkSingleNum(ofac, 1, TRUE)

  lspResult = lomb::lsp(
    x, time, from = periodRange[1L], to = periodRange[2L], type = 'period',
    ofac = ofac, plot = FALSE)

  spec = as.data.table(lspResult[c('scanned', 'power')])
  data.table::setnames(spec, 'scanned', 'period')
  spec = spec[period %between% periodRange]
  return(spec)}
