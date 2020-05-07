#' @importFrom data.table data.table := %between%
NULL


globalVariables('period')


#' Calculate FFT-based periodogram
#'
#' Calculate the periodogram of a time series using a fast Fourier transform.
#'
#' @param x Numeric vector of measurements.
#' @param deltat Numeric value of the time interval between measurements.
#' @param periodRange Numeric vector of the minimum and maximum values of the
#'   period to consider, in units of `deltat`.
#' @param pad Numeric value of the proportion of the length of `x` by which to
#'   pad `x` with zeros.
#' @param na.action Function specifying how to handle `NA` values in `x`.
#'   Default is [imputeTS::na_ma()], which imputes missing values by weighted
#'   moving average.
#' @param ... Other arguments passed to [stats::spec.pgram()].
#'
#' @return `data.table` of the estimated spectral density, with columns `period`
#'   (in the same units as `deltat`) and `power`.
#'
#' @examples
#' library('data.table')
#'
#' set.seed(1764)
#' deltat = 0.1
#' tau = 24
#' tt = seq(0, tau * 5, deltat)
#' x = sin(tt / tau * 2 * pi) + rnorm(length(tt))
#'
#' spec = spectr(x, deltat)
#' specPeak = spec[which.max(power)]
#'
#' @export
spectr = function(x, deltat, periodRange = c(18, 32), pad = 50,
                  na.action = imputeTS::na_ma, ...) {
  stopifnot(is.vector(x, 'numeric'),
            is.vector(deltat, 'numeric'),
            length(deltat) == 1L,
            deltat > 0,
            is.null(periodRange) || is.vector(periodRange, 'numeric'),
            is.null(periodRange) || length(periodRange) == 2,
            is.null(periodRange) || all(periodRange >= 0),
            is.null(periodRange) || periodRange[2L] - periodRange[1L] > 0,
            is.vector(pad, 'numeric'),
            length(pad) == 1L,
            pad >= 0)

  pg = stats::spec.pgram(stats::ts(x, deltat = deltat), plot = FALSE,
                         pad = pad, na.action = na.action, ...)
  spec = data.table(period = 1 / pg$freq, power = pg$spec)
  data.table::setorderv(spec, 'period')
  if (!is.null(periodRange)) {
    spec = spec[period %between% periodRange]}
  return(spec)}
