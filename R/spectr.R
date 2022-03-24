#' @importFrom data.table data.table as.data.table := %between%
#' @importFrom foreach foreach %do% %dopar%
NULL




#' Calculate periodogram
#'
#' Calculate periodogram for a time-course using Lomb-Scargle, fast Fourier
#' transform, or selected version of chi-square. The `spectr` function is a
#' wrapper for the various methods. `lspgram` is in turn a wrapper for
#' [lomb::lsp()], and `fftpgram` a wrapper for [stats::spec.pgram()]. Among the
#' versions of chi-square, it is highly recommended to use greedy, which has
#' lower bias than standard and lower variance than conservative.
#'
#' @param x Numeric vector of measurements.
#' @param deltat Numeric value of the interval between time-points.
#' @param time Numeric vector of time-points. Can be specified instead of
#'   `deltat` for Lomb-Scargle.
#' @param periodRange Numeric vector of the minimum and maximum values of the
#'   period to consider, in the same units as `deltat` or `time`.
#' @param method Character indicating which method to use. Can be an unambiguous
#'   substring of the full name.
#' @param pad Numeric value of the proportion of the length of `x` by which to
#'   pad `x` with zeros. Must be > 0. Only used for FFT.
#' @param ofac Integer value of the oversampling factor. Must be >= 1. Only used
#'   for Lomb-Scargle.
#' @param na.action Function specifying how to handle `NA` values in `x`.
#'   Default is [stats::na.fail()], which gives an error if any values are
#'   missing. Ignored for Lomb-Scargle.
#' @param dopar Logical indicating whether to run calculations in parallel if
#'   a parallel backend is already set up, e.g., using
#'   [doParallel::registerDoParallel()]. Only used for chi-square.
#' @param ... Other arguments passed to [stats::spec.pgram()] for FFT.
#'
#' @return A `data.table` with various columns depending on the method. For any
#'   version of chi-square, columns will be `period`, `chisq`, `df`, and
#'   `log_pval`. The log p-value is more reliable than the p-value, since R has
#'   finite precision, so p-values less than about 5e-324 would be set to 0. For
#'   Lomb-Scargle and FFT, columns will be `period` and `power`.
#'
#' @examples
#' library('data.table')
#'
#' set.seed(1789)
#' deltat = 0.1
#' tau = 25
#' tt = seq(0, 24 * 3, deltat)
#' x = 3 * sin(tt / tau * 2 * pi) + rnorm(length(tt))
#'
#' specCsp = spectr(x, deltat, method = 'greedy')
#' peakCsp = specCsp[which.min(log_pval)]
#'
#' specLsp = spectr(x, deltat, method = 'lomb')
#' peakLsp = specLsp[which.max(power)]
#'
#' specFft = spectr(x, deltat, method = 'fft')
#' peakFft = specFft[which.max(power)]
#'
#' @export
spectr = function(
  x, deltat, time, periodRange = c(18, 32),
  method = c('greedy_chisq', 'conservative_chisq', 'standard_chisq',
             'lombscargle', 'fft'),
  ofac = 50, pad = 50, na.action = stats::na.fail, dopar = FALSE, ...) {

  method = match.arg(method)

  if (endsWith(method, 'chisq')) {
    spec = cspgram(
      x, deltat, periodRange, gsub('_chisq', '', method), na.action, dopar)
  } else if (method == 'lombscargle') {
    spec = lspgram(x, deltat, time, periodRange, ofac)
  } else { # method == 'fft'
    spec = fftpgram(x, deltat, periodRange, pad, na.action, ...)}
  return(spec)}
