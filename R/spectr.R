#' @importFrom data.table data.table as.data.table := %between%
#' @importFrom foreach foreach %do% %dopar%
NULL


globalVariables(c('p', 'period', 'chisq', 'df', 'pval', 'log_pval'))


#' Calculate periodogram
#'
#' Calculate periodogram for a time-course using Lomb-Scargle, fast Fourier
#' transform, or various version of chi square. The `spectr` function is a
#' wrapper for the various methods. `lspgram` is in turn a wrapper for
#' [lomb::lsp()], and `fftpgram` a wrapper for [stats::spec.pgram()].
#'
#' @param x Numeric vector of measurements.
#' @param deltat Numeric value of the interval between time-points.
#' @param time Numeric vector of time-points. Can be specified instead of
#'   `deltat` for Lomb-Scargle.
#' @param periodRange Numeric vector of the minimum and maximum values of the
#'   period to consider, in the same units as `deltat` or `time`.
#' @param method Character indicating which method to use.
#' @param pad Numeric value of the proportion of the length of `x` by which to
#'   pad `x` with zeros. Must be > 0. Only used for FFT.
#' @param ofac Integer value of the oversampling factor. Must be >= 1. Only used
#'   for Lomb-Scargle.
#' @param na.action Function specifying how to handle `NA` values in `x`.
#'   Default is [imputeTS::na_ma()], which imputes missing values by weighted
#'   moving average. Ignored for Lomb-Scargle.
#' @param dopar Logical indicating whether to run calculations in parallel if
#'   a parallel backend is already set up, e.g., using
#'   [doParallel::registerDoParallel()]. Only used for chi square.
#' @param ... Other arguments passed to [stats::spec.pgram()] for FFT.
#'
#' @return A `data.table` with various columns depending on the method. For any
#'   version of chi square, columns will be `period`, `chisq`, `df`, `log_pval`,
#'   and `pval`. The log p-value is more reliable than the p-value, since R has
#'   finite precision, so p-values less than about 5e-324 will be set to 0. For
#'   Lomb-Scargle and FFT, columns will be `period` and `power`.
#'
#' @example R/spectr_example.R
#'
#' @export
spectr = function(x, deltat, time, periodRange = c(18, 32),
                  method = c('greedy_chisq', 'conservative_chisq',
                             'standard_chisq', 'lombscargle', 'fft'),
                  ofac = 50, pad = 50, na.action = imputeTS::na_ma,
                  dopar = FALSE, ...) {
  method = match.arg(method)

  if (endsWith(method, 'chisq')) {
    spec = cspgram(x, deltat, periodRange, gsub('_chisq', '', method),
                   na.action, dopar)
  } else if (method == 'lombscargle') {
    spec = lspgram(x, deltat, time, periodRange, ofac)
  } else { # method == 'fft'
    spec = fftpgram(x, deltat, periodRange, pad, na.action, ...)}
  return(spec)}
