#' @importFrom data.table data.table := %between%
#' @importFrom foreach foreach %do% %dopar%
NULL


globalVariables(c('limNow', 'peakIdx', 'p', 'period', 'chisq', 'df', 'pval'))


#' Calculate periodogram
#'
#' Calculate the periodogram of a time series using a fast Fourier transform.
#'
#' @param x Numeric vector of measurements.
#' @param deltat Numeric value of the time interval between measurements.
#' @param pad Numeric value of the proportion of the length of `x` by which to
#'   pad `x` with zeros, passed to `\link[stats]{spec.pgram}()`.
#' @param na.action Function specifying how to handle `NA` values in `x`,
#'   passed to `\link[stats]{spec.pgram}()`. Default is
#'   `\link[imputeTS]{na_ma}()`, which imputes missing values by weighted
#'   moving average.
#' @param ... Other arguments passed to `\link[stats]{spec.pgram}()`.
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
#' spec = spectrPgram(x, deltat)
#' peaks = spectrPeaks(spec[period %between% (tau + c(-4, 4))])
#'
#' @seealso `\link{spectrPeaks}`
#'
#' @export
spectrPgram = function(x, deltat, pad = 20, na.action = imputeTS::na_ma, ...) {
  pg = stats::spec.pgram(stats::ts(x, deltat = deltat), plot = FALSE,
                         pad = pad, na.action = na.action, ...)
  return(data.table(period = 1 / pg$freq, power = pg$spec))}


#' Find peaks in periodogram
#'
#' Find local maxima and use spline interpolation to estimate one or more peaks
#' in the spectral density.
#'
#' @param spec `data.table` with at least columns `period` and `power`.
#' @param nPeaks Integer of the maximum number of peaks to return, sorted by
#'   decreasing `power`.
#' @param splineDf Numeric value of degrees of freedom for the natural cubic
#'   spline used to fit the periodogram around each peak, passed to
#'   `\link[splines]{ns}()`.
#' @param ... Other arguments passed to `\link[pracma]{findpeaks}()`.
#'
#' @return `data.table` of periodogram peaks, with columns `period` and `power`.
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
#' spec = spectrPgram(x, deltat)
#' peaks = spectrPeaks(spec[period %between% (tau + c(-4, 4))])
#'
#' @seealso `\link{spectrPgram}`
#'
#' @export
spectrPeaks = function(spec, nPeaks = 1L, splineDf = 3L, ...) {
  stopifnot(data.table::is.data.table(spec),
            all(c('period', 'power') %in% colnames(spec)),
            nPeaks >= 1, splineDf >= 2)

  data.table::setorderv(spec, 'period')
  peaks = pracma::findpeaks(spec$power, ...)
  if (is.null(peaks)) {
    return(data.table())}

  peaks = peaks[order(peaks[, 1L], decreasing = TRUE), , drop = FALSE]

  dPeak = foreach(peakIdx = 1:min(nPeaks, nrow(peaks)), .combine = rbind) %do% {
    idxLeft = floor(mean(peaks[peakIdx, 2:3]))
    idxRight = ceiling(mean(peaks[peakIdx, c(2L, 4L)]))
    specNow = spec[idxLeft:idxRight, ]

    if (nrow(specNow) >= splineDf + 1L) {
      lmResult = stats::lm(power ~ splines::ns(period, df = splineDf), data = specNow)
      f = function(x) stats::predict(lmResult, data.frame(period = x))
      optResult = stats::optimize(f, interval = range(specNow$period), maximum = TRUE)
      dNow = data.table(period = optResult$maximum,
                        power = optResult$objective)
    } else {
      dNow = data.table(period = spec$period[peaks[peakIdx, 2L]],
                        power = peaks[peakIdx, 1L])}}

  return(dPeak)}


#' Calculate chi-square periodogram
#'
#' Calculate the chi-square periodogram of a time series.
#'
#' @param x Numeric vector of measurements.
#' @param deltat Numeric value of the time interval between measurements.
#' @param periodRange Numeric vector of the minimum and maximum values of the
#'   period, in units of `deltat`, for which to calculate the chi-squared
#'   statistic.
#' @param na.action Function specifying how to handle `NA` values in `x`.
#'   Default is `\link[imputeTS]{na_ma}()`, which imputes missing values by
#'   weighted moving average.
#' @param dopar Logical indicating whether to run calculations in parallel, if
#'   a parallel backend has already been set up, e.g., using
#'   `\link[doParallel]{registerDoParallel}()`.
#'
#' @return `data.table` with columns `period`, `chisq`, `df`, and `pval`.
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
#' specChisq = chisqPgram(x, deltat, dopar = FALSE)
#' peaksChisq = specChisq[which.min(pval)]
#'
#' @seealso `\link{spectrPgram}`
#'
#' @export
chisqPgram = function(x, deltat, periodRange = c(18, 32),
                      na.action = imputeTS::na_ma, dopar = TRUE) {

  stopifnot(is.vector(periodRange, 'numeric'),
            length(periodRange) == 2,
            periodRange[2] - periodRange[1] > 0)

  pSpan = (periodRange[1] / deltat):(periodRange[2] / deltat)
  if (length(pSpan) == 0) {
    stop('deltat and periodRange are incompatible.')}
  k = length(x) %/% max(pSpan)
  x = na.action(x)

  if (isTRUE(dopar)) {
    doOp = `%dopar%`
  } else {
    doOp = `%do%`}

  d = doOp(foreach(p = pSpan, .combine = rbind), {
    xNow = x[1:(k * p)]
    mNow = mean(xNow)
    xMat = matrix(xNow, ncol = p, byrow = TRUE)
    xH = colMeans(xMat)
    qP = (k * length(xNow) * sum((xH - mNow)^2)) / sum((xNow - mNow)^2)
    dNow = data.table(p = p, chisq = qP)})

  d[, period := p * deltat]
  d[, df := p - 1]
  d[, pval := stats::pchisq(chisq, df, lower.tail = FALSE)]
  d[, p := NULL]
  data.table::setcolorder(d, 'period')
  return(d[])}


#' Calculate approximate alpha from a circadian time-course
#'
#' TODO.
#'
#' @param time Numeric vector of time.
#' @param activity Numeric vector of activity measurements.
#' @param tau Numeric value of the period of the time series.
#' @param thresh Numeric value of the activity threshold. If `NULL`, the
#'   `activity` vector is not binarized.
#' @param frac Numeric value of the fraction of activity above threshold (or
#'   fraction of overall activity, if `thresh` is `NULL`) to capture.
#'
#' @return `data.table` with columns `onset` and `width`, both in units of
#'   fraction of `tau`.
#'
#' @examples
#' library('data.table')
#'
#' # TODO
#'
#' @seealso `\link{spectrPgram}`, `\link{spectrPeaks}`
#'
#' @export
spectrAlpha = function(time, activity, tau, thresh, frac = 0.9) {
  # find minimum width of time window that contains at least frac of activity above threshold
  tt = time / tau
  tt = tt - floor(tt)

  if (is.null(thresh)) {
    stopifnot(all(activity >= 0))
    aat = activity
  } else {
    stopifnot(any(activity >= thresh))
    aat = activity >= thresh}

  # given lim and wd, calculate frac
  h = function(lim, wd, tt, aat) {
    idx = ((tt >= lim) & (tt < lim + wd)) | (tt < max(0, lim + wd - 1))
    return(sum(aat[idx], na.rm = TRUE) / sum(aat, na.rm = TRUE))}

  g = function(wd, lim, frac, tt, aat) {
    w = h(lim, wd, tt, aat) - frac
    w = ifelse(w < 0, .Machine$double.xmax, abs(w))
    return(w)}

  f = function(lim, frac, tt, aat) {
    # given frac and lim, optimize wd to find the desired frac, should be convex
    v = stats::optimize(g, interval = c(0, 1), lim, frac, tt, aat)
    return(v$minimum)}

  # given frac, optimize lim to find minimum wd, start near the global minimum
  lim = seq(0, 0.99, 0.01)
  wd = foreach(limNow = lim, .combine = c) %do% {f(limNow, frac, tt, aat)}
  u = stats::optim(lim[which.min(wd)], f, method = 'L-BFGS-B',
                   frac = frac, tt = tt, aat = aat, lower = 0, upper = 1)

  alpha = data.table(onset = u$par, width = u$value)
  return(alpha)}
