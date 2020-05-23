checkX = function(x) {
  stopifnot(is.vector(x, 'numeric'))
  invisible(0)}


checkSingleNum = function(z, lower, inclusive) {
  if (isTRUE(inclusive)) {
    op = `>=`
  } else {
    op = `>`}
  stopifnot(is.vector(z, 'numeric'),
            length(z) == 1L,
            op(z, lower))
  invisible(0)}


checkTime = function(x, deltat, time) {
  stopifnot(!(missing(deltat) && missing(time)))

  if (missing(time)) {
    checkSingleNum(deltat, 0, FALSE)
    time = seq(0, by = deltat, length.out = length(x))
  } else {
    stopifnot(is.vector(time, 'numeric'),
              length(time) == length(x))}
  return(time)}


checkPeriodRange = function(periodRange) {
  stopifnot(is.vector(periodRange, 'numeric'),
            length(periodRange) == 2L,
            all(periodRange >= 0),
            diff(periodRange) > 0)
  invisible(0)}
