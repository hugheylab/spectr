# spectr 1.0.1
* Revised code to not need `globalVariables()` in order to pass R CMD check.

# spectr 1.0.0
* Updated for lomb 2.0

# spectr 0.0.0.9017
* Switched to using `stats::na.fail` for default `na.action`

# spectr 0.0.0.9016
* Moved example in documentation to roxygen comments to avoid funny business in devtools testing

# spectr 0.0.0.9015
* Removed `pval` from output of `cspgram`, since `log_pval` is preferred
* Simplified testing suite

# spectr 0.0.0.9014
* Fixed setting of attribute in `cspgram` so `data.table` doesn't throw a warning
* Added `pkgdown` site
