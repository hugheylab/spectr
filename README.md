# spectr

[![check-deploy](https://github.com/hugheylab/spectr/workflows/check-deploy/badge.svg)](https://github.com/hugheylab/spectr/actions)
[![codecov](https://codecov.io/gh/hugheylab/spectr/branch/master/graph/badge.svg)](https://codecov.io/gh/hugheylab/spectr)
[![Netlify Status](https://api.netlify.com/api/v1/badges/eea83c4a-3e36-4245-8195-d0c15115832b/deploy-status)](https://app.netlify.com/sites/kind-edison-268732/deploys)
[![CRAN Status](https://www.r-pkg.org/badges/version/spectr)](https://cran.r-project.org/package=spectr)
[![drat version](https://raw.githubusercontent.com/hugheylab/drat/gh-pages/badges/spectr_drat_badge.svg)](https://github.com/hugheylab/drat/tree/gh-pages/src/contrib)

`spectr` provides a consistent interface to use various methods to calculate the periodogram of a time-course. The methods include Lomb-Scargle, fast Fourier transform, and three versions of the chi-square periodogram. To see how we've used `spectr`, check out the [paper](https://doi.org/10.1371/journal.pcbi.1008567).

## Installation

### Option 1: CRAN

```r
install.packages('spectr')
```

### Option 2: Hughey Lab Drat Repository

1. Install [`BiocManager`](https://cran.r-project.org/package=BiocManager).

    ```r
    if (!requireNamespace('BiocManager', quietly = TRUE))
      install.packages('BiocManager')
    ```

1. If you use RStudio, go to Tools → Global Options... → Packages → Add... (under Secondary repositories), then enter:

    - Name: hugheylab
    - Url: https://hugheylab.github.io/drat/

    You only have to do this once. Then you can install or update the package by entering:

    ```r
    BiocManager::install('spectr')
    ```

    Alternatively, you can install or update the package by entering:

    ```r
    BiocManager::install('spectr', site_repository = 'https://hugheylab.github.io/drat/')
    ```

## Usage

See the examples and detailed guidance in the [reference documentation](https://spectr.hugheylab.org/reference/index.html).
