# spectr

[![CircleCI](https://circleci.com/gh/hugheylab/spectr.svg?style=shield)](https://circleci.com/gh/hugheylab/spectr)
[![codecov](https://codecov.io/gh/hugheylab/spectr/branch/master/graph/badge.svg)](https://codecov.io/gh/hugheylab/spectr)

`spectr` provides a consistent interface to use various methods to calculate the periodogram of a time-course. The methods include Lomb-Scargle, fast Fourier transform, and three versions of the chi-square periodogram. To see how we've used `spectr`, check out the [paper](https://doi.org/10.1371/journal.pcbi.1008567).

## Installation

If you use RStudio, go to Tools -> Global Options... -> Packages -> Add... (under Secondary repositories), then enter:

- Name: hugheylab
- Url: https://hugheylab.github.io/drat/

You only have to do this once. Then you can install or update the package by entering:

```R
if (!requireNamespace('BiocManager', quietly = TRUE))
  install.packages('BiocManager')

BiocManager::install('spectr')
```

Alternatively, you can install or update the package by entering:

```R
if (!requireNamespace('BiocManager', quietly = TRUE))
  install.packages('BiocManager')

BiocManager::install('spectr', site_repository = 'https://hugheylab.github.io/drat/')
```

There's also a [docker image](https://hub.docker.com/r/hugheylab/hugheyverse), which has all dependencies installed.

```bash
docker pull hugheylab/hugheyverse
```

## Usage

See the examples and detailed guidance in the [reference documentation](https://spectr.hugheylab.org/reference/index.html).
