
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CASToolWSStressorPckg

<!-- badges: start -->
<!-- badges: end -->

The goal of CASToolWSStressorPckg is to serve StreamCat stressor data
for reaches in a state.

## Installation

You can install the development version of CASToolWSStressorPckg from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("laura-naslund/CASToolWSStressorPckg")
```

## Example

This is a basic example of how to retrieve stressor data and metadata.

``` r
library(CASToolWSStressorPckg)

## Retrieve stressor data
de_stressor_data <- retrieve_stressor_data("Delaware")

## Retrieve stressor metadata
de_stressor_metadata <- retrieve_stressor_info("Delaware")
```
