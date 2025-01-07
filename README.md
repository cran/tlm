
<!-- README.md is generated from README.Rmd. Please edit that file -->

## `tlm`

Variables in regression models are frequently transformed to achieve
homogeneity of variance, normality of errors, linearization of
associations, or a more homogeneous distribution of predictors. This
package is a tool to fit linear, logistic, and Poisson regression models
with logarithmic or power transformations. The package also show how to
report and interpret effects in the original scale of the variables.

### Getting started

- The last version released on CRAN can be installed within an R session
  by executing:

``` r
install.packages("tlm")
```

- The package tlm is available on the Comprehensive R Archive Network
  (CRAN), with info at the related web page
  <a href="https://CRAN.R-project.org/package=tlm" target="_blank">https://CRAN.R-project.org/package=tlm</a>.

- Once the package has been installed, a summary of the main functions
  is available by executing:

``` r
help(package = "tlm")
```

- A comprehensive tutorial, including a number of detailed examples, is
  available by executing:

``` r
vignette("tlm")
```

### References

The methodology used in the package is described in

- Barrera-Gómez J, Basagaña X. *Models with transformed variables:
  Interpretation and software*. Epidemiology. 2015;26(2):e16-17. DOI:
  10.10.1097/EDE.0000000000000247. URL:
  <https://journals.lww.com/epidem/Fulltext/2015/03000/Models_with_Transformed_Variables__Interpretation.27.aspx>
