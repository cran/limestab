# limestab

An R package providing LIME, OptiLIME, BayLIME and ALIME for tabular data,
together with a common set of stability diagnostics (CSI, VSI, MRMS, PCSI,
ICSI) for repeated explanations of the same instance.

## Installation

```r
# install.packages("devtools")
devtools::install_local("path/to/limestab")
```

## Building the package yourself

This project ships as a documented R source package. To regenerate the documentation and run package checks from the package root, use:

```r
install.packages(c("devtools", "roxygen2", "testthat"))
devtools::document()   # regenerates NAMESPACE and man/*.Rd from roxygen comments
devtools::check()      # runs R CMD check
devtools::build()      # builds the source tarball for CRAN-style submission
```

## Quick example

```r
library(limestab)

set.seed(1)
x <- matrix(rnorm(300 * 3), 300, 3, dimnames = list(NULL, c("a", "b", "c")))
predict_fn <- function(m) 2 * m[, "a"] - m[, "b"]

ex <- lime_explainer(x, feature_names = c("a", "b", "c"))
expl <- lime_explain_instance(ex, x[1, ], predict_fn, num_features = 3)
expl$local_exp

stab <- lime_check_stability(ex, x[1, ], predict_fn, num_features = 3, n_calls = 10)
stab$csi
stab$vsi
```

A full LIME vs. OptiLIME vs. BayLIME vs. ALIME comparison on the Pima Indians
Diabetes data set is bundled as an example:

```r
source(system.file("examples", "diabetes_four_methods.R", package = "limestab"))
```

## License

MIT
