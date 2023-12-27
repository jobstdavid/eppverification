# eppverification 0.4.0 (December 27, 2023)

**Note**: Most of the following changes serve as preparation for the next version with groundbreaking innovations!

- change `na.rm` and `mean` argument in nearly all functions to `na.action` and `aggregate`. This allows a more flexible handling of missing values and score aggregation. 
- expand functionality of `bh.test`.
- change test statistic in `dm.test` back to the one in version 0.2.0. 
- change argument name `interval.range` to `nominal.coverage` in the functions `cpi`, `cpi.plot` and `cov.plot`.  
- change argument name `ri` to `reliability` and `ent` to `entropy` in the functions `vr.hist` and `mvr.hist`.  
- changed name `var.pit` and `m.pit` to `dispersion` and `bias`, respectively.
- add weight matrix symmetry check for `vs`.
- remove function `line.plot`.


# eppverification 0.3.0 (September 30, 2023)

- univariate and multivariate score calculation is speeded-up using C++ code by the R-packages `Rfast`, `Rcpp`, `RcppArmadillo`.
- new function `bh.test` to perform a Benjamini-Hochberg-Correction for different p-values.
- new function `ss` to calculate the skill score.
- fast calculation of the sample covariance matrix for the determinate sharpness `ds`.
- change test statistic in `dm.test`. 
- better handling of NA values in the verification functions.
- changed `stat` to `after_stat` in `ggplot2` based functions.
- removed the functions `map.plot` and `box.plot`. 


# eppverification 0.2.0 (Januar 9, 2022)

- new function `dm.test` to perform a Diebold-Mariano-Test for two forecasts.
- new function `m.pit` to calculate mean of PIT values.
- new functions `ent` and `ment` to calculate the entropy of a (multivariate) forecast.
- add `m.pit` to `pit.hist`.
- add `ent` and `ment` to `vr.hist` and `mvr.hist`.


# eppverification 0.1.0 (October 17, 2021)

-   First release.
