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
