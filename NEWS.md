# mdepriv 0.0.6
* minor updates

# mdepriv 0.0.5
* improve installation instruction
* streamline suggested packages

# mdepriv 0.0.4
* minor fixes and improvements

# mdepriv 0.0.3
* some minor adjustments of code in vignette `mdepriv_get_started`
* few code-simplifications of functions `mdepriv` and `corr_mat` with no effect on their functionality

# mdepriv 0.0.2
* more efficient code for matrix filling within functions `mdepriv` and `corr_mat`
* code related to filling and to processing of matrices implemented as helper functions in a separate R file (`utils.R`)
* code to execute error / stop actions due to incorrect arguments of the functions `mdepriv` and `corr_mat` gathered as helper functions in a separate R file (`checks.R`)
* overall tidied up function code
* section on handling of missing values added to vignette `mdepriv_get_started`
* minor formal changes in vignette `mdepriv_get_started`
* internal trimming of `mdepriv()`'s output `score_i` to the range [0, 1] added. This was done due to rarely observed very minor overshooting of 1 by some values of `score_i` (difference < 1e-15). If in such a case the output  `score_i` would have been used as one of the `items` in a higher-level model with `mdepriv()`, that run would fail because `items` are restricted to the range [0, 1].
* besides the trimming of `mdepriv()`'s output `score_i`, no change in functionality of the package `mdepriv`

# mdepriv 0.0.1
* fixed bug: `tibble` in addition to ordinary `data.frame` or `matrix` as argument `data`: enabled in functions `mdepriv` and `corr_mat`.

# mdepriv 0.0.0
* 1st release on GitHub
* 2 functions: `mdepriv`, `corr_mat`
* 2 datasets: `MSNA_HC`, `simul_data`
* 2 vignettes: `mdepriv_get_started`, `what_is_different_from_stata`
