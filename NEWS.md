# mdepriv 0.0.2
* more efficient code for matrix filling within functions `mdepriv` and `corr_mat`
* code related to filling and to processing of matrices implemented as helper functions in a separate R file (`utils.R`)
* code to execute error / stop actions due to incorrect arguments of the functions `mdepriv` and `corr_mat` gathered as helper functions in a separate R file (`checks.R`)
* overall tidied up function code
* section on handling of missing values added to vignette `mdepriv_get_started`
* minor formal changes in vignette `mdepriv_get_started`
* no change in functionality of the package `mdepriv`

# mdepriv 0.0.1
* fixed bug: `tibble` in addition to ordinary `data.frame` or `matrix` as argument `data`: enabled in functions `mdepriv` and `corr_mat`.

# mdepriv 0.0.0
* 1st release on GitHub
* 2 functions: `mdepriv`, `corr_mat`
* 2 datasets: `MSNA_HC`, `simul_data`
* 2 vignettes: `mdepriv_get_started`, `what_is_different_from_stata`
