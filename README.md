
# mdepriv: Synthetic scores of multiple deprivation

## Purpose

`mdepriv` is a R package for combining binary, continuous and suitably
transformed ordinal items/indicators of deprivation into synthetic
measures of deprivation. As such, it is a tool of poverty analysis. It
is suitable also for generating composite measures of severity and
vulnerability in the humanitarian realm. The R implementation translates
the original Stata version by [Pi Alperin & Van Kerm
(2009)](http://medim.ceps.lu/stata/mdepriv_v3.pdf), with additional
features (notably, non-integer sampling weights are admitted).

mdepriv returns unit-level synthetic scores of multiple deprivation and
their statistical summaries. It offers several methods for determining
item/indicator weights in response to user preferences for rewarding
better discrimination and penalizing redundancy.

mdepriv is particularly appropriate in situations where the underlying
concept of deprivation / severity / vulnerability is intuitively
multi-dimensional, but the structure of dimensions is poorly understood;
plausibly they overlap, i.e. they reinforce each other to unknown
degrees.

Also, the measures produced under mdepriv do not presume normative
standards (e.g., poverty lines). There are no a-priori cut-offs of the
kind that are fundamental to multi-dimensional poverty measures in the
Alkire-Foster tradition (implemented in R for example in the functions
`svyafc` and `svyafcdec` of the package
[`convey`](https://CRAN.R-project.org/package=convey)). Shortfall
indicators (e.g., years of basic education missed) can be used the same
way as non-normative ones (e.g., workdays lost to illness).

## Installation

``` r
# install package devtools if not yet installed
#   install.packages("devtools")

# install fast from GitHub without vignettes (not recommanded)
#   devtools::install_github("a-benini/mdepriv")

# recommended: installation at a slower pace from GitHub including vignettes:
 pkgs_4_vignettes <- c("tidyverse", "kableExtra", "knitr", "rmarkdown")
 # packages required to build mdepriv's vignettes from GitHup repository
 new_pkgs_4_vignettes <- pkgs_4_vignettes[!pkgs_4_vignettes %in% installed.packages()]
 # among these packages the ones not yet installed
 if(length(new_pkgs_4_vignettes) > 0){install.packages(new_pkgs_4_vignettes)}
 # install the still missing packages
 devtools::install_github("a-benini/mdepriv", build_vignettes = TRUE)
 # install mdepriv from GitHup with its vignettes
```

## Usage

``` r
library(mdepriv)
```

## Usage Example

``` r
head(simul_data, 3) # demo dataset included in the mdepriv package
#>   id y1 y2 y3  y4    y5    y6    y7 sampl_weights
#> 1  1  0  0  0 0.0 0.369 0.174 0.196         0.556
#> 2  2  1  0  1 0.2 0.762 0.832 1.000         1.500
#> 3  3  0  1  1 0.4 0.708 0.775 0.833         0.973

mdepriv(data = simul_data,                  # a dataset ...
        items = c("y1", "y3", "y4", "y7"),  # from which items/indicators ...
        sampling_weights = "sampl_weights", # and optionally sampl. weights are selected ....
        method = "ds")                      # to be analyzed according to a chosen standard method (= weighting scheme)
#> $weighting_scheme
#> [1] "Desai & Shah (1988) weighting scheme"
#> 
#> $aggregate_deprivation_level
#> [1] 0.3243723
#> 
#> $summary_by_item
#>    Item     Index    Weight     Contri     Share
#> 1    y1 0.1501043 0.3352740 0.05032605 0.1551491
#> 2    y3 0.5402995 0.1813465 0.09798143 0.3020648
#> 3    y4 0.2683779 0.2886164 0.07745828 0.2387944
#> 4    y7 0.5062895 0.1947631 0.09860649 0.3039918
#> 5 Total        NA 1.0000000 0.32437225 1.0000000
#> 
#> $summary_scores
#>   N_Obs.      Mean Std._Dev. Min Max
#> 1    100 0.3266375 0.2320696   0   1
```

The above example demonstrates a simple case with four specified
arguments of the `mdepriv` package’s core function and four default
output elements, which are gathered in a `list`. However, this function
offers several more arguments and outputs. The vignette [Get Started
with
mdepriv](https://a-benini.github.io/mdepriv/articles/mdepriv_get_started.html)
walks one through these manifold options.

## References

Alkire, S., J. Foster, S. Seth, J. M. Roche and M. E. Santos (2015).
Multidimensional poverty measurement and analysis, Oxford University
Press, USA.

Pessoa, Djalma, et.al., convey - Income Concentration Analysis with
Complex Survey Samples, version 0.2.1.,
<https://CRAN.R-project.org/package=convey> (2020-01-02).

Pi Alperin, M. N. and Van Kerm, P. (2009), ‘mdepriv - Synthetic
indicators of multiple deprivation’, v2.0 (revised March 2014),
CEPS/INSTEAD, Esch/Alzette, Luxembourg.
<http://medim.ceps.lu/stata/mdepriv_v3.pdf> (2020-01-02).
