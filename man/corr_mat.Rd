% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corr_mat.R
\name{corr_mat}
\alias{corr_mat}
\title{Correlation Matrix Used in Certain Weighting Schemes}
\usage{
corr_mat(
  data,
  items,
  sampling_weights = NA,
  corr_type = c("mixed", "pearson"),
  output = c("numeric", "type", "both")
)
}
\arguments{
\item{data}{a \code{data.frame} or a \code{matrix} with columns containing variables representing deprivation items measured on the [0,1] range.
Optionally a column with user defined sampling weights (s. argument \code{sampling_weights}) can be included.}

\item{items}{a character string vector or list of such vectors specifying the indicators / items within the argument \code{data} from which the deprivation scores are derived.
If the user inputs a list with more than one vector, the items are grouped in dimensions.
By naming the list elements, the user defines the dimensions' names.
Else, dimensions are named by default \code{"Dimension 1"} to \code{"Dimension K"} where K is the number of dimensions.}

\item{sampling_weights}{a character string corresponding to column heading of a numeric variable within the argument \code{data} which is specified as sampling weights.
By default set to \code{NA}, which means no specification of sampling weights.}

\item{corr_type}{a character string selecting the correlation type.
Available choices are \code{"mixed"} (default) and \code{"pearson"}.
The option \code{"mixed"} detects automatically the appropriate correlation type \code{"pearson"}, \code{"polyserial"} or \code{"polychoric"} for each pair of items (s. Details).}

\item{output}{a character string vector selecting the output. Available choices are \code{"numeric"} (default), \code{"type"} or \code{"both"}.
\code{"numeric"} delivers a numeric correlation matrix or list of such matrices.
(the latter if the \code{items} are grouped in more than one dimension).
\code{"type"} delivers the correlation type applied to each pair of items in a matrix, respectively list of matrices analog to the numeric output.
\code{"both"} combines the \code{"numeric"} and \code{"type"} as a list.}
}
\value{
Either a single \code{matrix} or a \code{list} composed of several \code{matrix}es (s. argument \code{"output"}).
}
\description{
Returns the item correlation matrix (and when K dimensions have been specified, K matrices) that the function \code{\link{mdepriv}} internally generates
\itemize{
  \item when the argument \code{method} = \code{"bv"} and the argument \code{bv_corr_type} = \code{"mixed"} or \code{"pearson"}
  \item or when the argument \code{wa} is specified and the argument \code{wb} = \code{"mixed"} or \code{"pearson"}.
}
Permits inspection of the correlation structure and can be used for complementary analytic purposes such as factor analysis or biplots.
}
\details{
The calculation of the correlation coefficient for a pair of items is based on the function \code{\link[wCorr]{weightedCorr}}.

When setting the argument \code{corr_type} to \code{"mixed"}
the appropriate correlation type \code{"pearson"}, \code{"polyserial"} or \code{"polychoric"}
is automatically detected for each pair of items by the following rules:
\itemize{
  \item \code{"pearson"}: both items have > 10 distinct values.
  \item \code{"polyserial"}: one item has \eqn{\le} 10, the other > 10 distinct values.
  \item \code{"polychoric"}: both items have \eqn{\le} 10 distinct values.
}
When the argument \code{corr_type} is set to \code{"pearson"} this correlation type is forced on all item pairs.

Depending on the correlation type(s) used, the matrix may not be positive semidefinite and therefore not immediately suitable for purposes such as factor analysis.
This is more likely to happen when some of the items are binary.
The function \code{\link[Matrix]{nearPD}} will produce the nearest positive definite matrix.
}
\examples{
head(simul_data, 3) # data used for demonstration

corr_mat(simul_data, c("y1", "y4", "y5", "y6")) # default output: numeric
corr_mat(simul_data, c("y1", "y4", "y5", "y6"), output = "type")
corr_mat(simul_data, c("y1", "y4", "y5", "y6"), output = "both")

# with sampling weights (3rd argument)
corr_mat(simul_data, c("y1", "y4", "y5", "y6"), "sampl_weights")

# choose correlation type
corr_mat_default <- corr_mat(simul_data, c("y1", "y4", "y5", "y6"))
corr_mat_mixed <- corr_mat(simul_data, c("y1", "y4", "y5", "y6"), corr_type = "mixed")
all.equal(corr_mat_default, corr_mat_mixed) # "mixed is corr_type's default
# force a correlation type on all pairs of items
corr_mat(simul_data, c("y1", "y4", "y5", "y6"), corr_type = "pearson")

# grouping items in dimensions
corr_mat(simul_data, list(c("y1", "y4", "y5", "y6"), c("y2", "y3", "y7")))
# customized group / dimension labels
corr_mat(simul_data, list("Group A" = c("y1", "y4", "y5", "y6"),
                          "Group B" = c("y2", "y3", "y7")))

# mdepriv output / returns as template for corr_mat arguments
# items grouped as dimensions
dim <- list("Group X" = c("y1", "y4", "y5", "y6"), "Group Z" = c("y2", "y3", "y7"))

# model: betti-verma ("bv"): correlation type = pearson, rhoH = NA (data driven)
bv_pearson <- mdepriv(simul_data, dim, "sampl_weights",
                      method = "bv", bv_corr_type = "pearson", output = "all")
# use model output as arguments
corr_mat(bv_pearson$data, bv_pearson$items, bv_pearson$sampling_weights,
         corr_type = bv_pearson$wb, output = "both")

# model: user defined double weighting
# 1st factor = wa = "equal", 2nd factor = wa = "mixed" (correlation type),
# rhoH = NA (data driven)
eq_mixed <- mdepriv(simul_data, dim, "sampl_weights",
                    wa = "equal", wb = "mixed", output = "all")
# use model output as arguments
corr_mat(eq_mixed$data, eq_mixed$items, eq_mixed$sampling_weights,
         corr_type = eq_mixed$wb, output = "both")

# model: user defined double weighting
# 1st factor = wa = "bv", 2nd factator = wb = "diagonal"
# (all off-diagonal correlations = 0), rhoH = NA (irrelvant)
bv_diagonal <- mdepriv(simul_data, dim, "sampl_weights",
                       wa = "bv", wb = "diagonal", output = "all")
# use model output as arguments
try(
corr_mat(bv_diagonal$data, bv_diagonal$items, bv_diagonal$sampling_weights,
         corr_type = bv_diagonal$wb, output = "both")
)
# triggers an error because:
bv_diagonal$wb
# if corr_type is left as the default or set to a valid option, then ...
corr_mat(bv_diagonal$data, bv_diagonal$items, bv_diagonal$sampling_weights)
# ...  it works
# for the arguments data, items and sampling_weights the ...
# ... corresponding mdepriv outputs are always valid

# plot unique correlation values and their relation to rhoH
items_sel <- c("y1", "y4", "y5", "y6", "y2", "y3", "y7") # a selection of items
corr_mat  <- corr_mat(simul_data, items_sel) # corr_type default: "mixed"
rhoH      <- mdepriv(simul_data, items_sel, method = "bv", output = "rhoH")
             # bv_corr_type default: "mixed"

corr_val       <- unique(sort(corr_mat)) # sorted unique correlation values
dist_rhoH_corr <- abs(corr_val - rhoH) # distance of corr. values to rhoH
bounding_rhoH  <- abs(dist_rhoH_corr - min(dist_rhoH_corr)) < 10^-10
# TRUE if one of the two corr. values bounding rhoH else FALSE
corr_val_col   <- ifelse(bounding_rhoH, "black", "gray") # colors for corr. values

barplot(corr_val,
  col = corr_val_col,
  border = NA,
  ylim = c(-0.2, 1),
  ylab = "correlation value [-1,+1]",
  main = "sorted unique correlation values and rhoH"
)

abline(h = rhoH, col = "red", lwd = 1.5)
text(0, rhoH + 0.05, paste0("rhoH = ", round(rhoH, 4)), adj = 0, col = "red")

legend("left",
  "correlation values\nbounding largest gap",
  col = "black", pch = 15, pt.cex = 2, bty = "n"
)
}
