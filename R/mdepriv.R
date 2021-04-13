#' @title Synthetic Indicators of Multiple Deprivation
#'
#' @description calculates synthetic scores of multiple deprivation from unidimensional indicators and/or basic items of deprivation.
#' The scores are weighted sums of the individual indicators / items taking values on the [0,1] range.
#' Several alternative weighting methods are available, notably Betti & Verma's (1998) double-weighting method.
#'
#' @param data a \code{data.frame} or a \code{matrix} with columns containing variables representing deprivation items measured on the [0,1] range.
#' Optionally a column with user defined sampling weights (s. argument \code{sampling_weights}) can be included.
#' @param items a character string vector or \code{list} of such vectors specifying the indicators / items within the argument \code{data}, from which the deprivation scores are derived.
#' By using a \code{list} with more than one vector, the items are grouped in dimensions.
#' By naming the \code{list} elements, the user defines the dimensions' names.
#' Else, dimensions are named by default \code{"Dimension 1"} to \code{"Dimension K"} where K is the number of dimensions.
#' @param sampling_weights a character string corresponding to the column heading of a numeric variable within the argument \code{data} which is specified as sampling weights.
#' By default set to \code{NA}, which means no specification of sampling weights.
#' @param method a character string selecting the weighting scheme.
#'  Available choices are \code{"cz"} (default), \code{"ds"}, \code{"bv"} and \code{"equal"} (s. Details).
#' @param bv_corr_type a character string selecting the correlation type if \code{method = "bv"}.
#' Available choices are \code{"mixed"} (default) and \code{"pearson"}.
#' The option \code{"mixed"} automatically detects the appropriate correlation type
#' \code{"pearson"}, \code{"polyserial"} or \code{"polychoric"} for each pair of items (s. Details).
#' @param rhoH numeric. Permits setting \code{rhoH} if the argument \code{method} is set to \code{"bv"},
#' or the argument \code{wb} is set to either \code{"mixed"} or \code{"pearson"}.
#' \code{rhoH} distributes high and low coefficients of the triangular item correlation table to two factors.
#' By default \code{rhoH} is set to \code{NA}, which causes its automatic calculation according to Betti & Verma's (1998)
#' suggestion to divide the ordered set of correlation coefficients at the point of their largest gap.
#' Alternatively, the user can set a value for \code{rhoH} in the interval [-1, +1].
#' When \code{rhoH} is automatically calculated, the weights of items that overall are more weakly correlated with the other items turn out higher,
#' compared to their weights when the user chooses a \code{rhoH} value far from the automatic version.
#' If the user chooses more than one dimension, \code{rhoH} is common for all.
#' @param wa,wb two single character strings providing alternative, more flexible ways to select the weighting schemes.
#' Weights are computed as the product of two terms as in the Betti-Verma scheme. \code{wa} selects the form of the first factor
#' and is one of \code{"cz"}, \code{"ds"}, \code{"bv"} or \code{"equal"} (s. Details).
#' \code{wb} selects the form of the second factor and is one of \code{"mixed"}, \code{"pearson"} or \code{"diagonal"},
#' where the latter sets all off-diagonal correlations to zero (s. Details).
#' \code{wa} and \code{wb} are set both by default to \code{NA}, which means no specification.
#' Specify either \code{wa} and \code{wb}, or \code{method}, not both.
#' @param user_def_weights a numeric vector or \code{list} of such vectors to pass user-defined weights.
#' To pass these weights correctly to the corresponding items,
#' the structure of the vector, respectively of the \code{list}, must match the argument \code{items}.
#' By creating a \code{list} with more than one vector, the user groups the items in dimensions.
#' The elements of the vector, respectively of each vector within the \code{list},
#' must sum to 1. User-defined names of dimensions are inherited from the argument \code{items}.
#' \code{user_def_weights} is set by default to \code{NA}, which means unspecified.
#' @param score_i_heading a character string (default: \code{"score_i"})
#' giving a heading to the \code{score_i} column in the output \code{"data"}.
#' @param output a character string vector selecting the output.
#' Available multiple choices are \code{"view"} (default), \code{"all"}, \code{"weighting_scheme"},
#' \code{"aggregate_deprivation_level"}, \code{"summary_by_dimension"}, \code{"summary_by_item"},
#' \code{"summary_scores"}, \code{"score_i"}, \code{"sum_sampling_weights"},
#' \code{"data"}, \code{"items"}, \code{"sampling_weights"}, \code{"wa"},
#' \code{"wb"}, \code{"rhoH"}, \code{"user_def_weights"} and  \code{"score_i_heading"} (s. Value).
#'
#' @return a \code{list} or a single object according to the argument \code{output}. Possible \code{output} elements:
#' \itemize{
#'   \item \code{"view"} (default) composes a \code{list} including \code{"weighting_scheme"}, \code{"aggregate_deprivation_level"}, \code{"summary_by_dimension"}, \code{"summary_by_item"} and \code{"summary_scores"}.
#'   \item \code{"all"} delivers a \code{list} with all possible \code{output} elements.
#'   \item \code{"weighting_scheme"} a character string returning the weighting scheme chosen by
#'   the argument \code{method}, the arguments \code{wa} and \code{wb}, or the argument \code{user_def_weights}, respectively.
#'   \item \code{"aggregate_deprivation_level"} a single numeric value in the [0,1] range displaying the aggregate deprivation level.
#'   \item \code{"summary_by_dimension"} a \code{data.frame} containing the variables:
#'   \code{Dimension} (dimension names),
#'   \code{N_Item} (number of items per dimension),
#'   \code{Index} (within-dimension index),
#'   \code{Weight} (dimension weights),
#'   \code{Contri} (dimension contribution),
#'   \code{Share} (dimension relative contribution).
#'   If the user did not specify two or more dimensions (groups of items) \code{"summary_by_dimension"} is dropped from the output, unless it is explicitly requested as an element of the argument \code{output}.
#'   \item \code{"summary_by_item"} a \code{data.frame} containing variables:
#'   \code{Dimension} (dimension names),
#'   \code{Item} (item names),
#'   \code{Index} (item means),
#'   \code{Weight} (item weights),
#'   \code{Contri} (item contributions),
#'   \code{Share} (relative item contributions).
#'   The column \code{Dimension} is dropped unless at least two dimensions (groups of items) are specified or if \code{"summary_by_dimension"} is explicitly requested as an element of the argument \code{output}.
#'   \item \code{"summary_scores"} a \code{data.frame} containing these statistics of \code{"score_i"}:
#'   \code{N_Obs.} (number of observations),
#'   \code{Mean} (mean),
#'   \code{Std._Dev.} (standard deviation),
#'   \code{Min} (minimum),
#'   \code{Max} (maximum).
#'   \item \code{"score_i"} a numeric vector returning the score for each observation.
#'   \item \code{"sum_sampling_weights"} a numeric value equal to the sum of sampling weights.
#'   If the argument \code{sampling_weights} is unspecified, \code{NA} is returned.
#'   \item \code{"data"} a \code{data.frame} including the argument \code{data} as well as a merged column containing the scores
#'   (default heading \code{"score_i"}, which can be alter by the argument \code{score_i_heading}).
#'   \item \code{"items"} a named \code{list} of one or more character vectors returning the argument items grouped as dimensions.
#'   If no dimensions were specified a \code{list} with only one vector is returned.
#'   The list can be re-used as a template for the argument \code{"items"} in the functions \code{mdepriv} and \code{corr_mat}
#'   without needing to prior \code{\link[base]{unlist}}.
#'   \item \code{"sampling_weights"} single character strings returning the specification of the argument \code{sampling_weights}, if unspecified \code{NA}.
#'   \item \code{"wa", "wb"} two single character strings giving the weighting scheme for the 1st, respectively the 2nd weighting factor.
#'   If the argument \code{user_def_weights} is specified, \code{NA}'s are returned.
#'   \item \code{"rhoH"} a numeric value giving the effective rhoH. For weighting schemes not relying on \code{rhoH}, NA is returned.
#'   \item \code{"user_def_weights"} a named \code{list} of one or more numeric vectors returning the argument \code{user_def_weights} grouped as dimensions.
#'   The names of the \code{list}'s elements are identical with those of the output \code{"items"}.
#'   If the argument \code{user_def_weights} is unspecified, NA is returned.
#'   \item \code{"score_i_heanding"} single character strings returning the specification of the argumnent \code{score_i_heanding}.
#' }
#'
#' @details
#' \code{mdepriv} is an adaptation for R of a homonymous community-contributed Stata command developed by
#' \href{http://medim.ceps.lu/stata/mdepriv_v3.pdf}{Pi Alperin & Van Kerm (2009)}
#' for computing synthetic scores of multiple deprivation from unidimensional indicators and/or basic items of deprivation.
#' The underlying literature and algebra are not recapitulated here.
#' They are documented in \href{http://medim.ceps.lu/stata/mdepriv_v3.pdf}{Pi Alperin & Van Kerm (2009)}.
#' There are minor differences vis-a-vis the predecessor in Stata, pointed out in the
#' \href{../doc/what_is_different_from_stata.html}{\code{vignette("what_is_different_from_stata")}}.
#'
#' The scores are weighted sums of the individual indicators / items.
#' Both the items and the scores are limited to the [0, 1] range, and the item weights automatically sum to 1.
#' As customary in deprivation research, all indicators / items require negative orientation
#' (i.e., higher values are less desirable).
#' In preparation, any item with values on [0, max], where max > 1, has to be transformed, using a suitable transformation.
#' The choice of transformation function is dictated by substantive concerns.
#' With \eqn{i} indexing the \eqn{i-th} observation and \eqn{j} indexing the \eqn{j-th} item,
#' and all \eqn{xij} >= 0, plausible functions are:
#' \itemize{
#'   \item \eqn{yij = xij / (theoretical maximum of xj)}, particularly if all \eqn{xj} to be transformed have natural scales.
#'   \item \eqn{yij = xij / (c * mean(xj))} in the absence of natural scales or theoretical maxima, with \eqn{c >} 1 a constant identical for all \eqn{xj} and chosen large enough so that all \eqn{max(yj) <=} 1.
#'   \item the asymptotic \eqn{yij = (xij / mean(xj)) / (1 + (xij / mean(xj)))}, which implies \eqn{mean(yj) =} 0.5  and \eqn{max(yj)} < 1 for all \eqn{yj}
#' }
#' and many others.
#'
#' The multiplicative re-scaling (first two examples) preserves the coefficient of variation,
#' an important aspect of item discrimination; in the third example,
#' \eqn{yj} = 0.5 at the point where \eqn{xj = mean(xj)} for all \eqn{xj}.
#' This ensures the same neutral location for all items thus transformed.
#' Dichotomous items must be coded 0/1, where 1 represents the undesirable state.
#'
#' The transformation of ordinal indicators requires special care. Multiplicative re-scaling,
#' which turns ordinal variables into pseudo-interval level ones in the [0, 1] range, is acceptable
#' if it is unlikely to create significant distortions. Else, a data-driven transformation,
#' such as the ridit, may be appropriate.
#'
#' The available weighting schemes with the argument \code{method}, respectively \code{wa} are:
#' \itemize{
#'   \item \code{"cz"} for Cerioli & Zani (1990) weighting.
#'   \item \code{"ds"} for Desai & Shah (1988) weighting.
#'   \item \code{"bv"} for Betti & Verma (1998) weighting.
#'   \item \code{"equal"} for equi-proportionate weighting of items.
#' }
#'
#' The differences among the four methods are visualized in this graph,
#' as far as the weighting of dichotomous items is concerned.
#' From \eqn{equal} to \eqn{Betti-Verma},
#' the weighting schemes are increasingly sensitive to items with low prevalence in the sample.
#' Thus, in all schemes except "equal",
#' assets that most households own, and which only the most destitute lack (e.g., minimal cooking equipment),
#' are given higher weights than items that most lack,
#' and which only the better-off may own (e.g., a motorcycle) (Items are negatively coded,
#' e.g., 1 = "household lacks the asset").
#'
#' \figure{weightsbyweightingscheme.png}
#'
#' For continuous items (e.g., a food insecurity score),
#' higher values denote less desirable states.
#' Item weights are proportionate to (1 - \eqn{mean(yj)}) for the Desai-Shah scheme,
#' to \eqn{log}(1 / \eqn{mean(yj)}) for Cerioli-Zani, and to the coefficients of variation [i.e., \eqn{std.dev(xj)} / \eqn{mean(xj)}] for Betti-Verma.
#'
#' Differently from the other three methods, Betti-Verma also controls for redundancy among items by lowering the weights of items that are highly correlated with many items.
#'
#' Formulas and literature references are provided in \href{http://medim.ceps.lu/stata/mdepriv_v3.pdf}{Pi Alperin & Van Kerm (2009)}.
#' \code{"cz"} and \code{"ds"} are built on the function \code{\link[Weighted.Desc.Stat]{w.mean}}.
#' Whereas \code{"bv"} relies for its 1st factor (\code{wa}) on \code{\link[Weighted.Desc.Stat]{w.cv}}.
#' \code{"bv"}'s 2nd factor (\code{wb}) as well as any specification of \code{wb} but \code{"diagonal"} rely on \code{\link[wCorr]{weightedCorr}}.
#'
#' When setting the argument \code{bv_corr_type}, respectively \code{wb} to \code{"mixed"},
#' the appropriate correlation type \code{"pearson"}, \code{"polyserial"} or \code{"polychoric"}
#' is automatically detected for each pair of items by the following rules:
#' \itemize{
#'   \item \code{"pearson"}: both items have > 10 distinct values.
#'   \item \code{"polyserial"}: one item has \eqn{\le} 10, the other > 10 distinct values.
#'   \item \code{"polychoric"}: both items have \eqn{\le} 10 distinct values.
#' }
#' When \code{bv_corr_type} respectively \code{wb} is set to \code{"pearson"}
#' this correlation type is forced on all item pairs.
#'
#' @seealso \href{../doc/mdepriv_get_started.html}{\code{vignette("mdepriv_get_started")}}
#'
#' @references Betti, G. & Verma, V. K. (1998), 'Measuring the degree of poverty in a dynamic and comparative context: a multi-dimensional approach using fuzzy set theory', Working Paper 22, Dipartimento di Metodi Quantitativi, Universit`a di Siena.
#'
#' Cerioli, A. & Zani, S. (1990), 'A fuzzy approach to the measurement of poverty', in C. Dagum & M. Zenga (eds.), Income and Wealth Distribution, Inequality and Poverty, Springer Verlag, Berlin, 272-284.
#'
#' Desai, M. & Shah, A. (1988), 'An econometric approach to the measurement of poverty', Oxford Economic Papers, 40(3):505-522.
#'
#' Pi Alperin, M. N. & Van Kerm, P. (2009), 'mdepriv - Synthetic indicators of multiple deprivation', v2.0 (revised March 2014), CEPS/INSTEAD, Esch/Alzette, Luxembourg. \url{http://medim.ceps.lu/stata/mdepriv_v3.pdf} (2019-MM-DD).
#' @export
#'
#' @importFrom Weighted.Desc.Stat w.mean
#' @importFrom Weighted.Desc.Stat w.cv
#' @importFrom wCorr weightedCorr
#' @importFrom stats aggregate
#' @importFrom stats sd
#'
#' @examples head(simul_data, 3) # data used for demonstration
#'
#' # least possible specification: data & items:
#' mdepriv(simul_data, c("y1", "y2", "y3", "y4", "y5", "y6", "y7"))
#' # group items in dimensions:
#' mdepriv(simul_data, list(c("y1", "y2", "y3", "y4"), c("y5", "y6", "y7")))
#' # customized labelling of dimensions:
#' mdepriv(simul_data, list('Group A' = c("y1", "y2", "y3", "y4"), 'Group B' = c("y5", "y6", "y7")))
#'
#' # available outputs
#' no_dim_specified <- mdepriv(simul_data, c("y1", "y2", "y3", "y4", "y5", "y6", "y7"), output = "all")
#' two_dim <- mdepriv(simul_data, list(c("y1", "y2", "y3", "y4"), c("y5", "y6", "y7")), output = "all")
#' length(no_dim_specified)
#' length(two_dim)
#' data.frame(row.names = names(two_dim),
#'            no_or_1_dim_specified = ifelse(names(two_dim) %in% names(no_dim_specified), "X", ""),
#'            at_least_2_dim_speicified = "X")
#' setdiff(names(two_dim), names(no_dim_specified))
#' # if no dimensions are specified, "summary_by_dimension" is dropped from the two output wrappers
#' # (output = "view" (default), output = "all")
#' # however, even if no dimension is specified "summary_by_dimension" is accessible
#' mdepriv(simul_data, c("y1", "y2", "y3", "y4", "y5", "y6", "y7"), output = "summary_by_dimension")
#'
#' # apply sampling weights (3rd argument)
#' with_s_w <- mdepriv(simul_data, c("y1", "y4", "y5", "y6"), "sampl_weights", output = "all")
#' without_s_w <- mdepriv(simul_data, c("y1", "y4", "y5", "y6"), output = "all")
#' # return sum and specification of sampling weights if applied ...
#' with_s_w[c("sum_sampling_weights", "sampling_weights")]
#' # if not, NA's are returned:
#' without_s_w[c("sum_sampling_weights", "sampling_weights")]
#'
#' # weighting schemes
#' # the default weighting scheme is "Cerioli & Zani": method = "cz"
#' mdepriv(simul_data, c("y1", "y2", "y3"), output = "weighting_scheme")
#'
#' methods <- c("cz", "ds", "bv", "equal") # 4 standard weighting schemes availble
#' sapply(X = methods, function(X) mdepriv(simul_data, c("y1", "y2", "y3"),
#'                                         method = X, output = "weighting_scheme"))
#'
#' # alternative, more flexible ways to select (double) weighting schemes
#' mdepriv(simul_data, c("y1", "y2", "y3"), wa = "cz", wb = "mixed", output = "weighting_scheme")
#' # some of the double weighting specification are almost lookalikes of the standard weight methods
#' method_bv_pearson <- mdepriv(simul_data, c("y1", "y2", "y3"),
#'                              method = "bv", bv_corr_type = "pearson", output = "all")
#' method_bv_pearson$weighting_scheme
#' wa_wb_bv_pearson <- mdepriv(simul_data, c("y1", "y2", "y3"),
#'                             wa = "bv", wb = "pearson", output = "all")
#' wa_wb_bv_pearson$weighting_scheme
#' all.equal(method_bv_pearson[-1], wa_wb_bv_pearson[-1])
#'
#' # either a fixed or a data driven rhoH is involved in any true double weighting scheme
#' # (effective single weighting schemes: method: "cs", "ds", "equal" or wb = "diagonal")
#' items_sel <- c("y1", "y2", "y3", "y4", "y5", "y6", "y7") # a selection of items
#' # data driven:
#' mdepriv(simul_data, items_sel, method = "bv", output = "rhoH")
#' mdepriv(simul_data, items_sel, wa = "cz", wb = "pearson", output = "rhoH")
#' # fixed:
#' mdepriv(simul_data, items_sel, method = "bv", rhoH = 0.3, output = "rhoH")
#' mdepriv(simul_data, items_sel, wa = "cz", wb = "pearson", rhoH = 0.3, output = "rhoH")
#'
#' # check how weighting settings are applied:
#' bv <- mdepriv(simul_data, items_sel, method = "bv", output = "all")
#' bv[c("weighting_scheme", "wa", "wb", "rhoH")]
#' ds <- mdepriv(simul_data, items_sel, method = "ds", output = "all")
#' ds[c("weighting_scheme", "wa", "wb", "rhoH")]
#' equal_pearson <- mdepriv(simul_data, items_sel, wa = "equal",
#'                          wb = "pearson", output = "all")
#' equal_pearson[c("weighting_scheme", "wa", "wb", "rhoH")]
#' equal_pearson_rhoH_fixed <- mdepriv(simul_data, items_sel, wa = "equal",
#'                                     wb = "pearson", rhoH = 0.3 , output = "all")
#' equal_pearson_rhoH_fixed[c("weighting_scheme", "wa", "wb", "rhoH")]
#'
#' # pass expertise-base weights to the items
#' dim <- list('Group A' = c("y1", "y2", "y3"), 'Group B' = c("y4", "y5", "y6"))
#' # 'expertise weights' structured as dimensions
#' w_expertise  <- list(c(0.5, 0.25, 0.25), c(0.4, 0.45, 0.15))
#' model_expertise <- mdepriv(simul_data, items = dim,
#'                            user_def_weights = w_expertise, output = "all")
#' # check weighting settings ...
#' model_expertise[c("weighting_scheme", "wa", "wb", "rhoH", "user_def_weights")]
#' # ... wa, wb and rhoH are not involved, when expertise weights are applied,
#' # and therefore returned as NA's.
#' # user-defined names of dimensions are inherited from the argument items.
#'
#' # use outputs elements
#' dim <- list(c("y1", "y2", "y3"), c("y4", "y5", "y6", "y7"))
#' model_1 <- mdepriv(simul_data, items = dim, method = "bv", output = "all")
#'
#' model_1$summary_by_item
#' by_item_no_total <- subset(model_1$summary_by_item, Weight != 1)
#' barplot(Weight ~ Item, data = by_item_no_total)
#'
#' model_1$summary_scores
#' hist(model_1$score_i,
#'      main = 'model: method = "bv", bv_corr_type = "mixed" (default)',
#'      xlab = "scores")
#'
#' # output data ...
#' head(model_1$data, 3)
#' # ... compare to input data ...
#' head(simul_data, 3)
#' # ... only the scores have been merged to the (input) data
#' all.equal(model_1$data[ ,names(model_1$data) != "score_i"], simul_data)
#' # scores are twofold accessible
#' all.equal(model_1$score_i, model_1$data$score_i)
#'
#' # re-use output of a model as arguments in another model:
#' dim <- list(c("y1", "y2", "y3"), c("y4", "y5", "y6", "y7"))
#' model_1 <- mdepriv(simul_data, items = dim, method = "bv", output = "all")
#' model_2 <- mdepriv(simul_data, items = model_1$items, method = "ds", output = "all")
#' all.equal(model_1$items, model_2$items)
#' # how do the scores of the 2 models differ?
#' plot(model_1$score_i, model_2$score_i,
#'      xlab = model_1$weighting_scheme, ylab = model_2$weighting_scheme,
#'      xlim = c(0,1), ylim = c(0,1),
#'      asp = 1, main = "same item grouping")
#' abline(0, 1, col = "red", lty = 2, lwd = 2)
#'
#' # accumulating scores from different models in the output data
#' \dontrun{
#' # this code will throw an error message with a hint on how to handle the re-use ...
#' # ... of 'data' output as agrument. so run it and read!
#' model_3 <- mdepriv(model_1$data, items = model_1$items,
#'                    wa = "cz", wb = "mixed", output = "all")}
#' model_3 <- mdepriv(model_1$data, items = model_1$items,
#'                    wa = "cz", wb = "mixed", output = "all",
#'                    score_i_heading = "score_i_model_3")
#' head(model_3$data, 3)
#'
#' # if gathering scores from iteratered runs is the purpose it's expedient to avoid confusion ...
#' # ... by naming already the 1st scores column with reference to its model
#' model_1 <- mdepriv(simul_data, dim, method = "bv",
#'                    score_i_heading = "score_i_1", output = "all")
#' model_2 <- mdepriv(model_1$data, model_1$items, method = "ds",
#'                    score_i_heading = "score_i_2", output = "all")
#' model_3 <- mdepriv(model_2$data, model_1$items, wa = "cz", wb = "mixed",
#'                    score_i_heading = "score_i_3", output = "all")
#' head(model_3$data, 3)
mdepriv <- function(data,
                    items,
                    sampling_weights = NA,
                    method           = c("cz", "ds", "bv", "equal"),
                    bv_corr_type     = c("mixed", "pearson"),
                    rhoH             = NA,
                    wa               = NA,
                    wb               = NA,
                    user_def_weights = NA,
                    score_i_heading  = "score_i",
                    output           = c("view", "all", "weighting_scheme", "aggregate_deprivation_level",
                                         "summary_by_dimension", "summary_by_item", "summary_scores", "score_i",
                                         "sum_sampling_weights", "data", "items", "sampling_weights", "wa",  "wb",
                                         "rhoH", "user_def_weights", "score_i_heading")
)
{
  # ------------------------------------------------------------------------
  # check if input data is a data.frame or a matrix, otherwise stop
  check_data_(data)
  # in any case including a matrix or a tibble, the input data is turned into an ordinary data.frame
  data <- as.data.frame(data)
  # ------------------------------------------------------------------------
  check_items_(items)

  if (is.list(items)) {
    dim <- items
    items <- unlist(items)
  } else {
    dim <- list(items)
  }

  if (is.null(names(dim))) {
    names(dim) <- paste0("Dimension ", seq_along(dim))
  }

  check_dim_(dim)

  check_items_in_data_(items, data)

  check_items_duplicate_(items)

  check_items_length_(items)

  check_items_numeric_(items, data)

  check_items_NA_(items, data)

  check_items_range_(items, data)
  # ------------------------------------------------------------------------
  if (!is.null(user_def_weights)) {
    if (all(is.na(user_def_weights) & length(user_def_weights) == 1 & class(user_def_weights) == "logical")) {
      user_def_weights <- NULL
    }
  }
  if (!is.null(user_def_weights)) {
    check_user_def_weights_numeric_(user_def_weights)

    if (!is.list(user_def_weights)) {
      user_def_weights <- list(user_def_weights)
    }

    check_user_def_weights_str_(user_def_weights, dim)

    if (is.null(names(user_def_weights))) {
      names(user_def_weights) <- names(dim)
    }

    check_user_def_weights_names_(user_def_weights, dim)
  }
  # ------------------------------------------------------------------------
  if (is.null(sampling_weights)) {
    sampling_weights_arg <- NA_character_
    sampling_weights     <- rep(1, nrow(data))
  } else if (all(is.na(sampling_weights) & length(sampling_weights) == 1 & class(sampling_weights) %in% c("logical", "character"))) {
    sampling_weights_arg <- NA_character_
    sampling_weights     <- rep(1, nrow(data))
  } else {
    check_sampling_weights_class_(sampling_weights)
    check_sampling_weights_length_(sampling_weights)
    check_sampling_weights_in_data_(sampling_weights, data)
    check_sampling_weights_numeric_(sampling_weights, data)
    check_sampling_weights_in_items_(sampling_weights, items)
    sampling_weights_arg <- sampling_weights
    sampling_weights     <- data[[sampling_weights]]
  }

  check_sampling_weights_NA_(sampling_weights, sampling_weights_arg)
  check_sampling_weights_values_(sampling_weights, sampling_weights_arg)
  # ------------------------------------------------------------------------
  if (!is.null(rhoH)) {
    if (all(is.na(rhoH) & length(rhoH) == 1 & class(rhoH) %in% c("logical", "numeric"))) {
      rhoH <- NULL
    }
  }
  if (!is.null(rhoH)) {
    check_rhoH_(rhoH)
  }
  # ------------------------------------------------------------------------
  method       <- match.arg(method)
  bv_corr_type <- match.arg(bv_corr_type)

  if (!is.null(wa)) {
    if (all(is.na(wa) & length(wa) == 1 & class(wa) %in% c("logical", "character"))) {
      wa <- NULL
    }
  }
  if (!is.null(wa)) {wa <- match.arg(wa, c("cz", "ds", "bv", "equal"))}
  if (!is.null(wb)) {
    if (all(is.na(wb) & length(wb) == 1 & class(wb) %in% c("logical", "character"))) {
      wb <- NULL
    }
  }
  if (!is.null(wb)) {wb <- match.arg(wb, c("mixed", "pearson", "diagonal"))}

  check_wa_wb_(wa, wb)
  # ------------------------------------------------------------------------
  check_score_i_heading_class_(score_i_heading)
  check_score_i_heading_length_(score_i_heading)
  check_score_i_heading_in_data_(score_i_heading, data)
  # ------------------------------------------------------------------------
  output_op <- c(
    "view", "all", "weighting_scheme", "aggregate_deprivation_level",
    "summary_by_dimension", "summary_by_item", "summary_scores", "score_i",
    "sum_sampling_weights", "data", "items", "sampling_weights", "wa", "wb",
    "rhoH", "user_def_weights", "score_i_heading"
  )

  check_output_class_(output, output_op)
  check_output_in_output_op_(output, output_op)

  if (length(output) == length(output_op)) {
    if (all(output == output_op)) {
      output <- match.arg(output)
    }
  }
  output_arg <- output
  # ------------------------------------------------------------------------
  w_scheme <- NULL
  # ------------------------------------------------------------------------
  if(is.null(wa) & is.null(wb)){
    if(method == "cz"){
      w_scheme <- "Cerioli & Zani (1990) weighting scheme"
    } else if(method == "ds"){
      w_scheme <- "Desai & Shah (1988) weighting scheme"
    } else if(method == "bv"){
      w_scheme <- "Betti & Verma (1998) weighting scheme"
    } else if(method == "equal"){
      w_scheme <- "Equi-proportionate weighting scheme"
    }
    wa <- method
    if(method == "bv"){wb <- bv_corr_type}else{wb <- "diagonal"}
  }
  # ------------------------------------------------------------------------
  wb_general <- function(data, items, corr_type, sampling_weights, rhoH){
    if(corr_type != "diagonal" & length(items) > 1){
      CORR_MAT <- corr_mat_(data = data, items = items, corr_type = corr_type, sampling_weights = sampling_weights)

      wb_j <- function(x, rhoH){
        sum_l <- 1 + sum(x[x < rhoH])
        sum_h <- sum(x[x >= rhoH])
        1/(sum_l * sum_h)
      }

      apply(CORR_MAT, 2, wb_j, rhoH = rhoH)
    } else{
      rep(1, length(items))
    }
  }
  # ------------------------------------------------------------------------
  if(is.null(user_def_weights )){
    if(is.null(w_scheme)){w_scheme  <- paste0("User-defined weighting scheme: wa = ", dQuote(wa), ", wb = ", dQuote(wb), ".")}
    # ------------------------------------------------------------------------
    if(wa == "cz"){
      WA <- apply(as.matrix(data[ ,items]), 2, function(x){log(1 / Weighted.Desc.Stat::w.mean(x, mu = sampling_weights))})
    } else if(wa == "ds"){
      WA <- apply(as.matrix(data[ ,items]), 2, function(x){(1 - Weighted.Desc.Stat::w.mean(x, mu = sampling_weights))})
    } else if(wa == "bv"){
      WA <- apply(as.matrix(data[ ,items]), 2, Weighted.Desc.Stat::w.cv, mu = sampling_weights)
    } else if(wa == "equal"){
      WA <- rep(1/length(items), length(items))
    }

    if(is.null(rhoH) & wb != "diagonal"){
      corr_mat_all_items <- corr_mat_(data = data, items = items, corr_type = wb, sampling_weights = sampling_weights)
      diag(corr_mat_all_items) <- NA
      corr_values <- unique(sort(corr_mat_all_items))
      if(length(corr_values) == 1){
        rhoH <- corr_values
      }else{
        diff <- diff(corr_values)
        rhoH <- mean(corr_values[which(diff == max(diff)) + c(0, 1)])
      }
    }

    if(wb == "diagonal"){rhoH <- NA_real_}

    WB <- unlist(lapply(X = seq_along(dim),
                        function(X) wb_general(data             = data,
                                               items            = dim[[X]],
                                               corr_type        = wb,
                                               sampling_weights = sampling_weights,
                                               rhoH             = rhoH)))
    w  <- WA * WB
    # ------------------------------------------------------------------------
  }else{
    w_scheme <- "Item-wise user-defined weighting scheme"
    w        <- unlist(user_def_weights)
    wa       <- NA_character_
    wb       <- NA_character_
    rhoH     <- NA_real_
  }
  # ------------------------------------------------------------------------
  w <- w / sum(w)
  # normalize weights
  names(w) <- items
  # make sure the weigths are all labeled
  # ------------------------------------------------------------------------
  K         <- length(dim)
  Dimension <- unlist(lapply(X = seq_along(dim), function(X) rep(names(dim)[X], length(dim[[X]]))))
  Index     <- apply(as.matrix(data[ ,items]), 2, function(x) Weighted.Desc.Stat::w.mean(x, mu = sampling_weights))
  Weight    <- unlist(lapply(X = seq_along(dim), function(X) {w[dim[[X]]] / sum(w[dim[[X]]]) / K}))
  Contri    <- Index * Weight
  aggregate_deprivation_level <- sum(Contri)
  Share     <- Contri / aggregate_deprivation_level
  summary_by_item <- data.frame(Dimension, Item = items, Index, Weight, Contri, Share, stringsAsFactors = F, row.names = NULL)

  summary_by_dimension <- stats::aggregate(cbind(Weight, Contri, Share) ~ Dimension, data = summary_by_item, sum)
  summary_by_dimension <- data.frame(Dimension = summary_by_dimension$Dimension,
                                     N_Item    = lengths(dim),
                                     Index     = summary_by_dimension$Contri / summary_by_dimension$Weight,
                                     summary_by_dimension[ ,c("Weight", "Contri", "Share")],
                                     stringsAsFactors = F, row.names = NULL)
  sum_up_col <- c("N_Item", "Weight", "Contri", "Share")
  summary_by_dimension[nrow(summary_by_dimension) + 1, sum_up_col] <- colSums(summary_by_dimension[ ,sum_up_col])
  summary_by_dimension[nrow(summary_by_dimension), "Dimension"] <- "Total"

  sum_up_col <- sum_up_col[-1]
  summary_by_item[nrow(summary_by_item) + 1, sum_up_col] <- colSums(summary_by_item[ ,sum_up_col])
  summary_by_item[nrow(summary_by_item), "Dimension"] <- "Total"

  score_i <- as.matrix(data[, items]) %*% Weight
  score_i <- as.vector(score_i)

  data[ ,score_i_heading] <- score_i

  summary_scores <- data.frame(N_Obs.    = nrow(data),
                               Mean      = mean(score_i),
                               Std._Dev. = stats::sd(score_i),
                               Min       = min(score_i),
                               Max       = max(score_i))

  if(!is.na(sampling_weights_arg)){
    sum_sampling_weights <- sum(sampling_weights)
  } else{
    sum_sampling_weights <- NA_real_
    }

  if(is.null(user_def_weights)){user_def_weights <- NA}

  output <- list(w_scheme,
                 aggregate_deprivation_level,
                 summary_by_dimension,
                 summary_by_item,
                 summary_scores,
                 score_i,
                 sum_sampling_weights,
                 data,
                 dim,
                 sampling_weights_arg,
                 wa,
                 wb,
                 rhoH,
                 user_def_weights,
                 score_i_heading)
  names(output) <- output_op[-c(1,2)]

  if(K == 1 & !"summary_by_dimension" %in% output_arg){
    output[["summary_by_dimension"]] <- NULL
    output[["summary_by_item"]][ ,"Dimension"] <- NULL
    output[["summary_by_item"]][nrow(output[["summary_by_item"]]), "Item"] <- "Total"
  }
  if("all" %in% output_arg){all <- names(output)}else{all <- NULL}
  if("view" %in% output_arg){
    view <- c("weighting_scheme", "aggregate_deprivation_level", "summary_by_dimension", "summary_by_item", "summary_scores")
  }else{view <- NULL}
  output_arg <- c(all, view, output_arg[!output_arg %in% c("all", "view")])

  output <- output[names(output) %in% output_arg]
  if(length(output) == 1){output <- output[[1]]}
  output
  # ------------------------------------------------------------------------
} # end of function
