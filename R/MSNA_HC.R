#' Data Related to Living Standards in Host Communities Adjacent to the Rohingya Refugee Camps in Bangladesh, 2018
#'
#' Extract from the "COX'S BAZAR HOST COMMUNITY (HC) MULTI-SECTOR NEEDS ASSESSMENT (MSNA) - HOUSEHOLD DATA", December 2018.
#' Probability sample of 2855 households in 11 communes ("Unions") in two sub-districts ("Upazillas") in south-eastern Bangladesh.
#' Basis for the calculation of a living standards deprivation index, on which households differ by gender of household heads and commune of residence.
#'
#' @format A data frame with 2855 obs. in 15 variables:
#' \describe{
#'   \item{id}{Household ID: integer}
#'   \item{sampl_weights}{Sampling weights: float, 11 distinct values (1 per Union), range: [0.5694934, 1.396798]}
#'   \item{upazilla}{Upazilla (sub-district): two distinct values: "Teknaf", "Ukhiya": character string / factor}
#'   \item{union}{Union (commune): 11 distinct values.
#'   "Baharchhara", "Nhilla", "Sabrang", "Teknaf", "Teknaf Paurashava", and "Whykong" Unions in Teknaf Upazilla,
#'   as well as "Haldia Palong", "Jalia Palong", "Palong Khali", "Raja Palong", and "Ratna Palong" Unions in Ukhiya Upazilla: character string / factor}
#'   \item{hhh_gender}{Gender of household head: female or male: character string / factor}
#'   \item{ls_1_food}{Food deprivation indicator, calculated from the original Food Consumption Scores: float, range [0.0669643, 1]}
#'   \item{ls_2_livelihood}{Indicator of less favorable livelihoods combinations: Ordinal,
#'   rescaled to: 0.25 0.50 0.75 1.00. Of nine livelihood types that the MSNA observed,
#'   five - domestic work, non-agricultural work, fishing, small business, and remittances
#'   - are significantly associated with the scores of household food consumption.
#'   Across the sample households, the five types appeared in 23 combinations.
#'   These were mapped to a scale, with four levels, of increasingly less favorable combinations,
#'   reflected in a (nearly linear) decrease in the predicted food consumption scores.
#'   Because of that nearly linear effect, this indicator is treated as interval-level variable.}
#'   \item{ls_3_shelter}{Probability of living in a worse shelter than others:
#'   Four distinct values: 0.0696, 0.3204, 0.6645, 0.9137.
#'   Household dwellings are classified by construction types, which reflect decreasing levels of comfort and value:
#'   Pucca (highest, best), Semi-pucca, Kutcha, and Jhuprie (lowest, worst).
#'   For lack of finer grading or market value data, it is assumed that within each class,
#'   half of the households are slightly worse off than the other half.
#'   Thus, over the four classes,
#'   for a given household one of the noted four distinct values is the probability of occupying a
#'   dwelling worse than the other households in the population,
#'   calculated as the sum of proportions of households in types better than its own
#'   (if any) plus half of the proportion in its own (this measure is known as the "ridit";
#'   see  \url{https://en.wikipedia.org/wiki/Ridit_scoring}).}
#'   \item{water_1}{No year-round access to improved water source: binary / 0 or 1}
#'   \item{water_2}{Problems encountered when collecting water: binary / 0 or 1}
#'   \item{water_3}{Walking to and from the water source takes more than 30 minutes: binary / 0 or 1}
#'   \item{sanit_1}{Trash visible: binary / 0 or 1}
#'   \item{sanit_2}{Faeces visible: binary / 0 or 1}
#'   \item{sanit_3}{Stagnant water visible: binary / 0 or 1}
#'   \item{sanit_4}{Household members defecate outside home: binary / 0 or 1}
#' }
#' @details The continuous indicators ls_1_food, ls_2_livelihood and ls_3_shelter measure deprivation in three living standards (ls) components,
#' each one corresponding to a particular humanitarian sector.
#' The items with prefixes water_ and sanit_ are observed in the water,
#' respectively sanitation sub-sectors of the Water, Sanitation and Hygiene (WASH) sector.
#'
#' This dataset has been chosen to demonstrate a two-level deprivation model using the function mdepriv,
#' with the Betti-Verma double weighting rule operating at both levels.
#' At the lower level, the seven binary WASH items are aggregated to a continuous WASH deprivation indicator, ls_4_WASH.
#' To equalize subsector contributions, water_ and sanit_ items are grouped in different dimensions.
#' At the higher level, ls_4_WASH is combined with the other three ls-indicators and aggregated to a living standards deprivation index.
#'
#' In this humanitarian context, there is no basis to consider one or the other water or sanitation problems more or less important on the basis of their different prevalence.
#' Therefore, in aggregating the seven binary water_ and sanit_ items, \code{mdepriv} takes the arguments \code{wa} = \code{"equal"}.
#' Moreover, \code{wb} = \code{"mixed"} has the effect to reduce weights on more redundant items
#' (\code{wb} = \code{"diagonal"} would be neutral to redundancy; and \code{wb} = \code{"pearson"} would underrate the strength of correlations between binary items).
#' This model returns the deprivation scores in the variable "ls_4_WASH".
#'
#' At the second level, in the aggregation of the four continuous ls-indicators (ls_1_food, ls_2_livelihood, ls_3_shelter, ls_4_WASH),
#' the default Betti-Verma method is used, by setting \code{method} = \code{"bv"} in the function \code{mdepriv}.
#' This activates both mechanisms of the double-weighting scheme - rewarding more discriminating indicators with higher weights,
#' and penalizing redundant ones with lower weights.
#' @source ISCG (Inter Sector Coordination Group) / REACH / ACAPS-NPM, Cox Bazar, Bangladesh, December 2018,
#' published with the provision: "This tool is made available to all staff and partners of the ISCG,
#' and to the general public as a support tool for strategy and programming in the humanitarian response in Bangladesh
#' and other related purposes only.
#' Extracts from the information from this tool may be reviewed, reproduced or translated for the above-mentioned purposes,
#' but are not for sale or for use in conjunction with commercial purposes."
#' Original food consumptions scores calculated by REACH.
#' Indicators ls_1_food, ls_2_livelihood and ls_3_shelter calculated by Aldo Benini, from a cleaned dataset compiled by Sudeep Shrestha, ACAPS.
"MSNA_HC"
