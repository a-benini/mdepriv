check_data_ <- function(data) {
  if (!(is.data.frame(data) | is.matrix(data))) {
    stop(
      paste0(
        "The argument ", sQuote("data"), " is neither of the class ",
        dQuote("data.frame"), " nor of the class ", dQuote("matrix"), "."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_items_ <- function(items) {
  if (!(
    (is.vector(items) & is.character(items)) |
      (is.list(items) & all(vapply(items, is.character, logical(1))))
  )
  ) {
    stop(paste0(
      "The argument ", sQuote("items"),
      " is neither a vector nor a list of vectors constisting fully of character elements."
    ),
    call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_dim_ <- function(dim) {
  if (any(names(dim) == "")) {
    stop(
      paste0(
        "The argument ", sQuote("items"),
        " has been labelled with dimension names, but incompletely. Either all dimensions or strictly none must be labelled."
      ),
      call. = FALSE
    )
  } else if (any(duplicated(names(dim)))) {
    stop(paste0(
      "The argument ", sQuote("items"),
      " contains duplicates among the labelled dimension names. If labelled, dimensions names must be unique."
    ),
    call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_items_in_data_ <- function(items, data) {
  items_check <- !items %in% names(data)
  if (sum(items_check) > 0) {
    items_false <- items[items_check]
    stop(
      paste0(
        "The argument ",
        sQuote("data"),
        " does not contain the following variable",
        if (length(items_false) == 1) {
          ": "
        } else {
          paste0(
            "s: ",
            paste0(dQuote(items_false[-length(items_false)]), collapse = ", "),
            " and "
          )
        },
        dQuote(items_false[length(items_false)]),
        ". The argument ",
        sQuote("items"),
        " may only include variables that are part of the argument ",
        sQuote("data"),
        "."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_items_duplicate_ <- function(items) {
  items_duplicated <- unique(items[duplicated(items)])
  if (length(items_duplicated) > 0) {
    stop(
      paste0(
        "The argument ",
        sQuote("items"),
        " contains repeatedly the following variable",
        if (length(items_duplicated) == 1) {
          ": "
        } else {
          paste0(
            "s: ",
            paste0(dQuote(items_duplicated[-length(items_duplicated)]), collapse = ", "),
            " and "
          )
        },
        dQuote(items_duplicated[length(items_duplicated)]),
        "."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_items_length_ <- function(items) {
  if (length(items) == 1) {
    stop(
      paste0(
        "The argument ",
        sQuote("items"),
        " has a length of 1. The argument ",
        sQuote("items"),
        " has to include at least 2 variables contained in the argument ",
        sQuote("data"),
        "."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_items_numeric_ <- function(items, data) {
  items_check_numeric <- vapply(items, function(x) !is.numeric(data[[x]]), logical(1))
  if (sum(items_check_numeric) > 0) {
    items_not_numeric <- items[items_check_numeric]
    stop(
      paste0(
        "The argument ",
        sQuote("items"),
        " contains the following non-numeric variable",
        if (length(items_not_numeric) == 1) {
          ": "
        } else {
          paste0(
            "s: ",
            paste0(dQuote(items_not_numeric[-length(items_not_numeric)]), collapse = ", "),
            " and "
          )
        },
        dQuote(items_not_numeric[length(items_not_numeric)]),
        ". The argument ",
        sQuote("items"),
        " only allows numeric variables contained in the argument ",
        sQuote("data"),
        "."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_items_NA_ <- function(items, data) {
  items_check_NA <- vapply(items, function(x) anyNA(data[[x]]), logical(1))
  if (sum(items_check_NA) > 0) {
    items_NA <- items[items_check_NA]
    stop(
      paste0(
        "The argument ",
        sQuote("items"),
        " contains the following variable",
        if (length(items_NA) == 1) {
          " with NA: "
        } else {
          paste0(
            "s with NA: ",
            paste0(dQuote(items_NA[-length(items_NA)]), collapse = ", "),
            " and "
          )
        },
        dQuote(items_NA[length(items_NA)]),
        ". The argument ",
        sQuote("items"),
        " does not allow any NA-values."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_items_range_ <- function(items, data) {
  check_range <- function(x) {
    any(x < 0 | x > 1)
  }
  items_check_range <- vapply(items, function(x) check_range(data[[x]]), logical(1))
  if (sum(items_check_range) > 0) {
    items_outside_range <- items[items_check_range]
    stop(
      paste0(
        "The argument ",
        sQuote("items"),
        " contains the following variable",
        if (length(items_outside_range) == 1) {
          " with values outside of the [0,1] range: "
        } else {
          paste0(
            "s with values outside of the [0,1] range: ",
            paste0(dQuote(items_outside_range[-length(items_outside_range)]), collapse = ", "),
            " and "
          )
        },
        dQuote(items_outside_range[length(items_outside_range)]),
        ". All variables in ",
        sQuote("items"),
        " must be within the range [0,1]."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_user_def_weights_numeric_ <- function(user_def_weights) {
  if (!(
    (is.vector(user_def_weights) & is.numeric(user_def_weights)) |
    (is.list(user_def_weights) & all(vapply(user_def_weights, is.numeric, logical(1))))
  )
  ) {
    stop(paste0(
      "The argument ",
      sQuote("user_def_weights"),
      " is neither a vector nor a list of vectors consisting entirely of numeric elements."
    ),
    call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_user_def_weights_str_ <- function(user_def_weights, dim) {
  if (any(length(user_def_weights) != length(dim))) {
    stop(
      paste0(
        "The number of dimension in the argument ",
        sQuote("user_def_weights"),
        " does not match the number of dimensions specified in the argument ",
        sQuote("items"),
        "."
      ),
      call. = FALSE
    )
  } else if (any(lengths(user_def_weights) != lengths(dim))) {
    stop(
      paste0(
        "The number of elements per dimension in the argument ",
        sQuote("user_def_weights"),
        " does not match the number of elements per dimension in the argument ",
        sQuote("items"),
        "."
      ),
      call. = FALSE
    )
  } else if (anyNA(unlist(user_def_weights))) {
    stop(
      paste0(
        "At least for one of the items the argument ",
        sQuote("user_def_weights"),
        " is specified with a NA-value. If specified, the argument ",
        sQuote("user_def_weights"),
        " has be numeric and defined for each item."
      ),
      call. = FALSE
    )
  } else if (any(unlist(lapply(user_def_weights, sum)) != 1)) {
    stop(
      paste0(
        "At least for one dimension in the argument ",
        sQuote("user_def_weights"),
        " the elements do not sum up to 1."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_user_def_weights_names_ <- function(user_def_weights, dim) {
  if (any(names(user_def_weights) != c(names(dim)))) {
    stop(
      paste0(
        "The labelling of the dimensions in the argument ",
        sQuote("user_def_weights"),
        " does not match the labelling of the dimensions in the argument ",
        sQuote("items"),
        "."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_sampling_weights_class_ <- function(sampling_weights) {
  if (class(sampling_weights) != "character") {
    stop(
      paste0(
        "The argument ",
        sQuote("sampling_weights"),
        " is not of the class ",
        dQuote("character"),
        ". The argument ",
        sQuote("sampling_weights"),
        " accepts only a single character string specifying a numeric variable within the argument ",
        sQuote("data"),
        "."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_sampling_weights_length_ <- function(sampling_weights) {
  if (length(sampling_weights) != 1) {
    stop(paste0(
      "The argument ",
      sQuote("sampling_weights"),
      " is a character vector with a length of ",
      length(sampling_weights),
      ". The argument ",
      sQuote("sampling_weights"),
      " accepts only a single character string specifying a numeric variable within the argument ",
      sQuote("data"),
      "."
    ),
    call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_sampling_weights_in_data_ <- function(sampling_weights, data) {
  if (!sampling_weights %in% names(data)) {
    stop(
      paste0(
        "The argument ",
        sQuote("sampling_weights"),
        " is a character string, as required; however, it does not match any variable within the argument ",
        sQuote("data"),
        "."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_sampling_weights_numeric_ <- function(sampling_weights, data) {
  if (!is.numeric(data[[sampling_weights]])) {
    stop(
      paste0(
        "The argument ",
        sQuote("sampling_weights"),
        ", which is specified as variable ",
        dQuote(sampling_weights),
        " and included in the argument ",
        sQuote("data"),
        ", is not a numeric vector."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_sampling_weights_in_items_ <- function(sampling_weights, items) {
  if (sampling_weights %in% items) {
    stop(
      paste0(
        "The argument ",
        sQuote("sampling_weights"),
        ", which is specified as variable ",
        dQuote(sampling_weights),
        " is already among the variables selected with the argument ",
        sQuote("items"),
        "."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_sampling_weights_NA_ <- function(sampling_weights, sampling_weights_arg) {
  if (anyNA(sampling_weights) & !is.na(sampling_weights_arg)) {
    stop(
      paste0(
        "The argument ",
        sQuote("sampling_weights"),
        ", which is specified as variable ",
        dQuote(sampling_weights_arg),
        " and included in the argument ",
        sQuote("data"),
        ", is a numeric vector including NA-values. NA is not valid as sampling weight."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_sampling_weights_values_ <- function(sampling_weights, sampling_weights_arg) {
  if (any(sampling_weights <= 0) & !is.na(sampling_weights_arg)) {
    stop(
      paste0(
        "The argument ",
        sQuote("sampling_weights"),
        ", which is specified as variable ",
        dQuote(sampling_weights_arg),
        " and included in the argument ",
        sQuote("data"),
        " contains values =< 0. Only positive values are valid as sampling weights."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_rhoH_ <- function(rhoH) {
  if (!is.numeric(rhoH)) {
    stop(
      paste0("The argument ", sQuote("rhoH"), " is not numeric."),
      call. = FALSE
    )
  } else if (length(rhoH) > 1) {
    stop(
      paste0(
        "The argument ", sQuote("rhoH"), " has a length of ", length(rhoH),
        ". It must be NA or a single numeric value in the [-1,+1] range."
      ),
      call. = FALSE
    )
  } else if (rhoH < -1 | rhoH > 1) {
    stop(
      paste0(
        "The argument ", sQuote("rhoH"), " is outside the allowed [-1,+1] range."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
check_wa_wb_ <- function(wa, wb) {
  if (!is.null(wa) & is.null(wb)) {
    stop(
      paste0(
        "The argument ", sQuote("wa"), " is specified as ", dQuote(wa),
        ", whereas the argument ", sQuote("wb"),
        " is unspecified. Possible options for ", sQuote("wb"), " are: ",
        dQuote("mixed"), ", ", dQuote("pearson"), " or ", dQuote("diagonal"), "."
      ),
      call. = FALSE
    )
  }
  if (is.null(wa) & !is.null(wb)) {
    stop(
      paste0(
        "The argument ", sQuote("wb"), " is specified as ", dQuote(wb),
        ", whereas the argument ", sQuote("wa"),
        " is unspecified. Possible options for ", sQuote("wa"), " are: ",
        dQuote("cz"), ", ", dQuote("ds"), ", ", dQuote("bv"), " or ", dQuote("equal"), "."
      ),
      call. = FALSE
    )
  }
}
# ------------------------------------------------------------------------
