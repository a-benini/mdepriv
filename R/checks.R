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
