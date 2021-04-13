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
