corr_mat_ <- function(data, items, corr_type, sampling_weights) {
  weightedCorr_automated <- function(x, y, corr_type, sampling_weights) {
    n_distinct_x <- length(unique(x))
    n_distinct_y <- length(unique(y))

    if (n_distinct_x < n_distinct_y) {
      x_temp <- x
      y_temp <- y
      x <- y_temp
      y <- x_temp
      n_distinct_x <- length(unique(x))
      n_distinct_y <- length(unique(y))
    }

    if (corr_type == "mixed") {
      if (n_distinct_x <= 10 & n_distinct_y <= 10) {
        corr_type <- "polychoric"
      } else if (n_distinct_x > 10 & n_distinct_y <= 10) {
        corr_type <- "polyserial"
      } else {
        corr_type <- "pearson"
      }
    }

    if (all(x == y)) {
      1
    } else if (n_distinct_x == n_distinct_y) {
      corr_xy <- wCorr::weightedCorr(x = x, y = y, method = corr_type, weights = sampling_weights)
      corr_yx <- wCorr::weightedCorr(x = y, y = x, method = corr_type, weights = sampling_weights)
      mean(c(corr_xy, corr_yx))
    } else {
      wCorr::weightedCorr(x = x, y = y, method = corr_type, weights = sampling_weights)
    }
  }

  corr_mat <- matrix(NA, length(items), length(items), dimnames = list(items, items))
  diag(corr_mat) <- 1
  for (i in items) {
    for (j in items) {
      if (is.na(corr_mat[i, j]) & is.na(corr_mat[j, i])) {
        corr_mat[i, j] <- weightedCorr_automated(x = data[[i]], y = data[[j]], corr_type = corr_type, sampling_weights = sampling_weights)
        corr_mat[j, i] <- corr_mat[i, j]
      }
    }
  }

  corr_mat
}
# ------------------------------------------------------------------------
corr_mat_type_ <- function(data, items, corr_type) {
  corr_type_ <- function(x, y, corr_type) {
    n_distinct_x <- length(unique(x))
    n_distinct_y <- length(unique(y))

    if (n_distinct_x < n_distinct_y) {
      x_temp <- x
      y_temp <- y
      x <- y_temp
      y <- x_temp
      rm(x_temp, y_temp)
      n_distinct_x <- length(unique(x))
      n_distinct_y <- length(unique(y))
    }

    if (corr_type == "mixed") {
      if (n_distinct_x <= 10 & n_distinct_y <= 10) {
        corr_type <- "polychoric"
      } else if (n_distinct_x > 10 & n_distinct_y <= 10) {
        corr_type <- "polyserial"
      } else {
        corr_type <- "pearson"
      }
    }

    corr_type
  }

  corr_mat_type <- matrix(NA, length(items), length(items), dimnames = list(items, items))
  for (i in items) {
    for (j in items) {
      if (is.na(corr_mat_type[i, j]) & is.na(corr_mat_type[j, i])) {
        corr_mat_type[i, j] <- corr_type_(x = data[[i]], y = data[[j]], corr_type = corr_type)
        corr_mat_type[j, i] <- corr_mat_type[i, j]
      }
    }
  }
  corr_mat_type
}
# ------------------------------------------------------------------------
wb_general_ <- function(data, items, corr_type, sampling_weights, rhoH) {
  if (corr_type != "diagonal" & length(items) > 1) {
    corr_mat__ <- corr_mat_(data = data, items = items, corr_type = corr_type, sampling_weights = sampling_weights)

    wb_j <- function(x, rhoH) {
      sum_l <- 1 + sum(x[x < rhoH])
      sum_h <- sum(x[x >= rhoH])
      1 / (sum_l * sum_h)
    }

    apply(corr_mat__, 2, wb_j, rhoH = rhoH)
  } else {
    rep(1, length(items))
  }
}
# ------------------------------------------------------------------------
