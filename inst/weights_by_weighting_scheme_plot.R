# weight of a dichotomous item by mdepriv weighting scheme.
# figure for mdepriv help page.
# =================================================================================
ds <- function(x) {(1 - x) / 0.5}

cz <- function(x) {log(x) / log(0.5)}

bv <- function(x) {((1 - x) / x)^0.5}

par(mar = c(7, 4, 3, 1))

plot(0:1, c(0, 6.25),
  type = "n",
  xlab = "proportion of sample with item present",
  ylab = "weigths, re-scaled",
  main = "weight of a dichotomous item\nby mdepriv weighting scheme",
  axes = FALSE
)
axis(1, at = (0:4) / 4)
axis(2, las = 1)
box()
abline(h = 0:6, v = (0:4) / 4, col = "gray", lwd = 0.25)
segments(0.025, 1, x1 = 1, col = "blue", lwd = 2)
curve(ds, 0.025, 1, xname = "x", col = "magenta", add = TRUE, lwd = 2)
curve(cz, 0.025, 1, xname = "x", col = "chartreuse3", add = TRUE, lwd = 2)
curve(bv, 0.025, 1, xname = "x", col = "orange", add = TRUE, lwd = 2)
legend("topright", c("equi-proportionate", "Desai & Shah", "Cerioli & Zani", "Betti & Verma"),
  col = c("blue", "magenta", "chartreuse3", "orange"),
  lwd = 2
)
mtext("range shown  = [0.025, 1]; re-scaled such that f(0.5) = 1 for all four function.\nAll items negatively oriented, i.e. 0 = desirable state, 1 = deprived.",
  side = 1, line = 5.5, cex = 0.8
)
# ---------------------------------------------------------------------------------
# export / save manually as "mdepriv/man/figures/weightsbyweightingscheme.png".
# ---------------------------------------------------------------------------------
# end of script
# =================================================================================
