RIplot.v4 <- function (data, n1, n2, ...)
{
  if (is.data.frame(data) & is.matrix(data)) {
    warning(gettextf("'%s' is neither a dataframe nor a matrix",
                     as.character(match.call()$data), domain = NA))
  }
  if (ncol(data) != n1 + n2) {
    warning(gettextf("The number of columns in '%s' doesn't match the number of replicates ",
                     as.character(match.call()$data), domain = NA))
  }
  c1.r <- data[, 1:n1]
  c2.r <- data[, (n1 + 1):(n1 + n2)]
  m.c1.r <- apply(c1.r, 1, mean)
  m.c2.r <- apply(c2.r, 1, mean)
  ratio.r <- log2(m.c2.r/m.c1.r)
  int.r <- log10(m.c1.r * m.c2.r)
  r.UP <- subset(data, ratio.r > 1)
  r.DO <- subset(data, ratio.r < -1)
  plot(ratio.r ~ int.r, pch = 20, col = grey(0.5), xlab = "Intensity", 
    ylab = "Ratio", ...)
  abline(h = c(-1, 0, 1))
  points(ratio.r[ratio.r > 1] ~ int.r[ratio.r > 1], pch = 20, 
    col = "#EE6A50")
  points(ratio.r[ratio.r < -1] ~ int.r[ratio.r < -1], pch = 20, 
    col = "#8EE5EE")
  text(x = max(int.r) - 1, y = 1.5, labels = dim(r.UP)[1])
  text(x = max(int.r) - 1, y = -1.5, labels = dim(r.DO)[1])
  res <- data.frame(ratio = ratio.r, intensity = int.r)
  invisible(list(RI = res, diff = c(dim(r.UP)[1], dim(r.DO)[1])))
}
