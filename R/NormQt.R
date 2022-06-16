#' Title
#'
#' @param data a dataframe of raw 2D Gel Volume data. data should be raw intensities displayed with gel as columns with the name of columns corresponding to the names of the gels and spots as rows with the names of the rows corresponding to the name of the spots. The replicates for each condition should be ordered in following columns.
#' @param n1 an integer. Number of replicates in condition 1.
#' @param n2 an integer. Number of replicates in condition 2.
#' @param plot logical. if TRUE (default) displaying two RIplot, one with the raw data, another with normalized data.
#'
#' @return
#' @export
#'
#' @examples
Norm.qt.v4 <- function(data, n1, n2, plot=T) {
  if (is.data.frame(data) & is.matrix(data)) {
    warning(gettextf("'%s' is neither a dataframe nor a matrix",
                     as.character(match.call()$data), domain = NA))
  }
  if (ncol(data) != n1 + n2) {
    warning(gettextf("The number of columns in '%s' doesn't match the number of replicates ",
                     as.character(match.call()$data), domain = NA))
  }
  mat <- as.matrix(data)
  mat.qt <- normalizeQuantiles(mat)
  if (plot) {
    par(mfrow = c(2, 1))
    RIplot.v4(data, n1 = n1, n2 = n2, main = "Raw data")
    RIplot.v4(mat.qt, n1 = n1, n2 = n2, main = "Normalized data")
    par(mfrow = c(1, 1))
  }
  norm <- log2(mat.qt)
  colnames(norm) <- colnames(data)
  rownames(norm) <- rownames(data)
  return(norm)
}
