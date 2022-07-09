
ES.prot.v4 <- function (data, n1, n2, f)
{
  if (!is.matrix(data)) {
    warning(gettextf("'%s' is not a matrix", as.character(match.call()$data),
                     domain = NA))
  }
  if (class(f) != "data.frame") {
    warning(gettextf("'%s' is not a dataframe", as.character(match.call()$f),
                     domain = NA))
  }
  if (ncol(data) != n1 + n2) {
    warning(gettextf("The number of columns in '%s' doesn't match the number of replicates ",
                     as.character(match.call()$data), domain = NA))
  }
  if (is.null(colnames(data))) {
    warning(gettextf("Columns of '%s' don't have names",
                     as.character(match.call()$data), domain = NA))
  }
  if (any(colnames(data) != rownames(f))) {
    warning(gettextf("The names of columns in '%1$s' doesn't match the names of rows in '%2$s' ",
                     as.character(match.call()$data), as.character(match.call()$f),
                     domain = NA))
  }
  n1 <- as.numeric(n1)
  n2 <- as.numeric(n2)
  c1 <- data[, 1:n1]
  c2 <- data[, (n1 + 1):(n1 + n2)]
  m.c1 <- apply(c1, 1, mean)
  m.c2 <- apply(c2, 1, mean)
  ratio <- m.c2 - m.c1
  r <- new("AnnotatedDataFrame", data = as.data.frame(ratio))
  f <- new("AnnotatedDataFrame", data = f)
  ES <- ExpressionSet(assayData = data, phenoData = f, featureData = r)
  return(ES)
}
