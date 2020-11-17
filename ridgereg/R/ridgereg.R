#' Ridge regression
#'
#' \code{ridge} fits ridge regression by singular value decomposition.
#'
#' @param X        Covariates.
#' @param y        Binary response.
#' @param lambda   A sequence of tuning parameters.
#' @param stan     Whether to standardize data.
#' @return
#' An S3 class 'ridgereg' object with the following two components:
#'   betas A p by L matrix containing coefficients for each of L lambda values.
#'   b0    A p-vector containing intercepts for each of L lambda values.
#' @examples
# set.seed(5400)
# n = 100; p = 500
# b0 = 1; beta = rnorm(p)
# X = matrix(runif(n*p, 0, 2), n, p)
# y = b0 + X %*% beta + rnorm(n)
# lambda = 10^seq(5, -5, len=20) 
# system.time(fit <- RidgeReg(X, y, lambda))
#' @export
RidgeReg = function(X, y, lambda, stan=TRUE) {
  n = NROW(X)
  p = NCOL(X)
  if (length(y) != n) stop("dimensions are not matched")
  if (stan) {
    X = scale(X, center=TRUE, scale=TRUE)
    my = mean(y)
    y = y - my
  }
  o = svd(X, nu=n, nv=p)
  m = sum(o$d >= 0); d = o$d[seq(m)]
  M = matrix(NA, p, m)
  for (j in seq(m)) {
    M[,j] = as.numeric(o$u[,j] %*% y) * o$v[,j]
  }
  betas = matrix(NA, p, length(lambda))
  b0 = 0 * lambda
  for (l in seq(length(lambda))) {
    c_lam = d/(d^2 + lambda[l])
    betas[,l] = M %*% c_lam
    if (stan) {
      betas[,l] = betas[,l]/attr(X, "scaled:scale")
      b0[l] = my - c(betas[,l] %*% attr(X, "scaled:center"))
    } else {
      b0[l] = -2 * mean(y - X %*% betas[,l])
    }
  }
  ret = list(betas=betas, b0=b0)
  class(ret) = "ridgereg"  # S3 class
  ret
}