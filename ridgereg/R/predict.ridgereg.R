#' Predicts reponse of ridge regression
#'
#' \code{predict.ridgereg} fits the response from a fitted 'ridgereg' object.
#'
#' @param object   A fitted 'ridgereg' object.
#' @param Xnew     A matrix of new values for x at which predictions are to be made.
#' @param ...      Not used. The dot argument that is passed on to predict.
#' @return
#' A vector of fitted responses.
#' @examples
# set.seed(5400)
# n = 100; p = 500
# b0 = 1; beta = rnorm(p)
# X = matrix(runif(n*p, 0, 2), n, p)
# y = b0 + X %*% beta + rnorm(n)
# lambda = 10^seq(5, -5, len=20) 
# predict(RidgeReg(X, y, lambda), X[1:10,])
#' @export
predict.ridgereg = function(object, Xnew, ...) {
  if (class(object) != "ridgereg") 
    stop("need a 'ridgereg' class")
  if (is.vector(Xnew)) Xnew = matrix(Xnew, 1)
  p = dim(object$betas)[1]
  if (dim(Xnew)[2] != p) stop("wrong dimension of Xnew")
  y = matrix(apply(object$betas, 2, 
    function(x) Xnew %*% x), nrow(Xnew))
  sweep(y, 2, object$b0, FUN="+") # add intercept
}
