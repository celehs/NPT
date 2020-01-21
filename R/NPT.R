#' Non-Parametric Transformation Model
#'
#' Implements the simplex algorithm for fitting non-parametric transformation models. The computationally intensive part is written in C++.
#'
#' @docType package
#' @name NPT-package
#' @aliases NPT
#' @keywords package
#' @useDynLib NPT
#' @import Rcpp
NULL

#' Objective function minimization
#' 
#' @param coef initial parameter values
#' @param Z covariates matrix
#' @param y response vector
#' @param u a vector of weights
#' @param v a vector of weigths 
#' @param tree indicator of whether the tree algorithm should be used for fast ranking
#' @param nrep number of replications in the optimization algorithm
#' @param ftol function tolerance to determine convergence
#' @param nfun_max maximum number of function evaluations
#' 
#' @export
npt.fit <- function(coef, Z, y, u = NULL, v = NULL, tree = TRUE, 
                    nrep = 1, ftol = 1e-6, nfun_max = 5000) {
  n <- length(y)
  if (is.null(u)) u <- rep(1, n)
  if (is.null(v)) v <- rep(1, n)
  M <- obj_min(coef, Z, y, u, v, tree, nrep, ftol, nfun_max)
  list(min = M[1, 1], param = M[1, -1])
}
