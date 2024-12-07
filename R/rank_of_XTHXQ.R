#' @title Compute the Rank of \( X^T H X Q \)
#' @description This function computes the rank of the matrix \( X^T H X Q \), where \( X \) is a matrix, \( H \) is an auxiliary matrix, and \( Q \) is another auxiliary matrix. The rank is computed using the `qr()` function, which performs a QR decomposition.
#' @param X A numeric matrix of size \(n * m\), where \(n\) is the number of rows and \(m\) is the number of columns.
#' @param H A numeric matrix of size \(n * n\), which is an auxiliary matrix used in the calculation.
#' @param Q A numeric matrix of size \(m * m\), which is another auxiliary matrix used in the calculation.
#' @return An integer representing the rank of the matrix \( X^T H X Q \).
#' @examples
#' \dontrun{
#' # Example: Compute the rank of X^T H X Q
#' X <- matrix(rnorm(100), nrow = 10)
#' H <- auxiliary_matrix_H(0.5, 10)
#' Q <- auxiliary_matrix_Q(10)
#' rank_result <- rank_of_XTHXQ(X, H, Q)
#' print(rank_result)
#' }
#' @export
rank_of_XTHXQ <- function(X, H, Q) {
  XTHXQ <- t(X) %*% H %*% X %*% Q
  rank_XTHXQ <- qr(XTHXQ)$rank
  return(rank_XTHXQ)
}
