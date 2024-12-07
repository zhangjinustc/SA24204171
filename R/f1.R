#' @title Compute the Quadratic Form and its Square Root
#' @description This function computes the quadratic form \( u^T H u \) where \( u \) is a vector and \( H \) is a matrix, then returns the square root of the result.
#' @param u A numeric vector of length \(n\).
#' @param H A numeric matrix of size \(n * n\) (square matrix).
#' @return A numeric value representing the square root of the quadratic form \( u^T H u \).
#' @examples
#' \dontrun{
#' # Example: Compute the square root of the quadratic form
#' u <- rnorm(5)  # Create a random vector
#' H <- matrix(rnorm(25), nrow = 5)  # Create a random matrix
#' result <- f1(u, H)
#' print(result)
#' }
#' @export
f1 <- function(u, H) {
  result <- t(u) %*% H %*% u
  return(sqrt(result))
}
