#' @title Compute the Quadratic Form and its Square Root
#' @description This function computes the quadratic form \( v^T Q v \) where \( v \) is a vector and \( Q \) is a matrix, then returns the square root of the result.
#' @param v A numeric vector of length \(m\).
#' @param Q A numeric matrix of size \(m * m\) (square matrix).
#' @return A numeric value representing the square root of the quadratic form \( v^T Q v \).
#' @examples
#' \dontrun{
#' # Example: Compute the square root of the quadratic form
#' v <- rnorm(5)  # Create a random vector
#' Q <- matrix(rnorm(25), nrow = 5)  # Create a random matrix
#' result <- f2(v, Q)
#' print(result)
#' }
#' @export
f2 <- function(v, Q) {
  result <- t(v) %*% Q %*% v
  return(sqrt(result))
}
