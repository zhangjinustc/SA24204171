#' @title Center and Scale Design Matrix
#' @description This function centers and scales the input design matrix, adjusting the first and last rows by a scaling factor.
#' @param X A numeric matrix representing the design matrix.
#' @param c A numeric value that controls the scaling factor applied to the first and last rows of the matrix.
#' @return A numeric matrix with the same dimensions as \code{X}, where the rows have been centered and scaled.
#' @examples
#' \dontrun{
#' # Example: Center and scale a design matrix
#' X <- matrix(rnorm(100), nrow = 10)
#' c <- 0.05
#' X_centered <- design_matrix(X, c)
#' print(X_centered)
#' }
#' @export
design_matrix <- function(X, c) {
  n <- nrow(X)
  m <- ncol(X)
  X_centered <- sweep(X, 2, colMeans(X), FUN = "-")
  factor <- (1 + 2 * c) / (1 + c)
  X_centered[1, ] <- X_centered[1, ] * factor
  X_centered[n, ] <- X_centered[n, ] * factor
  return(X_centered)
}

