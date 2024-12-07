#' @title Create an Auxiliary Matrix H
#' @description This function creates an auxiliary matrix \(H\) of size \(n * n\) with ones on the diagonal, and a constant value \(c\) on the diagonals just off the main diagonal (except for the first and last row/column).
#' @param c A numeric value that will fill the diagonals just off the main diagonal.
#' @param n An integer specifying the size of the square matrix \(H\) (the number of rows and columns).
#' @return A numeric matrix \(H\) of size \(n * n\), where the diagonal elements are 1, and the elements just off the main diagonal are set to the value \(c\).
#' @examples
#' \dontrun{
#' # Example: Create an auxiliary matrix H with n = 5 and c = 0.5
#' n <- 5
#' c <- 0.5
#' H <- auxiliary_matrix_H(c, n)
#' print(H)
#' }
#' @export
auxiliary_matrix_H <- function(c, n) {
  H <- matrix(0, n, n)
  diag(H) <- 1
  if (n > 1) {
    diag(H[-n, -1]) <- c
    diag(H[-1, -n]) <- c
  }

  return(H)
}
