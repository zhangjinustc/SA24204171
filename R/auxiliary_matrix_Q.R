#' @title Create an Auxiliary Matrix Q
#' @description This function creates an \(m * m\) identity matrix \(Q\) with ones on the diagonal and zeros elsewhere.
#' @param m An integer specifying the size of the square matrix \(Q\) (the number of rows and columns).
#' @return A numeric matrix \(Q\) of size \(m * m\), which is an identity matrix (with ones on the diagonal).
#' @examples
#' \dontrun{
#' # Example: Create an auxiliary identity matrix Q with m = 4
#' m <- 4
#' Q <- auxiliary_matrix_Q(m)
#' print(Q)
#' }
#' @export
auxiliary_matrix_Q <- function(m) {
  Q <- matrix(0, m, m)
  diag(Q) <- 1
  return(Q)
}
