#' @title Center and Scale Response Variable
#' @description This function centers and scales the response variable by subtracting its mean and applying a scaling factor to the first and last elements.
#' @param Y A numeric vector representing the response variable.
#' @param c A numeric value that controls the scaling factor applied to the first and last elements of the response vector.
#' @return A numeric vector with the same length as \code{Y}, where the response variable has been centered and scaled.
#' @examples
#' \dontrun{
#' # Example: Center and scale a response variable
#' Y <- rnorm(10)
#' c <- 0.05
#' Y_centered <- response_variable(Y, c)
#' print(Y_centered)
#' }
#' @export
response_variable <- function(Y, c) {
  n <- length(Y)
  Y_centered <- Y - mean(Y)
  factor <- (1 + 2 * c) / (1 + c)
  Y_centered[1] <- Y_centered[1] * factor
  Y_centered[n] <- Y_centered[n] * factor
  return(Y_centered)
}
