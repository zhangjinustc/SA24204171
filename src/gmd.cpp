#include <Rcpp.h>

using namespace Rcpp;

//' @name f3
//' @title Generate Random Numbers from Uniform Distribution
//' @description This function generates a matrix of random numbers drawn from a uniform distribution between 0 and 100
//' @param num An integer specifying the number of random numbers to generate
//' @return A NumericMatrix of size \code{num x 1}, where each element is a random number between 0 and 100
//' @examples
//' \dontrun{
//' # Generate 5 random numbers between 0 and 100
//' random_numbers <- f3(5)
//' print(random_numbers)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix f3(int num) {
  NumericMatrix result(num, 1);
  for (int i = 0; i < num; i++) {
    result(i, 0) = R::runif(0, 100);
  }
  return result;
}
