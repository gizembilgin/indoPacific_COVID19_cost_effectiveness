#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// Ensures that it is exported and avaliable as an R function
// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

// [[Rcpp::export]]
List DataFrameExample(const DataFrame & DF) {
  
  // access each column by name
  IntegerVector a = DF["a"];
  CharacterVector b = DF["b"];
  DateVector c = DF["c"];
  
  // do something
  a[2] = 42;
  b[1] = "foo";
  c[0] = c[0] + 7; // move up a week
  
  // create a new data frame
  DataFrame NDF = DataFrame::create(Named("a")=a,
                                    Named("b")=b,
                                    Named("c")=c);
  
  // and return old and new in list
  return List::create(Named("origDataFrame") = DF,
                      Named("newDataFrame") = NDF);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
