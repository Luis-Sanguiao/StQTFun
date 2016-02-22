#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector CountGroup(LogicalVector v)
{
  int i,j,n=v.size();
  IntegerVector output(n);

  for (i=0,j=1;i<n;output(i++)=j++)
    if (!v(i)) j=1;
    else if (LogicalVector::is_na(v(i))) break;
  for(;i<n;output(i++)=NA_INTEGER);

  return(output);

}
