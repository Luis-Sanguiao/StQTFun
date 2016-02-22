#include <Rcpp.h>
using namespace Rcpp;

//

// [[Rcpp::export]]
List nn3aux(NumericMatrix cinv, CharacterVector idreceivers,CharacterVector strreceivers, NumericMatrix matreceivers,
            CharacterVector iddonors,CharacterVector strdonors, NumericMatrix matdonors)
{
int i,j,k,l,jmin;
int nrec=matreceivers.ncol(),ndon=matdonors.ncol();
double distmin,dist;
CharacterVector neighbours(nrec);
NumericVector distances(nrec);
IntegerVector count(nrec);

  for (i=0;i<nrec;++i)
    {
    for(j=0,distmin=-1,jmin=-1;j<ndon;++j)
      if ((idreceivers(i)!=iddonors(j)) && (strreceivers(i)==strdonors(j)))
        {
        for(k=0,dist=0;k<cinv.nrow();++k)
          for(l=k;l<cinv.nrow();++l)
            if (k==l) dist+=cinv(k,l)*(matreceivers(k,i)-matdonors(k,j))*(matreceivers(k,i)-matdonors(k,j));
            else dist+=2*cinv(k,l)*(matreceivers(k,i)-matdonors(k,j))*(matreceivers(l,i)-matdonors(l,j));
        if ((distmin<0*dist) || (dist<distmin)) // isNAN check, -1<0*NAN is false!
          {
          distmin=dist;
          jmin=j;
          }
        }
    if (jmin!=-1)
      {
      neighbours(i)=iddonors(jmin);
      distances(i)=distmin;
      }
    else
      {
      neighbours(i)=NA_STRING;
      distances(i)=NA_REAL;
      }
    }

  return List::create(neighbours,distances);
}
