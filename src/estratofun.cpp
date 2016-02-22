#include <Rcpp.h>
using namespace Rcpp;

// This is a simple function using Rcpp that creates an R list
// containing a character vector and a numeric vector.
//
// Learn more about how to use Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//
// and browse examples of code using Rcpp at:
//
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List estratofun(CharacterVector estrato)
{
  int i,j,n=estrato.size();
  CharacterVector estrato_v(n),divi(n);

  char elemento[6],cad[3],*temp;

  for(i=0;i<n;++i)
  {
    for(j=0;j<6;++j) elemento[j]=estrato(i)[j];
    strncpy(cad,elemento,2);
    cad[2]='\0';
    divi(i)=cad;
    if ((strncmp(elemento,"91X",3)==0) || (strncmp(elemento,"47A",3)==0)
          || (strncmp(elemento,"90X",3)==0) || (strncmp(elemento,"93X",3)==0))
      elemento[2]='0';
    temp=elemento+4;
    if ((strcmp(cad,"46")==0) || (strcmp(cad,"47")==0) || (strcmp(cad,"49")==0))
    {
      if ((strncmp(temp,"00",2)==0) || (strncmp(temp,"01",2)==0) || (strncmp(temp,"11",2)==0) ||
          (strncmp(temp,"12",2)==0) || (strncmp(temp,"13",2)==0) || (strncmp(temp,"23",2)==0))
        elemento[3]='1';
      else
        elemento[3]='3';
      elemento[4]='\0';
      estrato_v(i)=elemento;
    }
    else
    {
      if ((strncmp(temp,"00",2)==0) || (strncmp(temp,"01",2)==0) || (strncmp(temp,"11",2)==0))
        elemento[2]='1';
      else if ((strncmp(temp,"12",2)==0) || (strncmp(temp,"13",2)==0) || (strncmp(temp,"23",2)==0))
        elemento[2]='2';
      else
        elemento[2]='3';
      elemento[3]='\0';
      estrato_v(i)=elemento;
    }
  }

  return List::create(estrato_v,divi);

}
