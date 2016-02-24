#' Returns the nearest neighbour for each element by stratum
#'
#' \code{nn3fun} returns a list with the nearest neighbour, order of the donor use, and distance.
#'
#' Mahalanobis distance is used
#'
#' @param id Vector with elements identification
#'
#' @param stratum Vector with stratum identification
#'
#' @param values Vector with variables to compute distance with
#'
#' @param donor Class vector for receivers: 0, donors: 1, both: 2
#'
#' @param nvar Number of variables in vector values
#'
#' @return list as said
#'
#' @examples
#' nn3fun
#' @import MASS Rcpp
#' @useDynLib StQTFun
#'
#' @export
nn3fun<-function(id,stratum,values,donor,nvar) {

  # We create receivers matrix and vectors for unit and stratum identification
  matreceivers<-matrix(values[donor!=1],nrow=nvar)
  if(!ncol(matreceivers)) {
    id[1] <- NA # preserving class
    return(list(id[1],as.numeric(NA),as.numeric(NA)))
  }
  idreceivers<-id[donor!=1][seq.int(1,length(matreceivers),nvar)]
  strreceivers<-stratum[donor!=1][seq.int(1,length(matreceivers),nvar)]

  # Same for donors
  matdonors<-matrix(values[donor!=0],nrow=nvar)
  if(!ncol(matdonors)) {
    id[1] <- NA # preserving class
    return(list(id[1],as.numeric(NA),as.numeric(NA)))
  }
  iddonors<-id[donor!=0][seq.int(1,length(matdonors),nvar)]
  strdonors<-stratum[donor!=0][seq.int(1,length(matdonors),nvar)]
  
  
  
  # Computation of the inverse of the covariance matrix. It may be singular, so we use ginv
  cinv<-ginv(var(t(matrix(values,nrow=nvar)),use="na.or.complete"))

  iddistlist<-nn3aux(cinv,idreceivers,strreceivers,matreceivers,iddonors,strdonors,matdonors)

  orderedreceivers<-order(iddistlist[[1]],iddistlist[[2]])
  idchange<-c(FALSE,iddistlist[[1]][orderedreceivers][-1]==iddistlist[[1]][orderedreceivers][-length(orderedreceivers)])
  countneighbours<-numeric(length(idreceivers))
  countneighbours[orderedreceivers]<-CountGroup(idchange)

  output.id<-rep(NA,length=length(id))
  output.count<-rep(NA,length=length(id))
  output.dist<-rep(NA,length=length(id))
  output.id[donor!=1]<-rep(iddistlist[[1]],each=nvar)
  output.count[donor!=1]<-rep(countneighbours,each=nvar)
  output.dist[donor!=1]<-rep(iddistlist[[2]],each=nvar)
  return(list(output.id,output.count,output.dist))

}
