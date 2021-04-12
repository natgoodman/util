#################################################################################
##
## Author:  Nat Goodman
## Created: 20-10-20
##          from frecl/util_freecell.R created circa 20-01-14
##          from misig/util.R created 19-01-01
##          from repwr/R/util.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Specialized versions of apply
## Not currently used but I don't want to lose code
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

## ---- Specialized versions of apply ----
## apply f to sub-matrices of e defined by groups
## TODO: port if useful
gapply=function(groups,e,f) {
  t(sapply(groups,function(g) {
    eg=e[g,];
    if (length(dim(eg))>=2) apply(eg,2,function(column) f(column))
    else sapply(eg,function(column) f(column));
  }))
}
## apply f to columns or rows of e.
## wrapper for apply that avoids simplifying single row results
capply=function(e,f,...,SIMPLIFY=FALSE,USE.NAMES=TRUE) {
  if (!is.null(dim(e))) {
    y=apply(e,2,f,...);
    if (is.null(dim(y))&&!SIMPLIFY) y=matrix(y,nrow=1);
    if (USE.NAMES&&length(y)>0) {
      colnames(y)=colnames(e);
      if (nrow(y)==nrow(e)) rownames(y)=rownames(e);
    }
    y;
  }
  else f(e,...);
}
rapply=function(e,f,...,SIMPLIFY=FALSE,USE.NAMES=TRUE) {
  if (!is.null(dim(e))) {
    y=apply(e,1,f,...);
    if (!is.null(dim(y))) y=t(y)
    else if (!SIMPLIFY&&length(y)>0) y=matrix(y,ncol=1);
    if (USE.NAMES&&length(y)>0) {
      rownames(y)=rownames(e);
      if (ncol(y)==ncol(e)) colnames(y)=colnames(e);
    }
    y;
  }
  else f(e,...);
}
## row and column functions
colMedians=function(e,...) capply(e,median,...)
rowMedians=function(e,...) rapply(e,median,...)

