#################################################################################
##
## Author:  Nat Goodman
## Created: 20-10-30
##          from frecl/util.R created 20-01-14
##          from misig/util.R created 19-01-01
##          from repwr/R/util.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Smoothing functions
## Last used in misig, I think, so rather old...
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Smoothing functions ----
## wrapper for smooth methods
## NG 19-12-31: extend for 'x', 'y' both matrices
##   'x' must be vector or 2-dimensional matrix-like object with same number of columns as y'
##   'y' must be vector or 2-dimensional matrix-like object
## NG 20-01-02: replace special-case spar, span by method.args - additional args passed to method
##    note defaults for aspline, spline, loess
smooth=
  function(x,y,method,length=100,
           method.args=
             switch(method,
                    aspline=list(method='improved'),
                    spline=list(spar=0.5),
                    loess=list(span=0.75),
                    list())) {
    if (is.vector(x)) x=as.matrix(x)
    else if (length(dim(x))!=2) stop("'x' must be vector or 2-dimensional matrix-like object");
    if (is.vector(y)) y=as.matrix(y)
    else if (length(dim(y))!=2) stop("'y' must be vector or 2-dimensional matrix-like object");
    if (ncol(x)>1&&ncol(x)!=ncol(y))
      stop("When 'x' has multiple columns, it must have same number of columns as 'y'");
    if (method=='none') return(list(x=x,y=y));
  
    x.smooth=apply(x,2,function(x) seq(min(x),max(x),length=length));
    y.smooth=do.call(cbind,lapply(seq_len(ncol(y)),function(j)
      if (ncol(x)==1) smooth_(x[,1],y[,j],x.smooth,method,method.args)
      else smooth_(x[,j],y[,j],x.smooth[,j],method,method.args)));
    list(x=x.smooth,y=y.smooth);
}
smooth_=function(x,y,x.smooth,method,method.args) {
  method.args=c(list(x,y,x.smooth),method.args);
  method.fun=switch(method,
                    aspline=aspline_,spline=spline_,loess=loess_,linear=approx_,approx=approx_,
                    stop(paste('Invalid smoothing method:',method)));
  y.smooth=do.call(method.fun,method.args);
  as.matrix(y.smooth);
}
## these functions wrap underlying smoothing methods with consistent API
## akima::aspline
aspline_=function(x,y,x.smooth,...) {
  y.smooth=if(all(is.na(y))) rep(NA,length(x.smooth))
    else if (length(which(!is.na(y)))==1) rep(y[which(!is.na(y))],length(x.smooth))
    else akima::aspline(x,y,x.smooth,...)$y;
}
## loess
loess_=function(x,y,x.smooth,...) {
  data=data.frame(x,y);
  ## fmla=as.formula('y~x');
  y.smooth=suppressWarnings(predict(loess(y~x,data=data,...),data.frame(x=x.smooth)));
  y.smooth;
}
## smooth.spline
## NG 18-11-07: remove NAs (same as akima::aspline) else smooth.spline barfs
spline_=function(x,y,x.smooth,...) {
  ## remove NAs. code adapted from akima::aspline
  ## CAUTION: must use '<-' not '=' or place assignment in extra parens ((na=is.na(y)))
  ##   see stackoverflow.com/questions/1741820 for explanation. gotta love R...
  if (any(na<-is.na(y))) x=x[!na]; y=y[!na];
  y.smooth=predict(smooth.spline(x,y,...),x.smooth)$y    
  y.smooth;
}
## approx - probably only for completeness
approx_=function(x,y,x.smooth,...) {
  y.smooth=if (all(is.na(y))) rep(NA,length(x.smooth))
    else if (length(which(!is.na(y)))==1) rep(y[which(!is.na(y))],length(x.smooth))
    else approx(x,y,x.smooth,...)$y;
  y.smooth;
}
