#################################################################################
##
## Author:  Nat Goodman
## Created: 20-10-26
##          from covid/R/util.R created 20-01-14
##          from misig/util.R created 19-01-01
##          from repwr/R/util.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## cq - create vector from unquoted strings
## cl - combine lists
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- create vector from unquoted strings ----

cq=function(...) {
 dots=match.call(expand.dots=FALSE)$...
 if (length(dots) &&
     !all(vapply(dots,function(x) is.atomic(x)||is.symbol(x)||is.character(x),
                 NA,USE.NAMES=FALSE))) 
   stop("... must contain atomic data like names or character strings");
 return(vapply(dots,as.character,""));
}

## ---- combine lists ----
## replace values in list by defaults. kinda like the way I set defaults in Perl
## code adapted from stackoverflow.com/questions/33004238, stackoverflow.com/questions/42207235
##   Thanks!
## default, actual both lists
fill_defaults=function(default,actual) {
  actual=actual[lengths(actual)!= 0];   # remove NULL elements from actual
  default[names(actual)]=actual;        # replace defaults elements by non-NULL actual
  default;
}
## combine lists, with elements in later lists overriding earlier
## can be used to replace or add elements in list. kinda like the way I set elements in Perl
## clc is same but preserves class of 1st argument
## cl=function(...) Reduce(function(...) cl_(...),list(...))
cl=function(...) {
  dots=list(...);
  names=names(dots);
  ## workaround R's handling of nchar applied to NULL
  if (is.null(names)) names=rep('',length(dots));
  dots=do.call(
    c,lapply(seq_along(dots),function(i)
      if(nchar(names[i])==0) {
        x=dots[i];
        if (!is.list(x[[1]])) x=list(x);
        x;
      } else {
        x=dots[i];
        names(x)=names[i];
        list(x);
      }));
  Reduce(function(...) cl_(...),dots);
}
## NG 20-08-06: handle un-named elements
## cl_=function(x,y) {
##   x=x[names(x) %-% names(y)];
##   c(x,y);
## }
cl_=function(x,y) {
  x[names(y)]=NULL;
  c(x,y);
}
clc=function(obj,...) {
  newobj=cl(obj,...);
  class(newobj)=class(obj);
  newobj;
}
