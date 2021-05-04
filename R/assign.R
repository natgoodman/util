#################################################################################
##
## Author:  Nat Goodman
## Created: 21-05-02
##          from param.R created 20-10-26
##          from covid/R/util.R created 20-01-14
##          from misig/util.R created 19-01-01
##          from repwr/R/util.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Assign local to global variables and vice versa
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Get and set global variables ----

## assign variables from parent to global
## if called without args, copies parent's local variables to global
## if called with args, copies parent's values for args to global
assign_global=function(...,list=character()) {
  dots=match.call(expand.dots=FALSE)$...
  parent.env=parent.frame(n=1);
  if (length(dots) &&
      !all(vapply(dots,function(x) is.atomic(x)||is.symbol(x)||is.character(x),
                  NA,USE.NAMES=FALSE))) 
    stop("... must contain atomic data like names or character strings");
  names=vapply(dots,as.character,"");
  if (length(names)==0L) names=character();
  names=c(list,names);
  if (!length(names)) names=ls(envir=parent.env);
  sapply(names,function(name) assign(name,get(name,envir=parent.env),envir=.GlobalEnv));
}
## assign variables from global to parent
assign_local=function(...,list=character()) {
  dots=match.call(expand.dots=FALSE)$...
  parent.env=parent.frame(n=1);
  if (length(dots) &&
      !all(vapply(dots,function(x) is.atomic(x)||is.symbol(x)||is.character(x),
                  NA,USE.NAMES=FALSE))) 
    stop("... must contain atomic data like names or character strings");
  names=vapply(dots,as.character,"");
  if (length(names)==0L) names=character();
  names=c(list,names);
  sapply(names,function(name) assign(name,get(name,envir=.GlobalEnv),envir=parent.env));
}
