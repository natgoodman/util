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
## Apply code to rows of data frame, often a parameter grid
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Apply code to rows of data frame ----

## like 'with' but works on vectors. I use it inside apply(cases,1,function(case)...)
## note that plain 'with' works fine when applied to cases as a whole
## withcase=function(case,...) with(data.frame(t(case)),...)
withrows=function(cases,case,expr) {
  var=as.character(pryr::subs(case));
  expr=pryr::subs(expr);
  parent=parent.frame(n=1);
  lapply(1:nrow(cases),function(i) {
    case=cases[i,,drop=FALSE];
    ## assign case so it'll be data frame in called code
    env=list2env(case,parent=parent); # assign vars from case
    assign(var,case,envir=env);         # so case will be visible in called code
    eval(expr,envir=env);               # do it!
  })}
