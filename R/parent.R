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
## Get values from parent function
## Meant to simplify deep call stacks
## Not currently used
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Get values from parent ----

## NG 20-10-19: rename and rewrite the 'parent' functions
##   'parent' now does one level dynamic lookup
##   'parpar' is 'parent' if exists else 'param'
##   'parall' does full dynamic lookup
## allow multi-argument versions, like 'param' that set values in caller
## let 'parent' set values in parent

## NG 19-06-27: commented out 'parent' is screwy
## NG 20-10-20: uncommented and renamed 'parentX' so I can test it
##   what it does is get value from parent frame. if not present,
##   searches from parent - this will be static
##   ----------
##   supposed to search dynamic environment tree but does static instead
##   seemed to work “back in the day” because params were global and static predecessor
##   of most functions is the global environment
##   ---------- 
## get value of variable from parent or set to default
## call with quoted or unquoted variable name
## if default missing, throws error if variable not found
parentX=function(what,default) {
  what=as.character(pryr::subs(what));
  if (exists(what,envir=parent.frame(n=2))) return(get(what,envir=parent.frame(n=2)));
  if (!missing(default)) return(default);
  stop(paste(sep='',"object '",what,"' not found in parent or lexical scope"));
}
## NG 20-10-20: this version does one level dynamic lookup
parent=function(what,default) {
  what=as.character(pryr::subs(what));
  if (exists(what,envir=parent.frame(n=2),inherits=FALSE))
    return(get(what,envir=parent.frame(n=2),inherits=FALSE));
  if (!missing(default)) return(default);
  stop(paste(sep='',"object '",what,"' not found in parent"));
}
## NG 20-10-20: this version does one level dynamic lookup then tries param then gets default
parpar=function(what,default) {
  what=as.character(pryr::subs(what));
  if (exists(what,envir=parent.frame(n=2),inherits=FALSE))
    return(get(what,envir=parent.frame(n=2),inherits=FALSE));
  if (exists(what,envir=param.env)) return(get(what,envir=param.env));
  if (!missing(default)) return(default);
  stop(paste(sep='',"object '",what,"' not found in parent or params"));
}
## NG 19-06-27, 20-10-19: this version does full dynamic lookup
## TODO: rename & extend per comments above
parall=function(what,default) {
  what=as.character(pryr::subs(what));
  n=2;
  repeat {
    env=parent.frame(n=n);
    if (n>100) stop(paste0('call stack too deep: n=',n));
    if (exists(what,envir=env,inherit=F)) return(get(what,envir=env));
    if (identical(env, globalenv())) break;
    n=n+1;
  }
  ## if fall out of loop, 'what' not found
  if (!missing(default)) return(default);
  stop(paste0("object '",what,"' not found in parent or any ancestor"));
}


## copy variable to parent
## NG 19-01-11: not used in effit. used once upon a time in dofig to update fignum
assign_parent=function(what,value) {
  what=as.character(pryr::subs(what));
  if (missing(value)) value=get(what,envir=parent.frame(n=1));
  assign(what,value,envir=parent.frame(n=2));
}

